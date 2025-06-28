#' @title Shapley Additive Global Importance (SAGE) Base Class
#'
#' @description Base class for SAGE (Shapley Additive Global Importance)
#' feature importance based on Shapley values with marginalization.
#' This is an abstract class - use MarginalSAGE or ConditionalSAGE.
#'
#' @details
#' SAGE uses Shapley values to fairly distribute the total prediction
#' performance among all features. Unlike perturbation-based methods,
#' SAGE marginalizes features by integrating over their distribution.
#' This is approximated by averaging predictions over a reference dataset.
#'
#' @references
#' `r print_bib("lundberg_2020")`
#'
#' @export
SAGE = R6Class(
  "SAGE",
  inherit = FeatureImportanceMethod,
  public = list(
    #' @field n_permutations (integer(1)) Number of permutations to sample.
    n_permutations = NULL,
    #' @field reference_data (data.table) Reference dataset for marginalization.
    reference_data = NULL,
    #' @field sampler ([FeatureSampler]) Sampler object for marginalization.
    sampler = NULL,
    #' @field convergence_history ([data.table]) History of SAGE values during computation.
    convergence_history = NULL,
    #' @field converged (`logical(1)`) Whether convergence was detected.
    converged = FALSE,
    #' @field n_permutations_used (`integer(1)`) Actual number of permutations used.
    n_permutations_used = NULL,

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
    #' @param n_permutations (`integer(1): 10L`) Number of permutations _per coalition_ to sample for Shapley value estimation.
    #'   The total number of evaluated coalitions is `1 (empty) + n_permutations * n_features`.
    #' @param reference_data (`data.table | NULL`) Optional reference dataset. If `NULL`, uses training data.
    #'   For each coalition to evaluate, an expanded datasets of size `n_test * n_reference` is created and evaluted in batches of `batch_size`.
    #' @param batch_size (`integer(1): 5000L`) Maximum number of observations to process in a single prediction call.
    #' @param sampler ([FeatureSampler]) Sampler for marginalization. Only relevant for `ConditionalSAGE`.
    #' @param max_reference_size (`integer(1): 100L`) Maximum size of reference dataset. If reference is larger, it will be subsampled.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      batch_size = 5000L,
      sampler = NULL,
      max_reference_size = 100L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        label = "Shapley Additive Global Importance"
      )

      self$n_permutations = checkmate::assert_int(n_permutations, lower = 1L)

      # For classification tasks, require predict_type = "prob"
      if (self$task$task_type == "classif") {
        if (learner$predict_type != "prob") {
          cli::cli_abort(c(
            "Classification learners require probability predictions for SAGE.",
            "i" = "Please set {.code learner$configure(predict_type = \"prob\")} before using SAGE."
          ))
        }
      }

      # Store sampler
      if (!is.null(sampler)) {
        self$sampler = sampler
      }

      # Use training data as reference if not provided
      if (is.null(reference_data)) {
        self$reference_data = self$task$data(cols = self$task$feature_names)
      } else {
        self$reference_data = checkmate::assert_data_table(reference_data)
      }

      # Subsample reference data if it's too large for efficiency
      if (!is.null(max_reference_size) && nrow(self$reference_data) > max_reference_size) {
        sample_idx = sample(nrow(self$reference_data), size = max_reference_size)
        self$reference_data = self$reference_data[sample_idx, ]
      }

      # Set parameters
      ps = ps(
        n_permutations = paradox::p_int(lower = 1L, default = 10L),
        batch_size = paradox::p_int(lower = 1L, default = 5000L),
        max_reference_size = paradox::p_int(lower = 1L, default = 100L),
        early_stopping = paradox::p_lgl(default = FALSE),
        convergence_threshold = paradox::p_dbl(lower = 0, upper = 1, default = 0.01),
        min_permutations = paradox::p_int(lower = 5L, default = 10L),
        check_interval = paradox::p_int(lower = 1L, default = 2L)
      )
      ps$values$n_permutations = n_permutations
      ps$values$batch_size = batch_size
      ps$values$max_reference_size = max_reference_size
      self$param_set = ps
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (logical(1)) Whether to store backends.
    #' @param batch_size (integer(1): 5000L) Maximum number of observations to process in a single prediction call.
    #' @param early_stopping (logical(1)) Whether to check for convergence and stop early.
    #' @param convergence_threshold (numeric(1)) Relative change threshold for convergence detection.
    #' @param min_permutations (integer(1)) Minimum permutations before checking convergence.
    #' @param check_interval (integer(1)) Check convergence every N permutations.
    compute = function(
      store_backends = TRUE,
      batch_size = NULL,
      early_stopping = NULL,
      convergence_threshold = NULL,
      min_permutations = NULL,
      check_interval = NULL
    ) {
      # Check if already computed
      if (!is.null(self$importance)) {
        return(self$importance)
      }

      # Reset convergence tracking
      self$convergence_history = NULL
      self$converged = FALSE
      self$n_permutations_used = NULL

      # Resolve parameters using hierarchical resolution
      batch_size = resolve_param(batch_size, self$param_set$values$batch_size, 5000L)
      early_stopping = resolve_param(
        early_stopping,
        self$param_set$values$early_stopping,
        FALSE
      )
      convergence_threshold = resolve_param(
        convergence_threshold,
        self$param_set$values$convergence_threshold,
        0.01
      )
      min_permutations = resolve_param(
        min_permutations,
        self$param_set$values$min_permutations,
        10L
      )
      check_interval = resolve_param(check_interval, self$param_set$values$check_interval, 5L)

      # Initial resampling to get trained learners
      rr = resample(
        self$task,
        self$learner,
        self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )

      # For convergence tracking, we'll use the first resampling iteration
      # (convergence is about permutation count, not resampling)
      iter_for_convergence = 1L

      # Compute SAGE values for convergence tracking (first iteration)
      first_result = private$.compute_sage_scores(
        learner = rr$learners[[iter_for_convergence]],
        test_dt = self$task$data(rows = rr$resampling$test_set(iter_for_convergence)),
        batch_size = batch_size,
        early_stopping = early_stopping,
        convergence_threshold = convergence_threshold,
        min_permutations = min_permutations,
        check_interval = check_interval
      )

      # Extract convergence data from first iteration
      if (!is.null(first_result$convergence_data)) {
        self$convergence_history = first_result$convergence_data$convergence_history
        self$converged = first_result$convergence_data$converged
        self$n_permutations_used = first_result$convergence_data$n_permutations_used
      }

      # If we have multiple resampling iterations, compute the rest without convergence tracking
      if (self$resampling$iters > 1) {
        remaining_results = lapply(seq_len(self$resampling$iters)[-iter_for_convergence], \(iter) {
          private$.compute_sage_scores(
            learner = rr$learners[[iter]],
            test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
            batch_size = batch_size,
            early_stopping = FALSE, # Only track convergence for first iteration
            convergence_threshold = convergence_threshold,
            min_permutations = min_permutations,
            check_interval = check_interval
          )
        })

        # Extract scores from all results (always list format now)
        all_scores = c(list(first_result$scores), lapply(remaining_results, function(x) x$scores))
      } else {
        all_scores = list(first_result$scores)
      }

      # Combine results across resampling iterations
      scores = rbindlist(all_scores, idcol = "iter_rsmp")

      # Aggregate by feature
      scores_agg = private$.aggregate_importances(scores)

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      copy(self$importance)
    },

    #' @description
    #' Plot convergence history of SAGE values.
    #' @param features (`character` | `NULL`) Features to plot. If NULL, plots all features.
    #' @return A ggplot2 object
    plot_convergence = function(features = NULL) {
      require_package("ggplot2")

      if (is.null(self$convergence_history)) {
        cli::cli_abort("No convergence history available. Run $compute() first.")
      }

      # Create a copy to avoid modifying the original
      plot_data = copy(self$convergence_history)

      if (!is.null(features)) {
        plot_data = plot_data[feature %in% features]
      }

      p = ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = n_permutations, y = importance, color = feature)
      ) +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(
          title = "SAGE Value Convergence",
          subtitle = if (self$converged) {
            sprintf(
              "Converged after %d permutations (saved %d)",
              self$n_permutations_used,
              self$n_permutations - self$n_permutations_used
            )
          } else {
            sprintf("Completed all %d permutations", self$n_permutations)
          },
          x = "Number of Permutations",
          y = "SAGE Value",
          color = "Feature"
        ) +
        ggplot2::theme_minimal(base_size = 12)

      if (self$converged) {
        p = p +
          ggplot2::geom_vline(
            xintercept = self$n_permutations_used,
            linetype = "dashed",
            color = "red",
            alpha = 0.5
          )
      }

      p
    }
  ),

  private = list(
    .compute_sage_scores = function(
      learner,
      test_dt,
      batch_size = NULL,
      early_stopping = FALSE,
      convergence_threshold = 0.01,
      min_permutations = 10L,
      check_interval = 5L
    ) {
      # Initialize SAGE values and tracking
      sage_values = numeric(length(self$features))
      names(sage_values) = self$features

      # Pre-generate ALL permutations upfront to ensure consistent random state
      all_permutations = replicate(self$n_permutations, sample(self$features), simplify = FALSE)

      # Always use iterative checkpoint-based computation
      # The only difference is whether we stop early based on convergence
      convergence_history = list()
      n_completed = 0
      converged = FALSE
      baseline_loss = NULL

      # Calculate total checkpoints for progress tracking
      total_checkpoints = ceiling(self$n_permutations / check_interval)
      current_checkpoint = 0

      # Start checkpoint-based progress bar
      if (xplain_opt("progress")) {
        cli::cli_progress_bar(
          "Computing SAGE values",
          total = total_checkpoints
        )
      }

      # Process permutations in checkpoints
      while (n_completed < self$n_permutations && !converged) {
        # Determine checkpoint size
        checkpoint_size = min(check_interval, self$n_permutations - n_completed)
        checkpoint_perms = (n_completed + 1):(n_completed + checkpoint_size)

        # Get permutations for this checkpoint from pre-generated list
        checkpoint_permutations = all_permutations[checkpoint_perms]

        # Build coalitions for this checkpoint
        checkpoint_coalitions = list()
        checkpoint_coalition_map = list()
        coalition_idx = 1

        # Add empty coalition only for first checkpoint
        if (n_completed == 0) {
          checkpoint_coalitions[[1]] = character(0)
          checkpoint_coalition_map[[1]] = list(checkpoint_perm_idx = 0, step = 0)
          coalition_idx = 2
        }

        # Add coalitions from permutations in this checkpoint
        for (i in seq_along(checkpoint_permutations)) {
          perm_features = checkpoint_permutations[[i]]

          for (j in seq_along(perm_features)) {
            coalition = perm_features[seq_len(j)]
            checkpoint_coalitions[[coalition_idx]] = coalition
            checkpoint_coalition_map[[coalition_idx]] = list(checkpoint_perm_idx = i, step = j)
            coalition_idx = coalition_idx + 1
          }
        }

        # Update progress: starting checkpoint
        current_checkpoint = current_checkpoint + 1
        n_coalitions_in_checkpoint = length(checkpoint_coalitions)

        # Evaluate coalitions for this checkpoint
        checkpoint_losses = private$.evaluate_coalitions_batch(
          learner,
          test_dt,
          checkpoint_coalitions,
          batch_size
        )

        # Update progress: checkpoint completed
        if (xplain_opt("progress")) {
          cli::cli_progress_update(inc = 1)
        }

        # Get baseline loss (from first checkpoint only)
        if (n_completed == 0) {
          baseline_loss = checkpoint_losses[1]
        }

        # Process checkpoint results
        for (i in seq_along(checkpoint_permutations)) {
          perm_features = checkpoint_permutations[[i]]
          prev_loss = baseline_loss

          for (j in seq_along(perm_features)) {
            feature = perm_features[j]

            # Find the loss for this coalition in checkpoint results
            coalition_lookup_idx = which(sapply(checkpoint_coalition_map, function(x) {
              x$checkpoint_perm_idx == i && x$step == j
            }))

            # Get the coalition loss directly (no adjustment needed)
            current_loss = checkpoint_losses[coalition_lookup_idx]

            # Calculate marginal contribution
            marginal_contribution = prev_loss - current_loss

            sage_values[feature] = sage_values[feature] + marginal_contribution

            prev_loss = current_loss
          }
        }

        n_completed = n_completed + checkpoint_size

        # Calculate current averages
        current_avg = sage_values / n_completed

        # Store convergence history (always, regardless of detect_convergence)
        checkpoint_history = data.table(
          n_permutations = n_completed,
          feature = names(current_avg),
          importance = as.numeric(current_avg) # Ensure numeric, not named vector
        )
        convergence_history[[length(convergence_history) + 1]] = checkpoint_history

        # Check convergence only if early stopping is enabled
        if (early_stopping && n_completed >= min_permutations && length(convergence_history) > 1) {
          # Get previous checkpoint values
          prev_checkpoint = convergence_history[[length(convergence_history) - 1]]
          curr_checkpoint = convergence_history[[length(convergence_history)]]

          # Merge by feature to ensure proper comparison (careful with data.table)
          prev_values = copy(prev_checkpoint)[order(feature)]$importance
          curr_values = copy(curr_checkpoint)[order(feature)]$importance

          # Calculate maximum relative change
          rel_changes = abs(curr_values - prev_values) / (abs(prev_values) + 1e-8)
          max_change = max(rel_changes, na.rm = TRUE)

          if (is.finite(max_change) && max_change < convergence_threshold) {
            converged = TRUE
            cli::cli_inform(c(
              "v" = "SAGE converged after {.val {n_completed}} permutations",
              "i" = "Maximum relative change: {.val {round(max_change, 4)}}",
              "i" = "Saved {.val {self$n_permutations - n_completed}} permutations"
            ))
          }
        }
      }

      # Close checkpoint progress bar
      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }

      # The convergence data will be set at the class level in compute()
      # Return as a list so we can access it from the main compute method
      convergence_data = list(
        convergence_history = if (length(convergence_history) > 0) {
          rbindlist(convergence_history)
        } else {
          NULL
        },
        converged = converged,
        n_permutations_used = n_completed
      )

      # Final averages
      final_sage_values = sage_values / n_completed

      # Return results with convergence data
      list(
        scores = data.table(
          feature = names(final_sage_values),
          importance = as.numeric(final_sage_values)
        ),
        convergence_data = convergence_data
      )
    },

    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
      # Batch evaluate multiple coalitions by creating datasets
      n_coalitions = length(all_coalitions)
      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      # Pre-allocate list for expanded data
      all_expanded_data = vector("list", n_coalitions)

      if (xplain_opt("debug")) {
        cli::cli_inform("Evaluating {.val {length(all_coalitions)}} coalitions")
      }

      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]]

        # Create test-reference combinations for this coalition
        test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add coalition and test instance IDs for tracking
        test_expanded[, .coalition_id := i]
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

        # Marginalize features not in coalition
        # Mild misnomer since ConditionalSAGE uses conditional sampler here
        marginalize_features = setdiff(self$features, coalition)
        if (length(marginalize_features) > 0) {
          test_expanded = private$.marginalize_features(
            test_expanded,
            reference_expanded,
            marginalize_features
          )
        }

        all_expanded_data[[i]] = test_expanded
      }

      # Combine ALL data into one big dataset
      combined_data = rbindlist(all_expanded_data)
      total_rows = nrow(combined_data)

      # Process data in batches if batch_size is specified and total rows exceed batch_size
      if (!is.null(batch_size) && total_rows > batch_size) {
        # Split into batches
        n_batches = ceiling(total_rows / batch_size)
        all_predictions = vector("list", n_batches)

        for (batch_idx in seq_len(n_batches)) {
          start_row = (batch_idx - 1) * batch_size + 1
          end_row = min(batch_idx * batch_size, total_rows)

          batch_data = combined_data[start_row:end_row]

          # Predict for this batch
          if (xplain_opt("debug")) {
            cli::cli_inform(
              "Predicting on {.val {nrow(batch_data)}} instances in batch {.val {batch_idx}/{n_batches}}"
            )
          }
          pred_result = learner$predict_newdata(newdata = batch_data, task = self$task)

          # Store predictions
          if (self$task$task_type == "classif") {
            all_predictions[[batch_idx]] = pred_result$prob
          } else {
            all_predictions[[batch_idx]] = pred_result$response
          }
        }

        # Combine predictions from all batches
        if (self$task$task_type == "classif") {
          combined_predictions = do.call(rbind, all_predictions)
        } else {
          combined_predictions = do.call(c, all_predictions)
        }
      } else {
        # Process all at once (original behavior)
        if (xplain_opt("debug")) {
          cli::cli_inform("Predicting on {.val {nrow(combined_data)}} instances at once")
        }

        pred_result = learner$predict_newdata(newdata = combined_data, task = self$task)

        if (self$task$task_type == "classif") {
          combined_predictions = pred_result$prob
        } else {
          combined_predictions = pred_result$response
        }
      }

      # Handle any NAs in predictions by replacing with default values
      if (self$task$task_type == "classif") {
        # Handle any NAs in probability predictions by replacing with uniform probabilities
        if (any(is.na(combined_predictions))) {
          n_classes = ncol(combined_predictions)
          uniform_prob = 1 / n_classes
          for (j in seq_len(n_classes)) {
            combined_predictions[is.na(combined_predictions[, j]), j] = uniform_prob
          }
        }
      } else {
        # Handle NAs in regression predictions
        combined_predictions[is.na(combined_predictions)] = 0
      }

      # Add predictions and aggregate by coalition and test instance
      if (self$task$task_type == "classif") {
        # For classification, we need to handle matrix predictions differently
        # Add each class probability as a separate column
        n_classes = ncol(combined_predictions)
        class_names = colnames(combined_predictions)

        for (j in seq_len(n_classes)) {
          combined_data[, paste0(".pred_class_", j) := combined_predictions[, j]]
        }

        # Aggregate: mean probability per class per test instance per coalition
        agg_cols = paste0(".pred_class_", seq_len(n_classes))
        avg_preds_by_coalition = combined_data[,
          lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          .SDcols = agg_cols,
          by = .(.coalition_id, .test_instance_id)
        ]

        # Rename columns to class names
        setnames(avg_preds_by_coalition, agg_cols, class_names)
      } else {
        # For regression, keep the simple approach
        combined_data[, .prediction := combined_predictions]

        # Aggregate: mean prediction per test instance per coalition
        avg_preds_by_coalition = combined_data[,
          .(
            avg_pred = mean(.prediction, na.rm = TRUE)
          ),
          by = .(.coalition_id, .test_instance_id)
        ]
      }

      # Calculate loss for each coalition
      coalition_losses = numeric(n_coalitions)
      for (i in seq_len(n_coalitions)) {
        coalition_data = avg_preds_by_coalition[.coalition_id == i]

        # Create prediction object and calculate loss
        if (self$task$task_type == "classif") {
          # For classification, extract the probability matrix
          class_names = self$task$class_names
          prob_matrix = as.matrix(coalition_data[, .SD, .SDcols = class_names])

          pred_obj = PredictionClassif$new(
            row_ids = seq_len(n_test),
            truth = test_dt[[self$task$target_names]],
            prob = prob_matrix
          )
        } else {
          pred_obj = PredictionRegr$new(
            row_ids = seq_len(n_test),
            truth = test_dt[[self$task$target_names]],
            response = coalition_data$avg_pred
          )
        }

        coalition_losses[i] = pred_obj$score(self$measure)
      }

      coalition_losses
    },

    .marginalize_features = function(test_data, reference_data, marginalize_features) {
      # Abstract method - must be implemented by subclasses
      cli::cli_abort(c(
        "Abstract method: {.fun marginalize_features} must be implemented by subclasses.",
        "i" = "Use {.cls MarginalSAGE} or {.cls ConditionalSAGE} instead of the abstract {.cls SAGE} class."
      ))
    }
  )
)

#' @title Marginal SAGE
#'
#' @description SAGE with marginal sampling (features are marginalized independently).
#' This is the standard SAGE implementation.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = MarginalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L
#' )
#' sage$compute()
#'
#' # Use batching for memory efficiency with large datasets
#' sage$compute(batch_size = 1000)
#' @export
MarginalSAGE = R6Class(
  "MarginalSAGE",
  inherit = SAGE,
  public = list(
    #' @description
    #' Creates a new instance of the MarginalSAGE class.
    #' @param task,learner,measure,resampling,features Passed to [SAGE].
    #' @param n_permutations (integer(1)) Number of permutations to sample.
    #' @param reference_data (data.table) Optional reference dataset.
    #' @param max_reference_size (integer(1)) Maximum size of reference dataset.
    #' @param batch_size (`integer(1): 5000L`) Maximum number of observations to process in a single prediction call.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      batch_size = 5000L,
      max_reference_size = 100L
    ) {
      # No need to initialize sampler as marginal sampling is done differently here
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        n_permutations = n_permutations,
        reference_data = reference_data,
        batch_size = batch_size,
        max_reference_size = max_reference_size
      )

      self$label = "Marginal SAGE"
    }
  ),

  private = list(
    .marginalize_features = function(test_data, reference_data, marginalize_features) {
      # Marginal sampling implementation: replace with reference data
      test_data[,
        (marginalize_features) := reference_data[, .SD, .SDcols = marginalize_features]
      ]
      test_data
    }
  )
)

#' @title Conditional SAGE
#'
#' @description SAGE with conditional sampling (features are marginalized conditionally).
#' Uses ARF by default for conditional marginalization.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sage = ConditionalSAGE$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   n_permutations = 3L
#' )
#' sage$compute()
#'
#' # Use batching for memory efficiency with large datasets
#' sage$compute(batch_size = 1000)
#' @export
ConditionalSAGE = R6Class(
  "ConditionalSAGE",
  inherit = SAGE,
  public = list(
    #' @description
    #' Creates a new instance of the ConditionalSAGE class.
    #' @param task,learner,measure,resampling,features Passed to [SAGE].
    #' @param n_permutations (integer(1)) Number of permutations to sample.
    #' @param reference_data (data.table) Optional reference dataset.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to ARFSampler.
    #' @param max_reference_size (integer(1)) Maximum size of reference dataset.
    #' @param batch_size (`integer(1): 5000L`) Maximum number of observations to process in a single prediction call.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      sampler = NULL,
      batch_size = 5000L,
      max_reference_size = 100L
    ) {
      # Use ARFSampler by default
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
        cli::cli_alert_info(
          "No {.cls ConditionalSampler} provided, using {.cls ARFSampler} with default settings."
        )
      } else {
        checkmate::assert_class(sampler, "ConditionalSampler")
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        n_permutations = n_permutations,
        reference_data = reference_data,
        sampler = sampler,
        batch_size = batch_size,
        max_reference_size = max_reference_size
      )

      self$label = "Conditional SAGE"
    }
  ),

  private = list(
    .marginalize_features = function(test_data, reference_data, marginalize_features) {
      # Conditional implementation: use sampler for conditional sampling
      # Extract unique test instances (before expansion with reference data)
      unique_test_data = test_data[, .(
        .test_instance_id = .test_instance_id,
        row_idx = .I
      )][, .SD[1], by = .test_instance_id]

      # Get the actual test data for these instances
      unique_instances = test_data[unique_test_data$row_idx]
      unique_instances[, c(".coalition_id", ".test_instance_id") := NULL]

      # Determine conditioning set (features in coalition = all features except marginalize_features)
      conditioning_set = setdiff(self$features, marginalize_features)

      # Use sampler to generate conditional samples
      sampled_data = self$sampler$sample(
        feature = marginalize_features,
        data = unique_instances,
        conditioning_set = conditioning_set
      )

      # Replace marginalized features in the expanded test data
      # Since test_data has multiple rows per test instance (one per reference instance),
      # we need to replicate the sampled values appropriately

      # Create a mapping from test instance ID to sampled values
      sampled_data[, .test_instance_id := unique_test_data$.test_instance_id]

      # Replace marginalized features with sampled values
      # Update all features at once using proper data.table join syntax
      test_data[
        sampled_data,
        (marginalize_features) := mget(paste0("i.", marginalize_features)),
        on = ".test_instance_id"
      ]

      test_data
    }
  )
)
