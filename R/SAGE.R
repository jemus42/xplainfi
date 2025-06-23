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

    #' @description
    #' Creates a new instance of the SAGE class.
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
    #' @param n_permutations (`integer(1)`) Number of permutations to sample for Shapley value estimation.
    #' @param reference_data (`data.table | NULL`) Optional reference dataset. If `NULL`, uses training data.
    #' @param sampler ([FeatureSampler]) Sampler for marginalization.
    #' @param max_reference_size (`integer(1) | NULL`) Maximum size of reference dataset. If reference is larger, it will be subsampled. If `NULL`, no subsampling is performed.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      sampler = NULL,
      max_reference_size = NULL
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
            "Classification learners must use predict_type = 'prob' for SAGE.",
            "i" = "Please set learner$predict_type = 'prob' before using SAGE."
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
        n_permutations = paradox::p_int(lower = 1L, default = 10L)
      )
      ps$values$n_permutations = n_permutations
      self$param_set = ps
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (logical(1)) Whether to store backends.
    #' @param batch_size (integer(1)) Maximum number of observations to process in a single prediction call. If NULL, processes all at once.
    compute = function(store_backends = TRUE, batch_size = NULL) {
      # Check if already computed
      if (!is.null(self$importance)) {
        return(self$importance)
      }

      # Initial resampling to get trained models
      rr = resample(
        self$task,
        self$learner,
        self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )

      # Compute SAGE values for each resampling iteration
      sage_scores = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_sage_scores(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
          batch_size = batch_size
        )
      })

      # Combine results across resampling iterations
      scores = rbindlist(sage_scores, idcol = "iter_rsmp")

      # Aggregate by feature
      scores_agg = private$.aggregate_importances(scores)

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      copy(self$importance)
    }
  ),

  private = list(
    .compute_sage_scores = function(learner, test_dt, batch_size = NULL) {
      # Initialize SAGE values for each feature
      sage_values = numeric(length(self$features))
      names(sage_values) = self$features

      # Pre-generate all permutations for efficiency
      all_permutations = replicate(self$n_permutations, sample(self$features), simplify = FALSE)

      # Pre-allocate lists for coalitions
      # Total coalitions = 1 (empty) + n_permutations * n_features
      n_total_coalitions = 1 + self$n_permutations * length(self$features)
      all_coalitions = vector("list", n_total_coalitions)
      coalition_map = vector("list", n_total_coalitions)

      # Add empty coalition
      all_coalitions[[1]] = character(0)
      coalition_map[[1]] = list(perm_idx = 0, step = 0) # Special case for baseline

      coalition_idx = 2
      for (perm_idx in seq_len(self$n_permutations)) {
        perm_features = all_permutations[[perm_idx]]

        for (i in seq_along(perm_features)) {
          coalition = perm_features[seq_len(i)]
          all_coalitions[[coalition_idx]] = coalition
          coalition_map[[coalition_idx]] = list(perm_idx = perm_idx, step = i)
          coalition_idx = coalition_idx + 1
        }
      }

      # Batch evaluate ALL coalitions at once
      all_losses = private$.evaluate_coalitions_batch(learner, test_dt, all_coalitions, batch_size)

      # Extract baseline loss
      baseline_loss = all_losses[1]

      # Process permutations using pre-computed losses
      for (perm_idx in seq_len(self$n_permutations)) {
        perm_features = all_permutations[[perm_idx]]
        prev_loss = baseline_loss

        for (i in seq_along(perm_features)) {
          feature = perm_features[i]

          # Find the loss for this coalition
          coalition_lookup_idx = which(sapply(coalition_map, function(x) {
            x$perm_idx == perm_idx && x$step == i
          }))
          current_loss = all_losses[coalition_lookup_idx]

          # Calculate marginal contribution
          marginal_contribution = prev_loss - current_loss
          sage_values[feature] = sage_values[feature] + marginal_contribution

          prev_loss = current_loss
        }
      }

      # Average over permutations
      sage_values = sage_values / self$n_permutations

      # Return as data.table
      data.table(
        feature = names(sage_values),
        importance = unname(sage_values)
      )
    },

    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
      # Batch evaluate multiple coalitions by creating datasets
      n_coalitions = length(all_coalitions)
      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      # Pre-allocate list for expanded data
      all_expanded_data = vector("list", n_coalitions)

      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]]

        # Create test-reference combinations for this coalition
        test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add coalition and test instance IDs for tracking
        test_expanded[, .coalition_id := i]
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

        # Marginalize features not in coalition
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
        "i" = "Use MarginalSAGE or ConditionalSAGE instead of the abstract SAGE class."
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
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      max_reference_size = NULL
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
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      sampler = NULL,
      max_reference_size = NULL
    ) {
      # Use ARFSampler by default
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
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

      # For each marginalized feature, update test_data by joining with sampled values
      for (feature_name in marginalize_features) {
        test_data[
          sampled_data,
          (feature_name) := get(paste0("i.", feature_name)),
          on = ".test_instance_id"
        ]
      }

      test_data
    }
  )
)
