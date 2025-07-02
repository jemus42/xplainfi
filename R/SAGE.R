#' @title Shapley Additive Global Importance (SAGE) Base Class
#'
#' @description Base class for SAGE (Shapley Additive Global Importance)
#' feature importance based on Shapley values with marginalization.
#' This is an abstract class - use [MarginalSAGE] or [ConditionalSAGE].
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
#' @seealso [MarginalSAGE] [ConditionalSAGE]
#'
#' @export
SAGE = R6Class(
  "SAGE",
  inherit = FeatureImportanceMethod,
  public = list(
    #' @field n_permutations (`integer(1)`) Number of permutations to sample.
    n_permutations = NULL,
    #' @field reference_data ([`data.table`][data.table::data.table]) Reference dataset for marginalization.
    reference_data = NULL,
    #' @field sampler ([FeatureSampler]) Sampler object for marginalization.
    sampler = NULL,
    #' @field convergence_history ([`data.table`][data.table::data.table]) History of SAGE values during computation.
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
    #' @param reference_data ([`data.table`][data.table::data.table] | `NULL`) Optional reference dataset. If `NULL`, uses training data.
    #'   For each coalition to evaluate, an expanded datasets of size `n_test * n_reference` is created and evaluted in batches of `batch_size`.
    #' @param batch_size (`integer(1): 5000L`) Maximum number of observations to process in a single prediction call.
    #' @param sampler ([FeatureSampler]) Sampler for marginalization. Only relevant for `ConditionalSAGE`.
    #' @param max_reference_size (`integer(1): 100L`) Maximum size of reference dataset. If reference is larger, it will be subsampled.
    #' @param early_stopping (`logical(1): FALSE`) Whether to enable early stopping based on convergence detection.
    #' @param convergence_threshold (`numeric(1): 0.01`) Relative change threshold for convergence detection.
    #' @param se_threshold (`numeric(1): Inf`) Standard error threshold for convergence detection.
    #' @param min_permutations (`integer(1): 10L`) Minimum permutations before checking convergence.
    #' @param check_interval (`integer(1): 2L`) Check convergence every N permutations.
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
      max_reference_size = 100L,
      early_stopping = FALSE,
      convergence_threshold = 0.01,
      se_threshold = Inf,
      min_permutations = 10L,
      check_interval = 2L
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
        se_threshold = paradox::p_dbl(lower = 0, default = Inf),
        min_permutations = paradox::p_int(lower = 5L, default = 10L),
        check_interval = paradox::p_int(lower = 1L, default = 2L)
      )
      ps$values$n_permutations = n_permutations
      ps$values$batch_size = batch_size
      ps$values$max_reference_size = max_reference_size
      ps$values$early_stopping = early_stopping
      ps$values$convergence_threshold = convergence_threshold
      ps$values$se_threshold = se_threshold
      ps$values$min_permutations = min_permutations
      ps$values$check_interval = check_interval
      self$param_set = ps
    },

    #' @description
    #' Compute SAGE values.
    #' @param store_backends (`logical(1)`) Whether to store backends.
    #' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
    #' @param early_stopping (`logical(1)`) Whether to check for convergence and stop early.
    #' @param convergence_threshold (`numeric(1)`) Relative change threshold for convergence detection.
    #' @param se_threshold (`numeric(1)`) Standard error threshold for convergence detection.
    #' @param min_permutations (`integer(1)`) Minimum permutations before checking convergence.
    #' @param check_interval (`integer(1)`) Check convergence every N permutations.
    compute = function(
      store_backends = TRUE,
      batch_size = NULL,
      early_stopping = NULL,
      convergence_threshold = NULL,
      se_threshold = NULL,
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
      se_threshold = resolve_param(
        se_threshold,
        self$param_set$values$se_threshold,
        Inf
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
        se_threshold = se_threshold,
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
            se_threshold = se_threshold,
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
    #' @return A [ggplot2][ggplot2::ggplot] object
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
        ggplot2::aes(x = n_permutations, y = importance, fill = feature, color = feature)
      ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = importance - se, ymax = importance + se),
          alpha = 1 / 3
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
          color = "Feature",
          fill = "Feature"
        ) +
        ggplot2::theme_minimal(base_size = 14)

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
      se_threshold = Inf,
      min_permutations = 10L,
      check_interval = 5L
    ) {
      # This function computes the SAGE values for a single resampling iteration.
      # It iterates through permutations of features, evaluates coalitions, and calculates marginal contributions.

      # Initialize numeric vectors to store marginal contributions and their squares for variance calculation.
      # We track both sum and sum of squares to calculate running variance and standard errors.
      sage_values = numeric(length(self$features)) # Sum of marginal contributions
      sage_values_sq = numeric(length(self$features)) # Sum of squared marginal contributions
      names(sage_values) = self$features
      names(sage_values_sq) = self$features

      # Pre-generate ALL permutations upfront to ensure consistent random state.
      # Relevant for reproducibility, especially when using early stopping or parallel processing.
      # Example: if self$features = c("x1", "x2", "x3") and self$n_permutations = 2,
      # all_permutations might be list(c("x2", "x1", "x3"), c("x3", "x1", "x2"))
      all_permutations = replicate(self$n_permutations, sample(self$features), simplify = FALSE)

      # Initialize variables for iterative checkpoint-based computation.
      # This allows for early stopping based on convergence and provides progress updates.
      convergence_history = list() # Stores SAGE values at each checkpoint for convergence tracking
      n_completed = 0 # Number of permutations processed so far
      converged = FALSE # Flag to indicate if convergence has been detected
      baseline_loss = NULL # Loss of the empty coalition (model with no features / all features marginalized)

      # Calculate total checkpoints for progress tracking.
      # A checkpoint is a group of 'check_interval' permutations.
      total_checkpoints = ceiling(self$n_permutations / check_interval)
      current_checkpoint = 0

      # Start checkpoint-based progress bar if progress display is enabled.
      if (xplain_opt("progress")) {
        cli::cli_progress_bar(
          "Computing SAGE values",
          total = total_checkpoints
        )
      }

      # Main loop: Process permutations in checkpoints until all permutations are done or convergence is reached.
      while (n_completed < self$n_permutations && !converged) {
        # Determine the size of the current checkpoint.
        # This ensures that the last checkpoint processes only the remaining permutations.
        checkpoint_size = min(check_interval, self$n_permutations - n_completed)
        # Define the indices of permutations to be processed in this checkpoint.
        checkpoint_perms = (n_completed + 1):(n_completed + checkpoint_size)

        # Get the actual permutation sequences for this checkpoint from the pre-generated list.
        checkpoint_permutations = all_permutations[checkpoint_perms]

        # Prepare lists to store unique coalitions and their mapping back to permutations.
        # This avoids redundant evaluations of the same coalition across different permutations/steps.
        checkpoint_coalitions = list() # Stores unique feature coalitions (e.g., c("x1", "x2"))
        checkpoint_coalition_map = list() # Maps coalition index to its permutation and step (e.g., list(checkpoint_perm_idx = 1, step = 2))
        coalition_idx = 1 # Counter for unique coalitions

        # Add the empty coalition ({}) only for the very first checkpoint.
        # The loss of the empty coalition serves as the baseline for marginal contributions.
        if (n_completed == 0) {
          checkpoint_coalitions[[1]] = character(0) # Represents the empty set of features
          checkpoint_coalition_map[[1]] = list(checkpoint_perm_idx = 0, step = 0) # Special mapping for baseline
          coalition_idx = 2 # Start next coalition index from 2
        }

        # Iterate through each permutation in the current checkpoint to build all necessary coalitions.
        # For each permutation (e.g., P = (x2, x1, x3)), we generate coalitions:
        # {}, {x2}, {x2, x1}, {x2, x1, x3}
        for (i in seq_along(checkpoint_permutations)) {
          perm_features = checkpoint_permutations[[i]] # Current permutation (e.g., c("x2", "x1", "x3"))

          # Build growing coalitions for the current permutation.
          for (j in seq_along(perm_features)) {
            # 'coalition' is the set of features considered up to the current step 'j'.
            # Example: for P=(x2, x1, x3):
            # j=1: coalition = c("x2")
            # j=2: coalition = c("x2", "x1")
            # j=3: coalition = c("x2", "x1", "x3")
            coalition = perm_features[seq_len(j)]

            # Store the coalition and its mapping. This allows us to retrieve the loss
            # for this specific coalition later from the batch evaluation results.
            checkpoint_coalitions[[coalition_idx]] = coalition
            checkpoint_coalition_map[[coalition_idx]] = list(checkpoint_perm_idx = i, step = j)
            coalition_idx = coalition_idx + 1
          }
        }

        # Update progress: indicate that a new checkpoint is starting.
        current_checkpoint = current_checkpoint + 1
        n_coalitions_in_checkpoint = length(checkpoint_coalitions) # Total unique coalitions to evaluate in this batch

        # Evaluate all unique coalitions collected in this checkpoint in a single batch.
        # This is a performance optimization to minimize prediction calls to the learner.
        checkpoint_losses = private$.evaluate_coalitions_batch(
          learner,
          test_dt,
          checkpoint_coalitions,
          batch_size
        )

        # Update progress bar.
        if (xplain_opt("progress")) {
          cli::cli_progress_update(inc = 1)
        }

        # Store the baseline loss (loss of the empty coalition) from the first checkpoint.
        # This is the model's performance when no features are available.
        if (n_completed == 0) {
          baseline_loss = checkpoint_losses[1] # The first element is always the empty coalition's loss
        }

        # Process the results from the current checkpoint to calculate marginal contributions.
        for (i in seq_along(checkpoint_permutations)) {
          perm_features = checkpoint_permutations[[i]] # Current permutation (e.g., c("x2", "x1", "x3"))
          prev_loss = baseline_loss # Start with baseline loss for the first feature in permutation

          # Iterate through features in the current permutation to calculate their marginal contributions.
          for (j in seq_along(perm_features)) {
            feature = perm_features[j] # The feature being added at this step (e.g., "x2", then "x1", then "x3")

            # Find the index of the current coalition's loss in the 'checkpoint_losses' vector.
            # This uses the 'checkpoint_coalition_map' to link back to the batched results.
            coalition_lookup_idx = which(sapply(checkpoint_coalition_map, function(x) {
              x$checkpoint_perm_idx == i && x$step == j
            }))

            # Get the loss for the current coalition (e.g., loss({x2}), then loss({x2, x1}), etc.).
            current_loss = checkpoint_losses[coalition_lookup_idx]

            # Calculate the marginal contribution of the 'feature' just added.
            # Contribution = (Loss without feature) - (Loss with feature)
            # A smaller loss is better, so a positive contribution means the feature improved performance.
            marginal_contribution = prev_loss - current_loss

            # Add this marginal contribution to the total SAGE value for the 'feature'.
            # Also track the squared contribution for variance calculation.
            sage_values[feature] = sage_values[feature] + marginal_contribution
            sage_values_sq[feature] = sage_values_sq[feature] + marginal_contribution^2

            # Update 'prev_loss' for the next iteration in this permutation.
            # The current coalition's loss becomes the 'previous' loss for the next feature's contribution.
            prev_loss = current_loss
          }
        }

        # Update the count of completed permutations.
        n_completed = n_completed + checkpoint_size

        # Calculate the current average SAGE values and standard errors based on completed permutations.
        current_avg = sage_values / n_completed

        # Calculate running variance and standard errors for each feature
        # Variance = E[X^2] - E[X]^2, SE = sqrt(Var / n)
        current_variance = (sage_values_sq / n_completed) - (current_avg^2)
        # Ensure variance is non-negative (numerical precision issues)
        current_variance[current_variance < 0] = 0
        current_se = sqrt(current_variance / n_completed)

        # Store the current average SAGE values and standard errors in the convergence history.
        # Used for plotting, early stopping, and uncertainty quantification.
        checkpoint_history = data.table(
          n_permutations = n_completed,
          feature = names(current_avg),
          importance = as.numeric(current_avg),
          se = as.numeric(current_se)
        )
        convergence_history[[length(convergence_history) + 1]] = checkpoint_history

        # Check for convergence if early stopping is enabled and enough permutations have been processed.
        if (early_stopping && n_completed >= min_permutations && length(convergence_history) > 1) {
          # Get SAGE values from the previous and current checkpoints.
          prev_checkpoint = convergence_history[[length(convergence_history) - 1]]
          curr_checkpoint = convergence_history[[length(convergence_history)]]

          # Ensure features are in the same order for comparison.
          prev_values = copy(prev_checkpoint)[order(feature)]$importance
          curr_values = copy(curr_checkpoint)[order(feature)]$importance
          curr_se_values = copy(curr_checkpoint)[order(feature)]$se

          # Calculate the maximum relative change between current and previous SAGE values.
          # A small max_change indicates convergence.
          rel_changes = abs(curr_values - prev_values) / (abs(prev_values) + 1e-8) # Add epsilon to avoid division by zero
          max_change = max(rel_changes, na.rm = TRUE)

          # Calculate maximum standard error across features.
          max_se = max(curr_se_values, na.rm = TRUE)

          # SE threshold is already resolved as a parameter to this function

          # Check both relative change and standard error convergence criteria.
          rel_change_converged = is.finite(max_change) && max_change < convergence_threshold
          se_converged = is.finite(max_se) && max_se < se_threshold

          # Convergence requires both criteria to be met (when SE threshold is finite)
          if (is.finite(se_threshold)) {
            converged = rel_change_converged && se_converged
            convergence_msg = c(
              "v" = "SAGE converged after {.val {n_completed}} permutations",
              "i" = "Maximum relative change: {.val {round(max_change, 4)}} (threshold: {.val {convergence_threshold}})",
              "i" = "Maximum standard error: {.val {round(max_se, 4)}} (threshold: {.val {se_threshold}})",
              "i" = "Saved {.val {self$n_permutations - n_completed}} permutations"
            )
          } else {
            # If SE threshold is infinite, only check relative change
            converged = rel_change_converged
            convergence_msg = c(
              "v" = "SAGE converged after {.val {n_completed}} permutations",
              "i" = "Maximum relative change: {.val {round(max_change, 4)}}",
              "i" = "Saved {.val {self$n_permutations - n_completed}} permutations"
            )
          }

          if (converged) {
            cli::cli_inform(convergence_msg)
          }
        }
      }

      # Close the progress bar.
      if (xplain_opt("progress")) {
        cli::cli_progress_done()
      }

      # Calculate the final average SAGE values based on all completed permutations.
      final_sage_values = sage_values / n_completed

      # Return the computed scores and convergence data.
      list(
        scores = data.table(
          feature = names(final_sage_values),
          importance = as.numeric(final_sage_values)
        ),
        convergence_data = list(
          convergence_history = if (length(convergence_history) > 0) {
            rbindlist(convergence_history)
          } else {
            NULL
          },
          converged = converged,
          n_permutations_used = n_completed
        )
      )
    },

    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
      # This function evaluates the model's performance (loss) for a batch of feature coalitions.
      # It constructs expanded datasets for each coalition and then processes them in batches for prediction.

      # Get the number of unique coalitions to evaluate in this batch.
      n_coalitions = length(all_coalitions)
      # Get the number of observations in the test dataset.
      n_test = nrow(test_dt)
      # Get the number of observations in the reference dataset.
      n_reference = nrow(self$reference_data)

      # Pre-allocate a list to store the expanded data for each coalition.
      # Each element in this list will be a data.table containing test instances combined with reference instances.
      all_expanded_data = vector("list", n_coalitions)

      # Inform about the number of coalitions being evaluated in this batch (for debugging).
      if (xplain_opt("debug")) {
        cli::cli_inform("Evaluating {.val {length(all_coalitions)}} coalitions")
      }

      # Loop through each unique coalition in the current batch.
      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]] # Current coalition of features (e.g., c("x1", "x2"))

        # Create expanded datasets for the current coalition.
        # This is the core of SAGE's marginalization: for each test instance, we combine it
        # with every instance from the reference data. This creates a dataset of size
        # n_test * n_reference. For example, if n_test=100 and n_reference=50, this creates 5000 rows.
        test_expanded = test_dt[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add unique identifiers to track which original test instance and coalition
        # each row in the expanded dataset corresponds to. This is crucial for aggregation later.
        test_expanded[, .coalition_id := i] # Identifies the coalition this row belongs to
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)] # Identifies the original test instance

        # Determine which features need to be marginalized (i.e., features NOT in the current coalition).
        marginalize_features = setdiff(self$features, coalition)
        if (length(marginalize_features) > 0) {
          # Call the private .marginalize_features method (implemented by subclasses like MarginalSAGE or ConditionalSAGE).
          # This method replaces the values of 'marginalize_features' in 'test_expanded'
          # with values derived from 'reference_expanded' (marginal or conditional sampling).
          test_expanded = private$.marginalize_features(
            test_expanded,
            reference_expanded,
            marginalize_features
          )
        }

        # Store the processed (marginalized) expanded data for the current coalition.
        all_expanded_data[[i]] = test_expanded
      }

      # Combine ALL expanded data.tables from all coalitions in this batch into one large data.table.
      # This single data.table will be used for prediction, allowing for efficient batch processing by the learner.
      # Example: if there are 10 coalitions, and each expands to 5000 rows, combined_data will have 50,000 rows.
      combined_data = rbindlist(all_expanded_data)
      total_rows = nrow(combined_data)

      # Process data in batches if a batch_size is specified and the total number of rows
      # exceeds this batch_size. This prevents out-of-memory errors for very large datasets.
      if (!is.null(batch_size) && total_rows > batch_size) {
        # Calculate the number of batches needed.
        n_batches = ceiling(total_rows / batch_size)
        # Pre-allocate a list to store predictions from each batch.
        all_predictions = vector("list", n_batches)

        # Loop through each batch.
        for (batch_idx in seq_len(n_batches)) {
          # Determine the start and end rows for the current batch.
          start_row = (batch_idx - 1) * batch_size + 1
          end_row = min(batch_idx * batch_size, total_rows)

          # Extract the data for the current batch.
          batch_data = combined_data[start_row:end_row]

          # Predict on the current batch.
          if (xplain_opt("debug")) {
            cli::cli_inform(
              "Predicting on {.val {nrow(batch_data)}} instances in batch {.val {batch_idx}/{n_batches}}"
            )
          }
          pred_result = learner$predict_newdata(newdata = batch_data, task = self$task)

          # Store the predictions (probabilities for classification, response for regression).
          if (self$task$task_type == "classif") {
            all_predictions[[batch_idx]] = pred_result$prob
          } else {
            all_predictions[[batch_idx]] = pred_result$response
          }
        }

        # Combine predictions from all batches into a single prediction object.
        if (self$task$task_type == "classif") {
          combined_predictions = do.call(rbind, all_predictions)
        } else {
          combined_predictions = do.call(c, all_predictions)
        }
      } else {
        # If no batching is needed (total_rows <= batch_size or batch_size is NULL),
        # process all data at once.
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

      # Handle any NA values in predictions. This can happen if the learner produces NAs.
      if (any(is.na(combined_predictions))) {
        cli::cli_warn("Observed {.val {sum(is.na(combined_predictions))} NAs} in prediction")

        # For classification, replace NAs with uniform probabilities across classes.
        if (self$task$task_type == "classif") {
          n_classes = ncol(combined_predictions)
          uniform_prob = 1 / n_classes

          for (j in seq_len(n_classes)) {
            combined_predictions[is.na(combined_predictions[, j]), j] = uniform_prob
          }
        } else {
          # For regression, replace NAs with 0.
          combined_predictions[is.na(combined_predictions)] = 0
        }
      }

      # Add the combined predictions back to the 'combined_data' data.table.
      # Then, aggregate these predictions by coalition and original test instance.
      # This step averages the predictions over the reference data for each test instance and coalition.
      if (self$task$task_type == "classif") {
        # For classification, add each class probability as a separate column.
        n_classes = ncol(combined_predictions)
        class_names = colnames(combined_predictions)

        for (j in seq_len(n_classes)) {
          combined_data[, paste0(".pred_class_", j) := combined_predictions[, j]]
        }

        # Aggregate: calculate the mean probability for each class, for each
        # unique combination of coalition and original test instance.
        agg_cols = paste0(".pred_class_", seq_len(n_classes))
        avg_preds_by_coalition = combined_data[,
          lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          .SDcols = agg_cols,
          by = .(.coalition_id, .test_instance_id)
        ]

        # Rename the aggregated columns back to their original class names.
        setnames(avg_preds_by_coalition, agg_cols, class_names)
      } else {
        # For regression, add the single prediction column.
        combined_data[, .prediction := combined_predictions]

        # Aggregate: calculate the mean prediction for each unique combination
        # of coalition and original test instance.
        avg_preds_by_coalition = combined_data[,
          .(
            avg_pred = mean(.prediction, na.rm = TRUE)
          ),
          by = .(.coalition_id, .test_instance_id)
        ]
      }

      # Calculate the final loss for each coalition.
      # This involves creating a prediction object for each coalition and scoring it.
      coalition_losses = numeric(n_coalitions)
      for (i in seq_len(n_coalitions)) {
        coalition_data = avg_preds_by_coalition[.coalition_id == i] # Get aggregated predictions for this coalition

        # Create a mlr3 Prediction object (either Classification or Regression) from the aggregated data.
        if (self$task$task_type == "classif") {
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

        # Score the prediction object using the specified measure (e.g., MSE, CE).
        coalition_losses[i] = pred_obj$score(self$measure)
      }

      # Return the vector of losses for all coalitions in this batch.
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
#' @description [SAGE] with marginal sampling (features are marginalized independently).
#' This is the standard SAGE implementation.
#'
#' @seealso [ConditionalSAGE]
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
    #' @param task,learner,measure,resampling,features,n_permutations,reference_data,batch_size,max_reference_size,early_stopping,convergence_threshold,se_threshold,min_permutations,check_interval Passed to [SAGE].
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_permutations = 10L,
      reference_data = NULL,
      batch_size = 5000L,
      max_reference_size = 100L,
      early_stopping = FALSE,
      convergence_threshold = 0.01,
      se_threshold = Inf,
      min_permutations = 10L,
      check_interval = 2L
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
        max_reference_size = max_reference_size,
        early_stopping = early_stopping,
        convergence_threshold = convergence_threshold,
        se_threshold = se_threshold,
        min_permutations = min_permutations,
        check_interval = check_interval
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
#' @description [SAGE] with conditional sampling (features are "marginalized" conditionally).
#' Uses [ARFSampler] as default [ConditionalSampler].
#'
#' @seealso [MarginalSAGE]
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
    #' @param task,learner,measure,resampling,features,n_permutations,reference_data,batch_size,max_reference_size,early_stopping,convergence_threshold,se_threshold,min_permutations,check_interval Passed to [SAGE].
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to [ARFSampler].
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
      max_reference_size = 100L,
      early_stopping = FALSE,
      convergence_threshold = 0.01,
      se_threshold = Inf,
      min_permutations = 10L,
      check_interval = 2L
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
        max_reference_size = max_reference_size,
        early_stopping = early_stopping,
        convergence_threshold = convergence_threshold,
        se_threshold = se_threshold,
        min_permutations = min_permutations,
        check_interval = check_interval
      )

      self$label = "Conditional SAGE"
    }
  ),

  private = list(
    # ConditionalSAGE uses a more efficient approach that avoids expanding data unnecessarily
    .evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
      n_coalitions = length(all_coalitions)
      n_test = nrow(test_dt)
      n_reference = nrow(self$reference_data)

      if (xplain_opt("debug")) {
        cli::cli_inform("Evaluating {.val {length(all_coalitions)}} coalitions (ConditionalSAGE)")
      }

      # Pre-allocate list for expanded data
      all_expanded_data = vector("list", n_coalitions)

      # For each coalition, do conditional sampling BEFORE expansion
      for (i in seq_along(all_coalitions)) {
        coalition = all_coalitions[[i]]
        marginalize_features = setdiff(self$features, coalition)

        if (length(marginalize_features) > 0) {
          # Sample conditionally for unique test instances
          conditioning_set = coalition
          sampled_data = self$sampler$sample(
            feature = marginalize_features,
            data = test_dt,
            conditioning_set = conditioning_set
          )

          # Create the marginalized test data
          marginalized_test = copy(test_dt)
          marginalized_test[,
            (marginalize_features) := sampled_data[, marginalize_features, with = FALSE]
          ]
        } else {
          # No marginalization needed
          marginalized_test = copy(test_dt)
        }

        # NOW expand with reference data (only once, with correct values)
        test_expanded = marginalized_test[rep(seq_len(n_test), each = n_reference)]
        reference_expanded = self$reference_data[rep(seq_len(n_reference), times = n_test)]

        # Add tracking IDs
        test_expanded[, .coalition_id := i]
        test_expanded[, .test_instance_id := rep(seq_len(n_test), each = n_reference)]

        all_expanded_data[[i]] = test_expanded
      }

      # Rest of the method is the same as base implementation
      combined_data = rbindlist(all_expanded_data)
      total_rows = nrow(combined_data)

      # Process data in batches if needed
      if (!is.null(batch_size) && total_rows > batch_size) {
        n_batches = ceiling(total_rows / batch_size)
        all_predictions = vector("list", n_batches)

        for (batch_idx in seq_len(n_batches)) {
          start_row = (batch_idx - 1) * batch_size + 1
          end_row = min(batch_idx * batch_size, total_rows)
          batch_data = combined_data[start_row:end_row]

          if (xplain_opt("debug")) {
            cli::cli_inform(
              "Predicting on {.val {nrow(batch_data)}} instances in batch {.val {batch_idx}/{n_batches}}"
            )
          }

          pred_result = learner$predict_newdata(newdata = batch_data, task = self$task)

          if (self$task$task_type == "classif") {
            all_predictions[[batch_idx]] = pred_result$prob
          } else {
            all_predictions[[batch_idx]] = pred_result$response
          }
        }

        # Combine predictions
        if (self$task$task_type == "classif") {
          combined_predictions = do.call(rbind, all_predictions)
        } else {
          combined_predictions = do.call(c, all_predictions)
        }
      } else {
        # Single prediction
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

      # Handle NAs in predictions
      if (self$task$task_type == "classif") {
        if (any(is.na(combined_predictions))) {
          n_classes = ncol(combined_predictions)
          uniform_prob = 1 / n_classes
          for (j in seq_len(n_classes)) {
            combined_predictions[is.na(combined_predictions[, j]), j] = uniform_prob
          }
        }
      } else {
        combined_predictions[is.na(combined_predictions)] = 0
      }

      # Aggregate predictions by coalition and test instance
      if (self$task$task_type == "classif") {
        n_classes = ncol(combined_predictions)
        class_names = colnames(combined_predictions)

        for (j in seq_len(n_classes)) {
          combined_data[, paste0(".pred_class_", j) := combined_predictions[, j]]
        }

        agg_cols = paste0(".pred_class_", seq_len(n_classes))
        avg_preds_by_coalition = combined_data[,
          lapply(.SD, function(x) mean(x, na.rm = TRUE)),
          .SDcols = agg_cols,
          by = .(.coalition_id, .test_instance_id)
        ]

        setnames(avg_preds_by_coalition, agg_cols, class_names)
      } else {
        combined_data[, .prediction := combined_predictions]

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

        if (self$task$task_type == "classif") {
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
    }
  )
)
