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
#' **Standard Error Calculation**: The standard errors (SE) reported in
#' `$convergence_history` reflect the uncertainty in Shapley value estimation
#' across different random permutations within a single resampling iteration.
#' These SEs quantify the Monte Carlo sampling error for a fixed trained model
#' and are only valid for inference about the importance of features for that
#' specific model. They do not capture broader uncertainty from model variability
#' across different train/test splits or resampling iterations.
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
		#' @field convergence_history ([`data.table`][data.table::data.table]) History of SAGE values during computation.
		convergence_history = NULL,
		#' @field converged (`logical(1)`) Whether convergence was detected.
		converged = FALSE,
		#' @field n_permutations_used (`integer(1)`) Actual number of permutations used.
		n_permutations_used = NULL,

		#' @description
		#' Creates a new instance of the SAGE class.
		#' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod.
		#' @param n_permutations (`integer(1)`: `10L`) Number of permutations _per coalition_ to sample for Shapley value estimation.
		#'   The total number of evaluated coalitions is `1 (empty) + n_permutations * n_features`.
		#' @param batch_size (`integer(1)`: `5000L`) Maximum number of observations to process in a single prediction call.
		#' @param n_samples (`integer(1)`: `100L`) Number of samples to use for marginalizing out-of-coalition features.
		#'   For [MarginalSAGE], this is the number of marginal data samples ("background data" in other implementations).
		#'   For [ConditionalSAGE], this is the number of conditional samples per test instance retrieved from `sampler`.
		#' @param early_stopping (`logical(1)`: `FALSE`) Whether to enable early stopping based on convergence detection.
		#' @param convergence_threshold (`numeric(1)`: `0.01`) Relative change threshold for convergence detection.
		#' @param se_threshold (`numeric(1)`: `Inf`) Standard error threshold for convergence detection.
		#' @param min_permutations (`integer(1)`: `10L`) Minimum permutations before checking convergence.
		#' @param check_interval (`integer(1)`: `2L`) Check convergence every N permutations.
		initialize = function(
			task,
			learner,
			measure,
			resampling = NULL,
			features = NULL,
			n_permutations = 10L,
			batch_size = 5000L,
			n_samples = 100L,
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

			# Set parameters
			ps = ps(
				n_permutations = paradox::p_int(lower = 1L, default = 10L),
				batch_size = paradox::p_int(lower = 1L, default = 5000L),
				n_samples = paradox::p_int(lower = 1L, default = 100L),
				early_stopping = paradox::p_lgl(default = FALSE),
				convergence_threshold = paradox::p_dbl(lower = 0, upper = 1, default = 0.01),
				se_threshold = paradox::p_dbl(lower = 0, default = Inf),
				min_permutations = paradox::p_int(lower = 5L, default = 10L),
				check_interval = paradox::p_int(lower = 1L, default = 2L)
			)
			ps$values$n_permutations = n_permutations
			ps$values$batch_size = batch_size
			ps$values$n_samples = n_samples
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
			# Store results
			self$resample_result = rr

			# For convergence tracking, we'll use the first resampling iteration
			# (convergence is about permutation count, not resampling)
			iter_for_convergence = 1L

			# Compute SAGE values for convergence tracking (first iteration)
			first_result = private$.compute_sage_scores(
				learner = rr$learners[[iter_for_convergence]],
				test_dt = self$task$data(rows = rr$resampling$test_set(iter_for_convergence)),
				n_permutations = self$n_permutations,
				batch_size = batch_size,
				early_stopping = early_stopping,
				convergence_threshold = convergence_threshold,
				se_threshold = se_threshold,
				min_permutations = min_permutations,
				check_interval = check_interval
			)

			# Extract convergence data from first iteration
			# `convergence_data` exists even if early_stopping = FALSE
			self$convergence_history = first_result$convergence_data$convergence_history
			self$converged = first_result$convergence_data$converged
			self$n_permutations_used = first_result$convergence_data$n_permutations_used

			# If we have multiple resampling iterations, compute the rest without convergence tracking
			if (self$resampling$iters > 1) {
				remaining_results = lapply(seq_len(self$resampling$iters)[-iter_for_convergence], \(iter) {
					private$.compute_sage_scores(
						learner = rr$learners[[iter]],
						test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
						# n_permutations_used is either same as n_permutations (if early_stopping = FALSE)
						# or its a smaller value if early_stopping = TRUE and it stopped early
						# The fallback to self$n_permutations should not be needed
						n_permutations = self$n_permutations_used %||% self$n_permutations,
						batch_size = batch_size,
						# Only track convergence etc. for first iteration
						early_stopping = FALSE
					)
				})

				# Extract scores from all results (always list format now)
				all_scores = c(list(first_result$scores), lapply(remaining_results, function(x) x$scores))
			} else {
				all_scores = list(first_result$scores)
			}

			# Combine results across resampling iterations
			scores = rbindlist(all_scores, idcol = "iter_rsmp")

			# iter_rsmp, feature, importance -- score_baseline or so don't apply here
			private$.scores = scores
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
				ggplot2::geom_line(linewidth = 1) +
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
		# This function computes the SAGE values for a single resampling iteration.
		# It iterates through permutations of features, evaluates coalitions, and calculates marginal contributions.
		.compute_sage_scores = function(
			learner,
			test_dt,
			n_permutations,
			batch_size = NULL,
			early_stopping = FALSE,
			convergence_threshold = 0.01,
			se_threshold = Inf,
			min_permutations = 10L,
			check_interval = 5L
		) {
			# Initialize numeric vectors to store marginal contributions and their squares for variance calculation.
			# We track both sum and sum of squares to calculate running variance and standard errors.
			sage_values = numeric(length(self$features)) # Sum of marginal contributions
			sage_values_sq = numeric(length(self$features)) # Sum of squared marginal contributions
			names(sage_values) = self$features
			names(sage_values_sq) = self$features

			# Pre-generate `n_permutations` permutations upfront
			# Relevant for reproducibility, especially when using early stopping or parallel processing.
			# Example: if self$features = c("x1", "x2", "x3") and n_permutations = 2,
			# all_permutations might be list(c("x2", "x1", "x3"), c("x3", "x1", "x2"))
			all_permutations = replicate(n_permutations, sample(self$features), simplify = FALSE)

			# Initialize variables for iterative checkpoint-based computation.
			# This allows for early stopping based on convergence and provides progress updates.
			convergence_history = list() # Stores SAGE values at each checkpoint for convergence tracking
			n_completed = 0 # Number of permutations processed so far
			converged = FALSE # Flag to indicate if convergence has been detected
			baseline_loss = NULL # Loss of the empty coalition (model with no features / all features marginalized)

			# Calculate total checkpoints for progress tracking.
			# A checkpoint is a group of 'check_interval' permutations.
			total_checkpoints = ceiling(n_permutations / check_interval)
			current_checkpoint = 0

			# Start checkpoint-based progress bar if progress display is enabled.
			if (xplain_opt("progress")) {
				cli::cli_progress_bar(
					"Computing SAGE values",
					total = total_checkpoints
				)
			}

			# Main loop: Process permutations in checkpoints until all permutations are done or convergence is reached.
			while (n_completed < n_permutations && !converged) {
				# Determine the size of the current checkpoint.
				# This ensures that the last checkpoint processes only the remaining permutations.
				checkpoint_size = min(check_interval, n_permutations - n_completed)
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
							"i" = "Saved {.val {n_permutations - n_completed}} permutations"
						)
					} else {
						# If SE threshold is infinite, only check relative change
						converged = rel_change_converged
						convergence_msg = c(
							"v" = "SAGE converged after {.val {n_completed}} permutations",
							"i" = "Maximum relative change: {.val {round(max_change, 4)}}",
							"i" = "Saved {.val {n_permutations - n_completed}} permutations"
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

		# Template method: Defines the complete prediction and aggregation pipeline
		# Subclasses only need to implement .expand_coalitions_data()
		.evaluate_coalitions_batch = function(learner, test_dt, all_coalitions, batch_size = NULL) {
			n_test = nrow(test_dt)

			if (xplain_opt("debug")) {
				cli::cli_inform("Evaluating {.val {length(all_coalitions)}} coalitions")
			}

			# STEP 1: Subclass-specific data expansion (abstract method)
			combined_data = private$.expand_coalitions_data(test_dt, all_coalitions)

			# STEPS 2-5: Shared processing pipeline using general utilities
			predictions = sage_batch_predict(
				learner,
				combined_data,
				self$task,
				batch_size,
				self$task$task_type
			)
			if (anyNA(predictions)) {
				cli::cli_warn("Encountered missing values in model prediction")
			}
			avg_preds = sage_aggregate_predictions(
				combined_data,
				predictions,
				self$task$task_type,
				self$task$class_names
			)

			# Private method (needs self$task and self$measure)
			coalition_losses = private$.calculate_coalition_losses(avg_preds, n_test, test_dt)

			coalition_losses
		},

		# Abstract method - must be implemented by subclasses
		# Returns: data.table with all feature columns plus .coalition_id and .test_instance_id
		.expand_coalitions_data = function(test_dt, all_coalitions) {
			cli::cli_abort(c(
				"Abstract method not implemented",
				"i" = "Subclasses must implement {.fn .expand_coalitions_data}",
				"i" = "This method should return a data.table with:",
				"*" = "All feature columns (with marginalized features replaced/sampled)",
				"*" = "{.field .coalition_id}: integer identifying which coalition",
				"*" = "{.field .test_instance_id}: integer identifying original test instance"
			))
		},

		# Private method - needs self$task and self$measure
		# Calculates losses from averaged predictions for each coalition
		.calculate_coalition_losses = function(avg_preds, n_test, test_dt) {
			n_coalitions = length(unique(avg_preds$.coalition_id))
			coalition_losses = numeric(n_coalitions)

			for (i in seq_len(n_coalitions)) {
				coalition_data = avg_preds[.coalition_id == i]

				pred_obj = if (self$task$task_type == "classif") {
					PredictionClassif$new(
						row_ids = seq_len(n_test),
						truth = test_dt[[self$task$target_names]],
						prob = as.matrix(coalition_data[, .SD, .SDcols = self$task$class_names])
					)
				} else {
					PredictionRegr$new(
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
