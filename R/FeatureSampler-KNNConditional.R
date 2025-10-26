#' @title k-Nearest Neighbors Conditional Sampler
#'
#' @description Implements conditional sampling using k-nearest neighbors (kNN).
#' For each observation, finds the `k` most similar observations based on conditioning
#' features, then samples the target features from these neighbors.
#'
#' @details
#' This sampler approximates the conditional distribution \eqn{P(X_B | X_A = x_A)} by:
#' 1. Finding the k nearest neighbors of \eqn{x_A} in the training data using Euclidean distance
#' 2. Sampling uniformly from the target feature values \eqn{X_B} of these k neighbors
#'
#' This is a simple, non-parametric approach that:
#' - Requires no distributional assumptions
#' - Handles mixed feature types naturally
#' - Is computationally efficient (no model fitting required)
#' - Adapts locally to the data structure
#'
#' The method is related to hot-deck imputation and kNN imputation techniques used in
#' missing data problems. As \eqn{k \to \infty} and \eqn{k/n \to 0}, the kNN conditional
#' distribution converges to the true conditional distribution under mild regularity
#' conditions (Lipschitz continuity).
#'
#' **Advantages:**
#' - Very fast (no model training)
#' - Works with any feature types
#' - No hyperparameters besides `k`
#' - Naturally respects local data structure
#'
#' **Limitations:**
#' - Sensitive to choice of `k`
#' - The full task data is required for prediction
#' - Can produce duplicates if `k` is small
#' - Distance metric matters (currently Euclidean only)
#' - May not extrapolate well to new regions
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sampler = KNNConditionalSampler$new(task, k = 5)
#'
#' # Sample features conditioned on others
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#'
#' @references `r print_bib("little_2019", "troyanskaya_2001")`
#'
#' @export
KNNConditionalSampler = R6Class(
	"KNNConditionalSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		feature_types = c("numeric", "integer"),

		#' @description
		#' Creates a new KNNConditionalSampler.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`.
		#' @param k (`integer(1)`: `5L`) Number of nearest neighbors to sample from.
		initialize = function(task, conditioning_set = NULL, k = 5L) {
			super$initialize(task, conditioning_set = conditioning_set)

			# Extend param_set with k parameter
			self$param_set = c(
				self$param_set,
				paradox::ps(k = paradox::p_int(lower = 1L, default = 5L))
			)
			self$param_set$set_values(k = k)

			self$label = "k-Nearest Neighbors Conditional Sampler"
		},

		#' @description
		#' Sample features from their kNN-based conditional distribution.
		#'
		#' @param feature (`character()`) Feature name(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs from task to use as conditioning values.
		#' @param conditioning_set (`character()` | `NULL`) Features to condition on.
		#'   If `NULL`, samples from marginal distribution (random sampling from training data).
		#' @param k (`integer(1)` | `NULL`) Number of neighbors. If `NULL`, uses stored parameter.
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids = NULL, conditioning_set = NULL, k = NULL) {
			super$sample(feature, row_ids, conditioning_set, k = k)
		},

		#' @description
		#' Sample from external data conditionally.
		#'
		#' @param feature (`character()`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @param conditioning_set (`character()` | `NULL`) Features to condition on.
		#' @param k (`integer(1)` | `NULL`) Number of neighbors. If `NULL`, uses stored parameter.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata, conditioning_set = NULL, k = NULL) {
			super$sample_newdata(feature, newdata, conditioning_set, k = k)
		}
	),

	private = list(
		# Core kNN sampling logic implementing k-nearest neighbors conditional sampling
		.sample_conditional = function(data, feature, conditioning_set, k = NULL, ...) {
			# Resolve k parameter
			k = resolve_param(k, self$param_set$values$k, 5L)

			# Get training data from task
			training_data = self$task$data(cols = self$task$feature_names)

			# Handle marginal case (no conditioning)
			if (length(conditioning_set) == 0) {
				# Simple random sampling (with replacement) from training data
				data[, (feature) := lapply(.SD, sample, replace = TRUE), .SDcols = feature]
				return(data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			# Conditional case: find k nearest neighbors for each observation
			# Extract conditioning features from both data sources
			query_cond = as.matrix(data[, .SD, .SDcols = conditioning_set])
			train_cond = as.matrix(training_data[, .SD, .SDcols = conditioning_set])

			# Normalize numeric features for distance calculation
			# This ensures all features contribute equally to distance
			numeric_cols = sapply(conditioning_set, function(col) {
				is.numeric(training_data[[col]])
			})

			if (any(numeric_cols)) {
				# Compute means and SDs from training data
				means = colMeans(train_cond[, numeric_cols, drop = FALSE])
				sds = apply(train_cond[, numeric_cols, drop = FALSE], 2, stats::sd)
				sds[sds == 0] = 1 # Avoid division by zero for constant features

				# Standardize both query and training data
				query_cond[, numeric_cols] = scale(
					query_cond[, numeric_cols, drop = FALSE],
					center = means,
					scale = sds
				)
				train_cond[, numeric_cols] = scale(
					train_cond[, numeric_cols, drop = FALSE],
					center = means,
					scale = sds
				)
			}

			# Create temporary index column
			data[, query_idx := .I]

			# Do the nearest neighbor sampling
			sampled = data[,
				{
					qp = query_cond[query_idx, , drop = FALSE]
					# Euclidian distances
					# sweep gets difference between each element in train_cond and query point
					dists = sqrt(rowSums((sweep(train_cond, 2, qp))^2))

					k_actual = min(k, length(dists))
					# Thought I could use partial sorting here but
					#   sort(dists, partial = 1:2, index.return = TRUE)$ix[1:k]
					# ...doesn't work because we can't combine partial sorting and index return
					# This works but does a full sort
					# neighbors = order(dists)[seq_len(k_actual)]
					# This way we "just" get the indices of the kth neighbours we want
					neighbors = which(dists <= sort(dists, partial = k_actual)[k_actual])
					# After getting the neighbours, pick one random value as the sampling result
					sampled_idx = sample(neighbors, 1)

					.(sampled_idx = sampled_idx)
				},
				by = query_idx
			]

			# Assign the features
			data[, (feature) := training_data[sampled$sampled_idx, .SD, .SDcols = feature]]
			data[, query_idx := NULL]

			data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
