#' @title k-Nearest Neighbors Conditional Sampler
#'
#' @description Implements conditional sampling using k-nearest neighbors (kNN).
#' For each observation, finds the k most similar observations based on conditioning
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
#' - No hyperparameters besides k
#' - Naturally respects local data structure
#'
#' **Limitations:**
#' - Sensitive to choice of k
#' - Can produce duplicates if k is small
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
#' @references
#' - Little, R. J., & Rubin, D. B. (2019). Statistical analysis with missing data (Vol. 793). John Wiley & Sons.
#' - Troyanskaya, O., et al. (2001). Missing value estimation methods for DNA microarrays. Bioinformatics, 17(6), 520-525.
#'
#' @export
KNNConditionalSampler = R6Class(
	"KNNConditionalSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		feature_types = c("numeric", "integer", "factor", "ordered", "logical"),

		#' @description
		#' Creates a new KNNConditionalSampler.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param k (`integer(1)`: `5L`) Number of nearest neighbors to sample from.
		initialize = function(task, k = 5L) {
			super$initialize(task)

			# Define param_set with k parameter
			self$param_set = paradox::ps(
				conditioning_set = paradox::p_uty(default = NULL),
				k = paradox::p_int(lower = 1L, default = 5L)
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
			if (is.null(conditioning_set) || length(conditioning_set) == 0) {
				# Simple random sampling from training data
				for (feat in feature) {
					data[, (feat) := sample(training_data[[feat]], .N, replace = TRUE)]
				}
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

			# For each observation, find k nearest neighbors and sample
			for (i in seq_len(nrow(data))) {
				# Compute distances to all training observations
				query_point = query_cond[i, , drop = FALSE]
				distances = sqrt(rowSums((sweep(train_cond, 2, query_point))^2))

				# Find k nearest neighbors
				# If k > n_train, use all training data
				k_actual = min(k, length(distances))
				neighbor_indices = order(distances)[seq_len(k_actual)]

				# Sample one neighbor uniformly
				sampled_idx = sample(neighbor_indices, 1)

				# Replace target features with values from sampled neighbor
				for (feat in feature) {
					data[i, (feat) := training_data[[feat]][sampled_idx]]
				}
			}

			data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
