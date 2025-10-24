#' @title Gaussian Conditional Sampler
#'
#' @description Implements conditional sampling assuming features follow a multivariate
#' Gaussian distribution. Computes conditional distributions analytically using standard
#' formulas for multivariate normal distributions.
#'
#' @details
#' For a joint Gaussian distribution \eqn{X \sim N(\mu, \Sigma)}, partitioned as
#' \eqn{X = (X_A, X_B)}, the conditional distribution is:
#'
#' \deqn{X_B | X_A = x_A \sim N(\mu_{B|A}, \Sigma_{B|A})}
#'
#' where:
#' \deqn{\mu_{B|A} = \mu_B + \Sigma_{BA} \Sigma_{AA}^{-1} (x_A - \mu_A)}
#' \deqn{\Sigma_{B|A} = \Sigma_{BB} - \Sigma_{BA} \Sigma_{AA}^{-1} \Sigma_{AB}}
#'
#' This is equivalent to the regression formulation used by fippy:
#' \deqn{\beta = \Sigma_{BA} \Sigma_{AA}^{-1}}
#' \deqn{\mu_{B|A} = \mu_B + \beta (x_A - \mu_A)}
#' \deqn{\Sigma_{B|A} = \Sigma_{BB} - \beta \Sigma_{AB}}
#'
#' **Assumptions:**
#' - Features are approximately multivariate normal
#' - Only continuous features are supported
#'
#' **Advantages:**
#' - Very fast (closed-form solution)
#' - Deterministic (given seed)
#' - No hyperparameters
#' - Memory efficient
#'
#' **Limitations:**
#' - Strong distributional assumption
#' - May produce out-of-range values for bounded features
#' - Cannot handle categorical features
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#' sampler = GaussianConditionalSampler$new(task)
#'
#' # Sample x2, x3 conditioned on x1
#' test_data = task$data(rows = 1:5)
#' sampled = sampler$sample_newdata(
#'   feature = c("important2", "important3"),
#'   newdata = test_data,
#'   conditioning_set = "important1"
#' )
#'
#' @export
GaussianConditionalSampler = R6Class(
	"GaussianConditionalSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		feature_types = c("numeric", "integer"),

		#' @field mu (`numeric()`) Mean vector estimated from training data.
		mu = NULL,

		#' @field sigma (`matrix()`) Covariance matrix estimated from training data.
		sigma = NULL,

		#' @description
		#' Creates a new GaussianConditionalSampler.
		#' @param task ([mlr3::Task]) Task to sample from. Must have only numeric/integer features.
		initialize = function(task) {
			super$initialize(task)

			# Extract feature data as matrix
			X = as.matrix(self$task$data(cols = self$task$feature_names))

			# Estimate mean and covariance
			self$mu = colMeans(X)
			self$sigma = stats::cov(X)

			self$label = "Gaussian Conditional Sampler"
		},

		#' @description
		#' Sample features from their conditional distribution using closed-form Gaussian formulas.
		#'
		#' @param feature (`character()`) Feature name(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs from task to use as conditioning values.
		#' @param conditioning_set (`character()` | `NULL`) Features to condition on.
		#'   If `NULL`, samples from marginal distribution (no conditioning).
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids = NULL, conditioning_set = NULL) {
			data_copy = private$.get_task_data_by_row_id(row_ids)
			private$.sample_conditional(data_copy, feature, conditioning_set)
		},

		#' @description
		#' Sample from external data conditionally.
		#'
		#' @param feature (`character()`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @param conditioning_set (`character()` | `NULL`) Features to condition on.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata, conditioning_set = NULL) {
			# Create copy to avoid modifying original
			if (inherits(newdata, "data.table")) {
				data_copy = data.table::copy(newdata)
			} else {
				data_copy = as.data.table(newdata)
			}

			private$.sample_conditional(data_copy, feature, conditioning_set)
		}
	),

	private = list(
		# Core sampling logic shared between sample() and sample_newdata()
		.sample_conditional = function(data, feature, conditioning_set) {
			# Handle marginal case (no conditioning)
			if (is.null(conditioning_set) || length(conditioning_set) == 0) {
				# Sample from marginal distribution N(μ_feature, Σ_feature)
				mu_marg = self$mu[feature]
				sigma_marg = self$sigma[feature, feature, drop = FALSE]

				samples = mvtnorm::rmvnorm(
					n = nrow(data),
					mean = mu_marg,
					sigma = private$.ensure_pd(sigma_marg)
				)

				data[, (feature) := as.data.table(samples)]
				return(data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)])
			}

			# Conditional case: X_B | X_A = x_A
			# Use lm.fit() for numerically stable regression coefficient estimation
			# This is more robust than manual matrix inversion

			# Get training data from task
			training_data = self$task$data(cols = self$task$feature_names)

			# Prepare design matrix (add intercept)
			X_cond = as.matrix(training_data[, .SD, .SDcols = conditioning_set])
			X_cond = cbind(1, X_cond) # Add intercept

			# Fit separate linear models for each target feature
			# Store coefficients and compute residual covariance
			n_features = length(feature)
			coef_list = vector("list", n_features)
			residuals_mat = matrix(0, nrow = nrow(training_data), ncol = n_features)

			for (j in seq_along(feature)) {
				y = training_data[[feature[j]]]

				# Use lm.fit() for robust coefficient estimation via QR decomposition
				fit = lm.fit(X_cond, y)
				coef_list[[j]] = fit$coefficients
				residuals_mat[, j] = fit$residuals
			}

			# Compute conditional covariance from residuals
			# This is empirically estimated from the regression residuals
			cond_cov = stats::cov(residuals_mat)
			if (n_features == 1) {
				cond_cov = matrix(cond_cov, 1, 1)
			}
			cond_cov = private$.ensure_pd(cond_cov)

			# Predict conditional means for new data
			X_new = as.matrix(data[, .SD, .SDcols = conditioning_set])
			X_new = cbind(1, X_new) # Add intercept

			# Compute predictions for each feature
			pred_mat = matrix(0, nrow = nrow(data), ncol = n_features)
			for (j in seq_along(feature)) {
				pred_mat[, j] = X_new %*% coef_list[[j]]
			}

			# Sample from multivariate normal with predicted means
			samples = mvtnorm::rmvnorm(nrow(data), mean = rep(0, n_features), sigma = cond_cov)
			samples = samples + pred_mat # Add conditional means

			# Update data
			data[, (feature) := as.data.table(samples)]

			data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},

		# Ensure matrix is positive definite for mvtnorm::rmvnorm
		# Uses eigenvalue decomposition to project to nearest PD matrix
		.ensure_pd = function(mat, tol = 1e-10) {
			# Make symmetric (numerical precision)
			mat = (mat + t(mat)) / 2

			# Eigenvalue decomposition
			eig = eigen(mat, symmetric = TRUE)

			# Truncate negative eigenvalues to small positive value
			eig$values[eig$values < tol] = tol

			# Reconstruct matrix
			eig$vectors %*% diag(eig$values, nrow = length(eig$values)) %*% t(eig$vectors)
		}
	)
)
