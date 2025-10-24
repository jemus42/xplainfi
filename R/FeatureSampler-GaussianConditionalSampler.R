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
			# Extract indices for marginalized (B) and conditioning (A) features
			marg_idx = match(feature, names(self$mu))
			cond_idx = match(conditioning_set, names(self$mu))

			# Extract subvectors and submatrices
			mu_marg = self$mu[marg_idx]
			mu_cond = self$mu[cond_idx]
			sigma_mm = self$sigma[marg_idx, marg_idx, drop = FALSE]
			sigma_mc = self$sigma[marg_idx, cond_idx, drop = FALSE]
			sigma_cc = self$sigma[cond_idx, cond_idx, drop = FALSE]
			sigma_cm = self$sigma[cond_idx, marg_idx, drop = FALSE]

			# Compute regression coefficient β = Σ_BA Σ_AA^(-1)
			# Using pseudo-inverse for numerical stability (following fippy)
			sigma_cc_inv = MASS::ginv(sigma_cc)
			beta = sigma_mc %*% sigma_cc_inv

			# Compute conditional covariance (constant across observations)
			# Σ_B|A = Σ_BB - β Σ_AB
			cond_cov = sigma_mm - beta %*% sigma_cm
			cond_cov = private$.ensure_pd(cond_cov)

			# For each row, compute conditional mean and sample
			for (i in seq_len(nrow(data))) {
				# Extract conditioning values
				x_cond = as.numeric(data[i, conditioning_set, with = FALSE])

				# Conditional mean: μ_B|A = μ_B + β (x_A - μ_A)
				cond_mean = mu_marg + as.vector(beta %*% (x_cond - mu_cond))

				# Sample from conditional distribution
				sampled = mvtnorm::rmvnorm(1, mean = cond_mean, sigma = cond_cov)

				# Update data
				data[i, (feature) := as.list(sampled)]
			}

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
