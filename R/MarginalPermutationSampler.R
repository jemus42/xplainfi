#' @title Marginal Permutation Sampler
#'
#' @description Implements marginal permutation-based sampling for Permutation Feature Importance (PFI).
#' Each specified feature is randomly shuffled (permuted) independently, breaking the
#' relationship between the feature and the target as well as between rows.
#'
#' @details
#' The permutation sampler randomly shuffles feature values across observations:
#' - Each feature is permuted **independently** within its column
#' - The association between feature values and target values is broken
#' - The association between feature values **across rows** is broken
#' - The marginal distribution of each feature is preserved
#'
#' **Important distinction from SAGE's "marginal" approach:**
#' - `MarginalPermutationSampler`: Shuffles features independently, breaking row structure
#' - `MarginalSAGE`: Uses reference data but keeps rows intact (features in coalition stay together)
#'
#' This is the classic approach used in Permutation Feature Importance (PFI) and
#' assumes features are independent.
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 10)
#' task$data()
#' sampler = MarginalPermutationSampler$new(task)
#'
#' # Sample using row_ids from stored task
#' sampler$sample("x1")
#'
#' # Or use external data
#' data = task$data()
#' sampler$sample_newdata("x1", newdata = data)
MarginalPermutationSampler = R6Class(
	"MarginalPermutationSampler",
	inherit = MarginalSampler,
	public = list(
		#' @description
		#' Creates a new instance of the MarginalPermutationSampler class.
		#' @param task ([mlr3::Task]) Task to sample from.
		initialize = function(task) {
			super$initialize(task)
			self$label = "Permutation sampler"
		}
	),

	private = list(
		# Implement marginal sampling via independent permutation
		.sample_marginal = function(data, feature) {
			# Permute each feature independently
			data[, (feature) := lapply(.SD, sample), .SDcols = feature]

			data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
