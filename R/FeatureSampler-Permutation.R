#' @title Permutation Feature Sampler
#'
#' @description Implements permutation-based sampling for Permutation Feature Importance (PFI).
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
#' - `PermutationSampler`: Shuffles features independently, breaking row structure
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
#' sampler = PermutationSampler$new(task)
#'
#' # Sample using row_ids from stored task
#' sampler$sample("x1")
#'
#' # Or use external data
#' data = task$data()
#' sampler$sample_newdata("x1", newdata = data)
PermutationSampler = R6Class(
	"PermutationSampler",
	inherit = FeatureSampler,
	public = list(
		#' @description
		#' Creates a new instance of the PermutationSampler class.
		#' @param task ([mlr3::Task]) Task to sample from.
		initialize = function(task) {
			super$initialize(task)
			self$label = "Permutation sampler"
		},

		#' @description
		#' Sample from stored task by random permutation.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @return Modified copy with permuted feature(s).
		sample = function(feature, row_ids = NULL) {
			data_copy = private$.get_task_data_by_row_id(row_ids)

			# Permute each feature independently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]

			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},

		#' @description
		#' Sample from external data by random permutation.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @return Modified copy with permuted feature(s).
		sample_newdata = function(feature, newdata) {
			# Create a copy to avoid modifying the original data
			if (inherits(newdata, "data.table")) {
				data_copy = data.table::copy(newdata)
			} else {
				setDT(newdata)
			}

			# Permute each feature independently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]
			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
