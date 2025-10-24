#' @title Marginal Feature Sampler
#'
#' @description Implements marginal sampling for PFI, where the feature of interest
#' is sampled independently of other features
#'
#' @export
#' @examples
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' sampler = MarginalSampler$new(task)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#' # Or use external data
#' data = task$data()
#' sampled_data_ext = sampler$sample_newdata("x1", newdata = data)
MarginalSampler = R6Class(
	"MarginalSampler",
	inherit = FeatureSampler,
	public = list(
		#' @description
		#' Creates a new instance of the MarginalSampler class
		#' @param task ([mlr3::Task]) Task to sample from
		initialize = function(task) {
			super$initialize(task)
			self$label = "Marginal sampler"
		},

		#' @description
		#' Sample from stored task by permutation (marginal distribution).
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @return Modified copy with permuted feature(s).
		sample = function(feature, row_ids = NULL) {
			data_copy = private$.get_task_data_by_row_id(row_ids)

			# Handle both single and multiple features efficiently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]

			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},
		#' @description
		#' Sample from external data by permutation. See `$sample()` for details.
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

			# Handle both single and multiple features efficiently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]
			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
