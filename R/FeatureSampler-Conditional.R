#' @title Conditional Feature Sampler
#'
#' @description Base class for conditional sampling methods where features
#' are sampled conditionally on other features. This is an abstract class
#' that should be extended by concrete implementations.
#'
#' @export
ConditionalSampler = R6Class(
	"ConditionalSampler",
	inherit = FeatureSampler,
	public = list(
		#' @description
		#' Creates a new instance of the ConditionalSampler class
		#' @param task ([mlr3::Task]) Task to sample from
		initialize = function(task) {
			super$initialize(task)
			self$label = "Conditional sampler"

			# Define param_set with conditioning_set parameter for all conditional samplers
			self$param_set = paradox::ps(
				conditioning_set = paradox::p_uty(default = NULL)
			)
		},

		#' @description
		#' Sample from stored task conditionally on other features.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @param conditioning_set (`character` | `NULL`) Features to condition on.
		#' @param ... Additional arguments passed to the sampler implementation.
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids = NULL, conditioning_set = NULL, ...) {
			data_copy = private$.get_task_data_by_row_id(row_ids)
			private$.sample_conditional(data_copy, feature, conditioning_set, ...)
		},

		#' @description
		#' Sample from external data conditionally.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @param conditioning_set (`character` | `NULL`) Features to condition on.
		#' @param ... Additional arguments passed to the sampler implementation.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata, conditioning_set = NULL, ...) {
			# Create copy to avoid modifying original
			if (inherits(newdata, "data.table")) {
				data_copy = data.table::copy(newdata)
			} else {
				data_copy = as.data.table(newdata)
			}

			private$.sample_conditional(data_copy, feature, conditioning_set, ...)
		}
	),

	private = list(
		# @description
		# Internal method for conditional sampling. Must be implemented by subclasses.
		# @param data (`data.table`) Data to sample from (copy that can be modified).
		# @param feature (`character`) Feature(s) to sample.
		# @param conditioning_set (`character` | `NULL`) Features to condition on.
		# @param ... Additional sampler-specific arguments.
		.sample_conditional = function(data, feature, conditioning_set, ...) {
			cli::cli_abort(c(
				"Abstract method",
				i = "Subclasses must implement the {.fn .sample_conditional} method"
			))
		}
	)
)
