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
		#' @param conditioning_set (`character` | `NULL`) Features to condition on. If `NULL`, uses all other features.
		#'   This is only supported by some implementations, e.g. [ARFSampler].
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids, conditioning_set = NULL) {
			cli::cli_abort(c(
				"Abtract method",
				i = "Use a concrete implementation like {.cls ARFSampler}) or {.cls KnockoffSamplerGaussian}"
			))
		},

		#' @description
		#' Sample from external data conditionally. See `$sample()` for details.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @param conditioning_set (`character` | `NULL`) Features to condition on.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata, conditioning_set = NULL) {
			cli::cli_abort(c(
				x = "{.cls {class(self)[[1]]}} does not support sampling from external data.",
				i = "Only some samplers (e.g., {.cls ARFSampler}) support sampling using external data."
			))
		}
	)
)
