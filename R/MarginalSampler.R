#' @title Marginal Sampler Base Class
#'
#' @description Abstract base class for marginal sampling strategies that do not
#' condition on other features. Marginal samplers sample from P(X_S), the marginal
#' distribution of features S, ignoring any dependencies with other features.
#'
#' @details
#' This class provides a common interface for different marginal sampling approaches:
#'
#' - **MarginalPermutationSampler**: Shuffles features independently within the dataset
#' - **MarginalReferenceSampler**: Samples complete rows from reference data
#'
#' Both approaches sample from the marginal distribution P(X_S), but differ in how
#' they preserve or break within-row dependencies:
#'
#' - Permutation breaks ALL dependencies (both with target and between features)
#' - Reference sampling preserves WITHIN-row dependencies but breaks dependencies with test data
#'
#' **Comparison with ConditionalSampler:**
#'
#' - `MarginalSampler`: Samples from \eqn{P(X_S)} - no conditioning
#' - `ConditionalSampler`: Samples from \eqn{P(X_S | X_{-S})}- conditions on other features
#'
#' This base class implements the public `$sample()` and `$sample_newdata()` methods,
#' delegating to private `.sample_marginal()` which subclasses must implement.
#'
#' @export
MarginalSampler = R6Class(
	"MarginalSampler",
	inherit = FeatureSampler,
	public = list(
		#' @description
		#' Sample features from their marginal distribution.
		#' @param feature (`character()`) Feature name(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs from task to use.
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids = NULL) {
			data_copy = private$.get_task_data_by_row_id(row_ids)
			private$.sample_marginal(data_copy, feature)
		},

		#' @description
		#' Sample from external data.
		#' @param feature (`character()`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata) {
			if (inherits(newdata, "data.table")) {
				data_copy = data.table::copy(newdata)
			} else {
				data_copy = as.data.table(newdata)
			}
			private$.sample_marginal(data_copy, feature)
		}
	),

	private = list(
		# Abstract method for marginal sampling
		# Subclasses must implement this
		.sample_marginal = function(data, feature) {
			cli::cli_abort(c(
				"Abstract method",
				"i" = "Subclasses must implement the {.fn .sample_marginal} method"
			))
		}
	)
)
