#' @title Marginal Reference Sampler
#'
#' @description Samples complete observations from reference data to replace feature values.
#' This approach samples from the marginal distribution while preserving within-row
#' feature dependencies.
#'
#' @details
#' This sampler implements what is called "marginal imputation" in the SAGE literature
#' (Covert et al. 2020). For each observation, it samples a complete row from reference
#' data and takes the specified feature values from that row. This approach:
#'
#' - Samples from the marginal distribution P(X_S) where S is the set of features
#' - Preserves dependencies **within** the sampled reference row
#' - Breaks dependencies **between** test and reference data
#'
#' **Terminology note:** In SAGE literature, this is called "marginal imputation" because
#' features outside the coalition are "imputed" by sampling from their marginal distribution.
#' We use `MarginalReferenceSampler` to avoid confusion with missing data imputation and to
#' clarify that it samples from reference data.
#'
#' **Comparison with other samplers:**
#'
#' - `PermutationSampler`: Shuffles each feature independently, breaking all row structure
#' - `MarginalReferenceSampler`: Samples complete rows, preserving within-row dependencies
#' - `ConditionalSampler`: Samples from P(X_S | X_{-S}), conditioning on other features
#'
#' **Use in SAGE:**
#'
#' This is the default approach for `MarginalSAGE`. For a test observation x and features
#' to marginalize S, it samples a reference row x_ref and creates a "hybrid" observation
#' combining x's coalition features with x_ref's marginalized features.
#'
#' @examples
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 100)
#'
#' # Default: uses task's own data as reference
#' sampler = MarginalReferenceSampler$new(task)
#' sampled = sampler$sample("important1", row_ids = 1:10)
#'
#' # Custom reference data
#' reference_task = tgen("friedman1")$generate(n = 50)
#' sampler_custom = MarginalReferenceSampler$new(
#'   task = task,
#'   reference_data = reference_task$data()
#' )
#'
#' @references
#' Covert, I., Lundberg, S., & Lee, S. I. (2020).
#' Understanding Global Feature Contributions With Additive Importance Measures.
#' *Advances in Neural Information Processing Systems*, 33.
#'
#' @export
MarginalReferenceSampler = R6Class(
	"MarginalReferenceSampler",
	inherit = FeatureSampler,
	public = list(
		#' @field reference_data ([`data.table`][data.table::data.table])
		#'   Reference data to sample from for marginalization.
		reference_data = NULL,

		#' @description
		#' Creates a new instance of the MarginalReferenceSampler class.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param reference_data ([`data.table`][data.table::data.table] | `NULL`)
		#'   Reference data to sample from. If `NULL`, uses the task's own data.
		initialize = function(task, reference_data = NULL) {
			super$initialize(task)

			# Default: use task's own data as reference
			if (is.null(reference_data)) {
				self$reference_data = task$data(cols = task$feature_names)
			} else {
				# Validate reference data has required features
				if (inherits(reference_data, "data.table")) {
					self$reference_data = data.table::copy(reference_data)
				} else {
					self$reference_data = as.data.table(reference_data)
				}

				missing_features = setdiff(task$feature_names, names(self$reference_data))
				if (length(missing_features) > 0) {
					cli::cli_abort(c(
						"Reference data missing required features",
						"x" = "Missing: {.val {missing_features}}"
					))
				}

				# Keep only task features
				self$reference_data = self$reference_data[, .SD, .SDcols = task$feature_names]
			}

			self$label = "Marginal reference sampler"
		},

		#' @description
		#' Sample from stored task by sampling complete reference rows.
		#' @param feature (`character()`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids = NULL) {
			data_copy = private$.get_task_data_by_row_id(row_ids)

			# For each row, sample one complete observation from reference data
			# and take the specified features from it
			sampled_indices = sample.int(
				nrow(self$reference_data),
				nrow(data_copy),
				replace = TRUE
			)

			# Replace features with values from sampled reference rows
			data_copy[, (feature) := self$reference_data[sampled_indices, .SD, .SDcols = feature]]

			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		},

		#' @description
		#' Sample from external data by sampling complete reference rows.
		#' @param feature (`character()`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(feature, newdata) {
			# Create a copy to avoid modifying the original data
			if (inherits(newdata, "data.table")) {
				data_copy = data.table::copy(newdata)
			} else {
				data_copy = as.data.table(newdata)
			}

			# For each row, sample one complete observation from reference data
			sampled_indices = sample.int(
				nrow(self$reference_data),
				nrow(data_copy),
				replace = TRUE
			)

			# Replace features with values from sampled reference rows
			data_copy[, (feature) := self$reference_data[sampled_indices, .SD, .SDcols = feature]]

			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
