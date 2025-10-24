#' @title Feature Sampler Class
#'
#' @description Base class for implementing different sampling strategies
#' for feature importance methods like PFI and CFI
#'
#' @export
FeatureSampler = R6Class(
	"FeatureSampler",
	public = list(
		#' @field task ([mlr3::Task]) Original task.
		task = NULL,
		#' @field label (`character(1)`) Name of the sampler.
		label = NULL,
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provied [mlr3::Task] to ensure compatibility.
		feature_types = c(
			"numeric",
			"factor",
			"ordered",
			"integer",
			"logical",
			"Date",
			"POSIXct",
			"character"
		),
		#' @field param_set ([paradox::ParamSet]) Parameter set for the sampler.
		param_set = NULL,

		#' @description
		#' Creates a new instance of the FeatureSampler class
		#' @param task ([mlr3::Task]) Task to sample from
		initialize = function(task) {
			self$task = mlr3::assert_task(task, feature_types = self$feature_types)
			# Initialize empty param_set - subclasses should define their own
			self$param_set = paradox::ps()
		},

		#' @description
		#' Sample values for feature(s) from stored task
		#' @param feature (`character`) Feature name(s) to sample (can be single or multiple). Must match those in the stored [Task][mlr3::Task].
		#' @param row_ids (`integer()`: `NULL`) Row IDs of the stored [Task][mlr3::Task] to use as basis for sampling.
		#' @return Modified copy of the input features with the feature(s) sampled:
		#'   A [data.table][data.table::data.table] with same number of columns and one row matching the supplied `row_ids`
		sample = function(feature, row_ids = NULL) {
			cli::cli_abort(c(
				"Abtract method",
				i = "Use a concrete implementation."
			))
		},
		#' @description
		#' Sample values for feature(s) using external data
		#' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
		#' @param newdata ([`data.table`][data.table::data.table] ) External data to use for sampling.
		sample_newdata = function(feature, newdata) {
			cli::cli_abort(c(
				"Abtract method",
				i = "Use a concrete implementation."
			))
		},

		#' @description
		#' Print sampler
		#'
		#' @param ... Ignored.
		print = function(...) {
			cli::cli_h1(self$label)
			cli::cli_ul()
			cli::cli_li(
				"Task: {.val {self$task$id}} ({.strong {self$task$nrow}x{self$task$n_features}})"
			)
			cli::cli_li("Supported feature type{?s}: {.val {self$feature_types}}")
			cli::cli_end()
		}
	),
	private = list(
		.get_task_data_by_row_id = function(row_ids) {
			if (!checkmate::test_subset(row_ids, self$task$row_ids)) {
				cli::cli_abort(c(
					x = "Requested {.val {length(setdiff(row_ids, self$task$row_ids))}} row_id{?s} not in stored {.cls Task}.",
					"!" = "For {.code $sample}, the row_ids must match those of the stored {.cls Task}."
				))
			}
			self$task$data(rows = row_ids)
		}
	)
)
