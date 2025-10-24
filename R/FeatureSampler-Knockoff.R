#' @title Knockoff-based Conditional Sampler
#'
#' @description Implements conditional sampling using Knockoffs.
#'
#' @details
#' The KnockoffSampler samples [Knockoffs][knockoff::knockoff] based on the task data.
#' This class allows arbitrary `knockoff_fun`, which also means that no input checking
#' against supported feature types can be done. Use [KnockoffGaussianSampler] or
#' [KnockoffSequentialSampler] for these variants specifically.
#'
#' @examplesIf requireNamespace("knockoff", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler with default parameters
#' sampler = KnockoffSampler$new(task)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#' \dontrun{
#' # Example with sequential knockoffs (https://github.com/kormama1/seqknockoff)
#' task = tgen("simplex")$generate(n = 100)
#' sampler_seq = KnockoffSampler$new(task, knockoff_fun = seqknockoff::knockoffs_seq)
#' sampled_seq = sampler_seq$sample("x1")
#' }
#' @references `r print_bib("watson_2021", "blesch_2023")`
#'
#' @export
KnockoffSampler = R6Class(
	"KnockoffSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field x_tilde Knockoff matrix with one (or `iters`) row(s) per original observation in `task`.
		x_tilde = NULL,

		#' @description
		#' Creates a new instance of the KnockoffSampler class.
		#' @param task ([mlr3::Task]) Task to sample from
		# @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
		#' @param knockoff_fun (`function`) Step size for variance adjustment. Default are second-order Gaussian knockoffs.
		#' @param iters (`integer(1)`: 1) Number of repetitions the `knockoff_fun` is applied to create multiple `x_tilde`
		#' instances per observation.
		initialize = function(
			task,
			# conditioning_set = NULL,
			knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x)),
			iters = 1
		) {
			super$initialize(task)
			self$label = "Knockoff sampler"

			require_package("knockoff")

			# Override param_set to include Knockoff-specific parameters
			self$param_set = paradox::ps(
				# conditioning_set = paradox::p_uty(default = NULL),
				knockoff_fun = paradox::p_uty(
					default = function(x) knockoff::create.second_order(as.matrix(x)),
					custom_check = function(x) {
						if (is.function(x)) TRUE else "knockoff_fun must be a function."
					}
				),
				iters = paradox::p_int(lower = 1, default = 1)
			)

			# Set parameter values
			values_to_set = list()
			values_to_set$knockoff_fun = knockoff_fun
			values_to_set$iters = iters
			self$param_set$set_values(.values = values_to_set)

			# Create knockoff matrix, features only
			# No assertions here on feature types, the user has been warned in the doc
			if (iters == 1) {
				self$x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))
				self$x_tilde[, ..row_id := self$task$row_ids]
			} else {
				self$x_tilde = rbindlist(replicate(
					iters,
					{
						x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))
						x_tilde[, ..row_id := self$task$row_ids]
					},
					simplify = FALSE
				))
			}

			checkmate::assert_subset(colnames(self$x_tilde), c(self$task$feature_names, "..row_id"))
			checkmate::assert_true(nrow(self$x_tilde) == (iters * self$task$nrow))
		},

		#' @description
		#' Sample from stored task using knockoff values. Replaces specified feature(s) with
		#' their knockoff counterparts from the pre-generated knockoff matrix.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @return Modified copy with knockoff feature(s).
		sample = function(
			feature,
			row_ids = NULL
		) {
			if (is.null(row_ids)) {
				row_ids = self$task$row_ids
			}
			data_copy = private$.get_task_data_by_row_id(row_ids)
			# Add row_ids because we need them
			data_copy[, ..row_id := row_ids]
			# Make room for feature(s) from x_tilde
			data_copy[, (feature) := NULL]
			# Add a sequence number within each ..row_id group in data_copy
			# Needed to match multiple instances per row_id if requested
			data_copy[, ..seq_id := seq_len(.N), by = ..row_id]
			# Count occurrences and sample from x_tilde
			# if row_id is requested 4 times but it's present in x_tilde 10 times that must be downsampled
			counts = data_copy[, .N, by = ..row_id]

			# Decide whether to sample from x_tilde with replacement -- only do so if needed
			replace = FALSE
			if (any(counts$N > self$param_set$values$iters)) {
				cli::cli_warn(c(
					"!" = "Some instances requested more often than they are present in generated knockoff matrix",
					i = "Will sample with replacement, so some knockoff values will be duplicated",
					i = "Create {.cls {class(self)[[1]]}} with {.code iters = {max(counts$N)}} or higher to prevent this"
				))
				replace = TRUE
			}

			x_tilde_sampled = self$x_tilde[counts, on = "..row_id", allow.cartesian = TRUE]
			# shuffle and only keep feature(s) from x_tilde to avoid duplicates on join later
			x_tilde_sampled = x_tilde_sampled[,
				.SD[sample(.N, N[1], replace = replace)],
				.SDcols = feature,
				by = ..row_id
			]
			x_tilde_sampled[, ..seq_id := seq_len(.N), by = ..row_id]

			# Inner join on both ..row_id and ..seq_id
			data_copy = data_copy[
				x_tilde_sampled,
				nomatch = 0L,
				on = c("..row_id", "..seq_id")
			]
			# Need to ensure output has matching row ids
			setorderv(data_copy, "..seq_id")
			checkmate::assert_true(all.equal(data_copy[["..row_id"]], row_ids))
			data_copy[, ..row_id := NULL]
			data_copy[, ..seq_id := NULL]

			setcolorder(data_copy, self$task$feature_names)
			data_copy[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]

			# Old / simpler approach doesn't work with duplicates
			# Subsample knockoff DT to match input and selected feature(s)
			# Ensure we get the x_tilde obs in the correct order as the supplied row_ids
			# unlikely to become a bottleneck but could use collapse::fmatch
			# replacements = self$x_tilde[
			# 	match(row_ids, self$x_tilde[["..row_id"]]),
			# 	.SD,
			# 	.SDcols = feature
			# ]
			# data_copy[, (feature) := replacements]
			# data_copy[]
		}
	)
)


#' @title Gaussian Knockoff Conditional Sampler
#'
#' @description
#' A [KnockoffSampler] defaulting to second-order Gaussian knockoffs
#' as created by [knockoff::create.second_order].
#'
#' @details
#' This is equivalent to [KnockoffSampler] using the default `knockoff_fun`.
#'
#' @examplesIf requireNamespace("knockoff", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler
#' sampler = KnockoffGaussianSampler$new(task)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#' @references `r print_bib("watson_2021", "blesch_2023")`
#'
#' @export
KnockoffGaussianSampler = R6Class(
	"KnockoffGaussianSampler",
	inherit = KnockoffSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provied [mlr3::Task] to ensure compatibility.
		feature_types = c(
			"numeric",
			"integer"
		),
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance using Gaussian knockoffs via [knockoff::create.second_order].
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param iters (`integer(1)`: 1) Number of repetitions the `knockoff_fun` is applied to create multiple `x_tilde`
		#' instances per observation.
		initialize = function(
			task,
			iters = 1
		) {
			require_package("knockoff")
			super$initialize(
				task = task,
				knockoff_fun = function(x) {
					knockoff::create.second_order(as.matrix(x))
				},
				iters = iters
			)
			self$label = "Gaussian Knockoff sampler"
		}
	)
)

#' @title Gaussian Knockoff Conditional Sampler
#'
#' @description
#' A [KnockoffSampler] defaulting to second-order Gaussian knockoffs
#' as created by `seqknockoff::knockoffs_seq`.
#'
#' @details
#' This is equivalent to [KnockoffSampler] using `knockoff_fun = seqknockoff::knockoffs_seq`.
#'
#' @examples
#' \dontrun{
#' # Requires seqknockoff (https://github.com/kormama1/seqknockoff)
#' task = tgen("simplex")$generate(n = 100)
#' sampler_seq = KnockoffSampler$new(task)
#' sampled_seq = sampler_seq$sample("x1")
#' }
#' @references `r print_bib("watson_2021", "blesch_2023")`
#'
#' @export
KnockoffSequentialSampler = R6Class(
	"KnockoffSequentialSampler",
	inherit = KnockoffSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provied [mlr3::Task] to ensure compatibility.
		feature_types = c("numeric", "factor"),
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance using sequential knockoffs via `seqknockoff::knockoffs_seq`.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param iters (`integer(1)`: 1) Number of repetitions the `knockoff_fun` is applied to create multiple `x_tilde`
		#' instances per observation.
		initialize = function(
			task,
			iters = 1
		) {
			require_package("seqknockoff", from = "https://github.com/kormama1/seqknockoff")
			super$initialize(
				task = task,
				knockoff_fun = seqknockoff::knockoffs_seq,
				iters = iters
			)
			self$label = "Sequential Knockoff sampler"
		}
	)
)
