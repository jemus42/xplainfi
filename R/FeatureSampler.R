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
		#' @field param_set ([paradox::ParamSet]) Parameter set for the sampler.
		param_set = NULL,

		#' @description
		#' Creates a new instance of the FeatureSampler class
		#' @param task ([mlr3::Task]) Task to sample from
		initialize = function(task) {
			self$task = mlr3::assert_task(task)
			# Initialize empty param_set - subclasses should define their own
			self$param_set = paradox::ps()
		},

		#' @description
		#' Sample values for feature(s) from stored task
		#' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
		#' @param row_ids (`integer()`: `NULL`) Row IDs of the stored [Task][mlr3::Task] to use as basis for sampling.
		#' @return Modified copy of the input data with the feature(s) sampled
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
			cli::cli_end()
		}
	)
)

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
			if (is.null(row_ids)) {
				row_ids = self$task$row_ids
			}
			data_copy = self$task$data(rows = row_ids)

			# Handle both single and multiple features efficiently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]

			data_copy[]
		},
		#' @description
		#' Sample from external data by permutation. See `$sample()` for details.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @return Modified copy with permuted feature(s).
		sample_newdata = function(feature, newdata) {
			# Create a copy to avoid modifying the original data
			data_copy = data.table::copy(newdata)

			# Handle both single and multiple features efficiently
			data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]

			data_copy[]
		}
	)
)

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
		#' @return Modified copy with sampled feature(s).
		sample = function(feature, row_ids, conditioning_set = NULL) {
			cli::cli_abort(c(
				"Abtract method",
				i = "Use a concrete implementation like {.cls ARFSampler}) or {.cls KnockoffSampler}"
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
				"Not implemented.",
				i = "Only some samplers (e.g. {.cls ARFSampler}) support sampling using external data."
			))
		}
	)
)

#' @title ARF-based Conditional Sampler
#'
#' @description Implements conditional sampling using Adversarial Random Forests (ARF).
#' ARF can handle mixed data types (continuous and categorical) and provides
#' flexible conditional sampling by modeling the joint distribution.
#'
#' @details
#' The ARFSampler fits an [Adversarial Random Forest][arf::arf] model on the task data,
#' then uses it to generate samples from \eqn{P(X_j | X_{-j})} where \eqn{X_j} is the
#' feature of interest and \eqn{X_{-j}} are the conditioning features.
#'
#' @examplesIf requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler with default parameters
#' sampler = ARFSampler$new(task, conditioning_set = "x2", verbose = FALSE)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1")
#' # Or use external data
#' data = task$data()
#' sampled_data_ext = sampler$sample_newdata("x1", newdata = data)
#'
#' # Example with custom parameters
#' sampler_custom = ARFSampler$new(task, round = FALSE)
#' sampled_custom = sampler_custom$sample("x1")
#' @references `r print_bib("watson_2023", "blesch_2025")`
#'
#' @export
ARFSampler = R6Class(
	"ARFSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field arf_model Adversarial Random Forest model created by [arf::adversarial_rf].
		arf_model = NULL,
		#' @field psi Distribution parameters estimated from by [arf::forde].
		psi = NULL,

		#' @description
		#' Creates a new instance of the ARFSampler class.
		#' To fit the ARF in parallel, register a parallel backend first (see [arf::arf]) and set `parallel = TRUE`.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
		#' @param num_trees (`integer(1)`: `10L`) Number of trees for ARF. Passed to [arf::adversarial_rf].
		#' @param min_node_size (`integer(1)`: `2L`) Minimum node size for ARF. Passed to [arf::adversarial_rf].
		#' @param finite_bounds (`character(1)`: `"no"`) How to handle variable bounds. Passed to [arf::forde]. Default is `"no"` for compatibility. `"local"` may improve extrapolation but can cause issues with some data.
		#' @param epsilon (`numeric(1)`: `0`) Slack parameter for when `finite_bounds != "no"`. Passed to [arf::forde].
		#' @param round (`logical(1)`: `TRUE`) Whether to round continuous variables back to their original precision in sampling. Can be overridden in `$sample()` calls.
		#' @param stepsize (`numeric(1)`: `0`) Number of rows of evidence to process at a time when `parallel` is `TRUE`. Default (`0`) spreads evidence evenly over registered workers. Can be overridden in `$sample()` calls.
		#' @param verbose (`logical(1)`: `FALSE`) Whether to print progress messages. Default is `FALSE` (arf's default is `TRUE`). Can be overridden in `$sample()` calls.
		#' @param parallel (`logical(1)`: `FALSE`) Whether to use parallel processing via `foreach`. See examples in [arf::forge()]. Can be overridden in `$sample()` calls.
		#' @param ... Additional arguments passed to [arf::adversarial_rf].
		initialize = function(
			task,
			conditioning_set = NULL,
			num_trees = 10L,
			min_node_size = 2L,
			finite_bounds = "no",
			epsilon = 0,
			round = TRUE,
			stepsize = 0,
			verbose = FALSE,
			parallel = FALSE,
			...
		) {
			require_package("arf")
			super$initialize(task)
			self$label = "Adversarial Random Forest sampler"

			# Define param_set with all parameters
			# Sampling parameters (can be overridden in $sample() calls)
			# Model fitting parameters (only used during initialization)
			self$param_set = paradox::ps(
				# Sampling parameters (stored for hierarchical resolution in $sample())
				conditioning_set = paradox::p_uty(default = NULL),
				round = paradox::p_lgl(default = TRUE),
				stepsize = paradox::p_dbl(lower = 0, default = 0),
				verbose = paradox::p_lgl(default = FALSE),
				parallel = paradox::p_lgl(default = FALSE),
				# Model fitting parameters (used only during initialization)
				num_trees = paradox::p_int(lower = 1L, default = 10L),
				min_node_size = paradox::p_int(lower = 1L, default = 2L),
				finite_bounds = paradox::p_fct(c("no", "local", "global"), default = "no"),
				epsilon = paradox::p_dbl(lower = 0, default = 0)
			)

			# Set parameter values
			values_to_set = list(
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel,
				num_trees = num_trees,
				min_node_size = min_node_size,
				finite_bounds = finite_bounds,
				epsilon = epsilon
			)
			if (!is.null(conditioning_set)) {
				values_to_set$conditioning_set = conditioning_set
			}

			self$param_set$set_values(.values = values_to_set)

			# Register sequential backend in an attempt to silence foreach warning
			if (!foreach::getDoParRegistered() & !parallel) {
				foreach::registerDoSEQ()
			}

			# Fit ARF model on the task data, features only
			task_data = self$task$data(cols = self$task$feature_names)

			# Train ARF and estimate distribution parameters
			self$arf_model = arf::adversarial_rf(
				x = task_data,
				num_trees = num_trees,
				min_node_size = min_node_size,
				verbose = verbose,
				parallel = parallel,
				replace = FALSE,
				...
			)
			self$psi = arf::forde(
				arf = self$arf_model,
				x = task_data,
				finite_bounds = finite_bounds,
				epsilon = epsilon,
				parallel = parallel
			)
		},

		#' @description
		#' Sample from stored task. Parameters `conditioning_set`, `round`, `stepsize`, `verbose`, and `parallel`
		#' use hierarchical resolution: function argument > stored `param_set` value > hard-coded default.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param row_ids (`integer()` | `NULL`) Row IDs to use. If `NULL`, uses all rows.
		#' @param conditioning_set (`character` | `NULL`) Features to condition on.
		#' @param round (`logical(1)` | `NULL`) Round continuous variables.
		#' @param stepsize (`numeric(1)` | `NULL`) Batch size for parallel processing.
		#' @param verbose (`logical(1)` | `NULL`) Print progress messages.
		#' @param parallel (`logical(1)` | `NULL`) Use parallel processing.
		#' @return Modified copy with sampled feature(s).
		sample = function(
			feature,
			row_ids = NULL,
			conditioning_set = NULL,
			round = NULL,
			stepsize = NULL,
			verbose = NULL,
			parallel = NULL
		) {
			if (is.null(row_ids)) {
				row_ids = self$task$row_ids
			}
			data_copy = self$task$data(rows = row_ids)

			private$.sample_arf(
				feature = feature,
				data = data_copy,
				conditioning_set = conditioning_set,
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel
			)
		},
		#' @description
		#' Sample from external data (e.g., test set). See `$sample()` for parameter details.
		#' @param feature (`character`) Feature(s) to sample.
		#' @param newdata ([`data.table`][data.table::data.table]) External data to use.
		#' @param conditioning_set (`character` | `NULL`) Features to condition on.
		#' @param round (`logical(1)` | `NULL`) Round continuous variables.
		#' @param stepsize (`numeric(1)` | `NULL`) Batch size for parallel processing.
		#' @param verbose (`logical(1)` | `NULL`) Print progress messages.
		#' @param parallel (`logical(1)` | `NULL`) Use parallel processing.
		#' @return Modified copy with sampled feature(s).
		sample_newdata = function(
			feature,
			newdata,
			conditioning_set = NULL,
			round = NULL,
			stepsize = NULL,
			verbose = NULL,
			parallel = NULL
		) {
			data_copy = data.table::copy(newdata)

			private$.sample_arf(
				feature = feature,
				data = data_copy,
				conditioning_set = conditioning_set,
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel
			)
		}
	),
	private = list(
		.sample_arf = function(
			feature,
			data,
			conditioning_set = NULL,
			round = NULL,
			stepsize = NULL,
			verbose = NULL,
			parallel = NULL
		) {
			# Determine conditioning set
			# Priority: 1) function argument, 2) stored param_set value, 3) default (all other features)
			conditioning_set = resolve_param(
				conditioning_set,
				self$param_set$values$conditioning_set,
				setdiff(self$task$feature_names, feature)
			)

			# Determine arf::forge parameters using hierarchical resolution
			round = resolve_param(round, self$param_set$values$round, TRUE)
			stepsize = resolve_param(stepsize, self$param_set$values$stepsize, 0)
			verbose = resolve_param(verbose, self$param_set$values$verbose, FALSE)
			parallel = resolve_param(parallel, self$param_set$values$parallel, FALSE)

			# Create evidence data frame with conditioning set for all rows
			# Handle empty conditioning set by passing NULL to arf::forge()
			if (length(conditioning_set) == 0) {
				# Equivalent (ish) to marginal permutation
				if (xplain_opt("debug")) {
					cli::cli_alert_info(
						"{.val conditioning_set} is length 0, passing {.code evidence = NULL} to {.fun arf::forge}"
					)
				}
				evidence = NULL
			} else {
				evidence = data[, .SD, .SDcols = conditioning_set]
			}

			if (xplain_opt("debug")) {
				cli::cli_inform(c(
					i = "Feature is {.val {feature}}",
					i = "Conditioning set is {.val {conditioning_set}}"
				))
			}
			# Generate conditional samples
			synthetic = arf::forge(
				params = self$psi,
				n_synth = 1L, # would be nrow(data) for evidence_row_mode = "or"
				evidence = evidence,
				evidence_row_mode = "separate",
				round = round,
				sample_NAs = FALSE,
				nomatch = "force",
				verbose = verbose,
				stepsize = stepsize,
				parallel = parallel
			)

			# Replace the feature(s) with sampled values using .SDcols pattern
			# Both "separate" and "or" modes now return exactly nrow(data) samples
			data[, (feature) := synthetic[, .SD, .SDcols = feature]]

			data[]
		}
	)
)

#' @title Knockoff-based Conditional Sampler
#'
#' @description Implements conditional sampling using Knockoffs.
#'
#' @details
#' The KnockoffSampler samples [Knockoffs][knockoff::knockoff] based on the task data.
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
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance of the KnockoffSampler class.
		#' @param task ([mlr3::Task]) Task to sample from
		# @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
		#' @param knockoff_fun (`function`) Step size for variance adjustment. Default are second-order Gaussian knockoffs.
		initialize = function(
			task,
			# conditioning_set = NULL,
			knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x))
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
				)
			)

			# Set parameter values
			values_to_set = list()
			# if (!is.null(conditioning_set)) {
			#   values_to_set$conditioning_set = conditioning_set
			# }
			values_to_set$knockoff_fun = knockoff_fun

			self$param_set$set_values(.values = values_to_set)

			# Create knockoff matrix, features only
			# TODO: Needs assertion on feature types but depends on knockoff_fun
			self$x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))

			checkmate::assert_subset(colnames(self$x_tilde), self$task$feature_names)
			checkmate::assert_true(nrow(self$x_tilde) == self$task$nrow)
			self$x_tilde[, ..row_id := self$task$row_ids]
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
			data_copy = self$task$data(rows = row_ids)

			# Subsample knockoff DT to match input and selected feature(s)
			# Ensure we get the x_tilde obs in the correct order as the supplied row_ids
			# unlikely to become a bottleneck but could use collapse::fmatch
			replacements = self$x_tilde[
				match(row_ids, self$x_tilde[["..row_id"]]),
				.SD,
				.SDcols = feature
			]
			data_copy[, (feature) := replacements]
			data_copy[]
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
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance using Gaussian knockoffs via [knockoff::create.second_order].
		#' @param task ([mlr3::Task]) Task to sample from.
		initialize = function(
			task
		) {
			require_package("knockoff")
			super$initialize(
				task = task,
				knockoff_fun = function(x) {
					knockoff::create.second_order(as.matrix(x))
				}
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
		#' @field x_tilde Knockoff matrix
		x_tilde = NULL,

		#' @description
		#' Creates a new instance using sequential knockoffs via [seqknockoff::knockoffs_seq].
		#' @param task ([mlr3::Task]) Task to sample from.
		initialize = function(
			task
		) {
			require_package("seqknockoff", from = "https://github.com/kormama1/seqknockoff")
			super$initialize(
				task = task,
				knockoff_fun = seqknockoff::knockoffs_seq
			)
			self$label = "Sequential Knockoff sampler"
		}
	)
)
