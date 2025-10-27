#' @title ARF-based Conditional Sampler
#'
#' @description Implements conditional sampling using Adversarial Random Forests (ARF).
#' ARF can handle mixed data types (continuous and categorical) and provides
#' flexible conditional sampling by modeling the joint distribution.
#'
#' @details
#' The ConditionalARFSampler fits an [Adversarial Random Forest][arf::arf] model on the task data,
#' then uses it to generate samples from \eqn{P(X_j | X_{-j})} where \eqn{X_j} is the
#' feature of interest and \eqn{X_{-j}} are the conditioning features.
#'
#' @examplesIf requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' # Create sampler with default parameters
#' sampler = ConditionalARFSampler$new(task, conditioning_set = "x2", verbose = FALSE)
#' # Sample using row_ids from stored task
#' sampled_data = sampler$sample("x1", row_ids = 1:10)
#' # Or use external data
#' data = task$data()
#' sampled_data_ext = sampler$sample_newdata("x1", newdata = data, conditioning_set = "x2")
#'
#' # Example with custom ARF parameters
#' sampler_custom = ConditionalARFSampler$new(
#'   task,
#'   min_node_size = 10L,
#'   finite_bounds = "local",
#'   verbose = FALSE
#' )
#' sampled_custom = sampler_custom$sample("x1", conditioning_set = "x2")
#' @references `r print_bib("watson_2023", "blesch_2025")`
#'
#' @export
ConditionalARFSampler = R6Class(
	"ConditionalARFSampler",
	inherit = ConditionalSampler,
	public = list(
		#' @field feature_types (`character()`) Feature types supported by the sampler.
		#'   Will be checked against the provied [mlr3::Task] to ensure compatibility.
		feature_types = c(
			"numeric",
			"factor",
			"ordered",
			"integer",
			"logical",
			"character"
		),
		#' @field arf_model Adversarial Random Forest model created by [arf::adversarial_rf].
		arf_model = NULL,
		#' @field psi Distribution parameters estimated from by [arf::forde].
		psi = NULL,

		#' @description
		#' Creates a new instance of the ConditionalARFSampler class.
		#' To fit the ARF in parallel, register a parallel backend first (see [arf::arf]) and set `parallel = TRUE`.
		#' @param task ([mlr3::Task]) Task to sample from.
		#' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
		#' @param num_trees (`integer(1)`: `10L`) Number of trees for ARF. Passed to [arf::adversarial_rf].
		#' @param min_node_size (`integer(1)`: `20L`) Minimum node size for ARF. Passed to [arf::adversarial_rf] and in turn to [ranger::ranger].
		#'   This is increased to 20 to mitigate overfitting.
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
			min_node_size = 20L,
			finite_bounds = "no",
			epsilon = 1e-15,
			round = TRUE,
			stepsize = 0,
			verbose = FALSE,
			parallel = FALSE,
			...
		) {
			require_package("arf")
			super$initialize(task, conditioning_set = conditioning_set)
			self$label = "Adversarial Random Forest sampler"

			# Extend param_set with ARF-specific parameters
			# Sampling parameters (can be overridden in $sample() calls)
			# Model fitting parameters (only used during initialization)
			self$param_set = c(
				self$param_set,
				paradox::ps(
					# Sampling parameters (stored for hierarchical resolution in $sample())
					round = paradox::p_lgl(default = TRUE),
					stepsize = paradox::p_dbl(lower = 0, default = 0),
					verbose = paradox::p_lgl(default = FALSE),
					parallel = paradox::p_lgl(default = FALSE),
					# Model fitting parameters (used only during initialization)
					num_trees = paradox::p_int(lower = 1L, default = 10L),
					min_node_size = paradox::p_int(lower = 1L, default = 20L),
					finite_bounds = paradox::p_fct(c("no", "local", "global"), default = "no"),
					epsilon = paradox::p_dbl(lower = 0, default = 1e-15)
				)
			)

			# Set parameter values
			self$param_set$set_values(
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel,
				num_trees = num_trees,
				min_node_size = min_node_size,
				finite_bounds = finite_bounds,
				epsilon = epsilon
			)

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
		#' Sample from stored task. Parameters use hierarchical resolution:
		#' function argument > stored `param_set` value > hard-coded default.
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
			super$sample(
				feature,
				row_ids,
				conditioning_set,
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel
			)
		},

		#' @description
		#' Sample from external data. See `$sample()` for parameter details.
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
			super$sample_newdata(
				feature,
				newdata,
				conditioning_set,
				round = round,
				stepsize = stepsize,
				verbose = verbose,
				parallel = parallel
			)
		}
	),
	private = list(
		# Core sampling logic implementing ARF conditional sampling
		# Called by $sample() and $sample_newdata() which pass ARF-specific parameters
		.sample_conditional = function(
			data,
			feature,
			conditioning_set = NULL,
			round = NULL,
			stepsize = NULL,
			verbose = NULL,
			parallel = NULL
		) {
			# Determine arf::forge parameters using hierarchical resolution
			round = resolve_param(round, self$param_set$values$round, TRUE)
			stepsize = resolve_param(stepsize, self$param_set$values$stepsize, 0)
			verbose = resolve_param(verbose, self$param_set$values$verbose, FALSE)
			parallel = resolve_param(parallel, self$param_set$values$parallel, FALSE)

			# Create evidence data frame with conditioning set for all rows
			# Handle empty conditioning set by passing NULL to arf::forge()
			if (length(conditioning_set) == 0) {
				# Equivalent (ish) to marginal sampling
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

			# Return in order of original task
			data[, .SD, .SDcols = c(self$task$target_names, self$task$feature_names)]
		}
	)
)
