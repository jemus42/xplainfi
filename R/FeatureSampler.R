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
    #' Sample values for feature(s)
    #' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table] ) Data to use for sampling context
    #' @return Modified copy of the input data with the feature(s) sampled
    sample = function(feature, data) {
      stop("Abstract method. Use a concrete implementation.")
    }
  )
)

#' @title Marginal Feature Sampler
#'
#' @description Implements marginal sampling for PFI, where the feature of interest
#' is sampled independently of other features
#'
#' @examples
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' sampler = MarginalSampler$new(task)
#' data = task$data()
#' sampled_data = sampler$sample("x1", data)
#' @export
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
    #' Sample values for feature(s) by permutation (marginal distribution)
    #' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table] ) Data to permute the feature(s) in
    #' @return Modified copy of the input data with the feature(s) permuted
    sample = function(feature, data) {
      # Create a copy to avoid modifying the original data
      data_copy = data.table::copy(data)

      # Handle both single and multiple features efficiently
      data_copy[, (feature) := lapply(.SD, sample), .SDcols = feature]

      data_copy[]
    },

    #' @description
    #' Print sampler
    #'
    #' @param ... Passed to `print()`
    print = function(...) {
      cli::cli_h2(self$label)
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
    #' Sample values for feature(s) conditionally on other features
    #' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table] ) Data containing conditioning features
    #' @param conditioning_set ([character]) Features to condition on (default: all other features)
    #' @return Modified copy of the input data with the feature(s) sampled conditionally
    sample = function(feature, data, conditioning_set = NULL) {
      stop("Abstract method. Use a concrete implementation like ARFSampler.")
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
#' data = task$data()
#' # Will use the stored parameters
#' sampled_data = sampler$sample("x1", data)
#'
#' # Example with "or" mode and custom parameters
#' sampler_or = ARFSampler$new(task, evidence_row_mode = "or", round = FALSE)
#' sampled_or = sampler_or$sample("x1", data)
#' @references `r print_bib("watson_2023", "blesch_2025")`
#'
#' @export
ARFSampler = R6Class(
  "ARFSampler",
  inherit = ConditionalSampler,
  public = list(
    #' @field arf_model Adversarial Random Forest model
    arf_model = NULL,
    #' @field psi Distribution parameters estimated from ARF
    psi = NULL,

    #' @description
    #' Creates a new instance of the ARFSampler class.
    #' To fit the ARF in parallel, set `arf_args = list(parallel = TRUE)` and register a parallel backend (see [arf::arf]).
    #' @param task ([mlr3::Task]) Task to sample from
    #' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
    #' @param evidence_row_mode (`character(1)`) Evidence row mode for `arf::forge()`. Default is "separate".
    #' @param round (`logical(1)`) Whether to round continuous variables. Default is `TRUE`.
    #' @param sample_NAs (`logical(1)`) Whether to sample missing values. Default is `FALSE`.
    #' @param nomatch (`character(1)`) How to handle factor levels not seen in training. Default is "force".
    #' @param stepsize (`numeric(1)`) Step size for variance adjustment. Default is 0.
    #' @param verbose (`logical(1)`) Whether to print progress messages. Default is `TRUE`.
    #' @param parallel (`logical(1)`) Whether to use parallel processing. Default is `TRUE`.
    #' @param arf_args,forde_args ([list]) Arguments passed to `arf::adversarial_rf` or `arf::forde` respectively.
    initialize = function(
      task,
      conditioning_set = NULL,
      evidence_row_mode = "separate",
      round = TRUE,
      sample_NAs = FALSE,
      nomatch = "force",
      stepsize = 0,
      verbose = TRUE,
      parallel = TRUE,
      arf_args = NULL,
      forde_args = NULL
    ) {
      super$initialize(task)
      self$label = "Adversarial Random Forest sampler"

      if (!requireNamespace("arf", quietly = TRUE)) {
        stop(
          "Package 'arf' is required for ARFSampler. Please install it with: install.packages('arf')"
        )
      }

      # Override param_set to include ARF-specific parameters
      self$param_set = paradox::ps(
        conditioning_set = paradox::p_uty(default = NULL),
        evidence_row_mode = paradox::p_fct(levels = c("separate", "or"), default = "separate"),
        round = paradox::p_lgl(default = TRUE),
        sample_NAs = paradox::p_lgl(default = FALSE),
        nomatch = paradox::p_fct(levels = c("force", "na"), default = "force"),
        stepsize = paradox::p_dbl(lower = 0, default = 0),
        verbose = paradox::p_lgl(default = TRUE),
        parallel = paradox::p_lgl(default = TRUE)
      )

      # Set parameter values
      values_to_set = list()
      if (!is.null(conditioning_set)) {
        values_to_set$conditioning_set = conditioning_set
      }
      values_to_set$evidence_row_mode = evidence_row_mode
      values_to_set$round = round
      values_to_set$sample_NAs = sample_NAs
      values_to_set$nomatch = nomatch
      values_to_set$stepsize = stepsize
      values_to_set$verbose = verbose
      values_to_set$parallel = parallel

      self$param_set$set_values(.values = values_to_set)

      # Register sequential backend in an attempt to silence foreach warning
      if (!foreach::getDoParRegistered()) {
        foreach::registerDoSEQ()
      }

      # Fit ARF model on the task data, features only
      task_data = self$task$data(cols = self$task$feature_names)

      # Train ARF and estimate distribution parameters
      arf_args$verbose = arf_args$verbose %||% verbose

      self$arf_model = do.call(arf::adversarial_rf, args = c(x = list(task_data), arf_args))
      self$psi = do.call(
        arf::forde,
        args = c(arf = list(self$arf_model), x = list(task_data), forde_args)
      )
    },

    #' @description
    #' Sample values for feature(s) conditionally on other features using ARF
    #' @param feature (`character`) Feature(s) of interest to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table]) Data containing conditioning features. Defaults to `$task$data()`, but typically a dedicated test set is provided.
    #' @param conditioning_set (`character(n) | NULL`) Features to condition on. If `NULL`, uses the stored parameter if available, otherwise defaults to all other features.
    #' @param evidence_row_mode (`character(1) | NULL`) Evidence row mode for `arf::forge()`. If `NULL`, uses the stored parameter value. Must be `"separate"` or `"or"`.
    #' @param round (`logical(1) | NULL`) Whether to round continuous variables. If `NULL`, uses the stored parameter value.
    #' @param sample_NAs (`logical(1) | NULL`) Whether to sample missing values. If `NULL`, uses the stored parameter value.
    #' @param nomatch (`character(1) | NULL`) How to handle factor levels not seen in training. If `NULL`, uses the stored parameter value.
    #' @param stepsize (`numeric(1) | NULL`) Step size for variance adjustment. If `NULL`, uses the stored parameter value.
    #' @param verbose (`logical(1) | NULL`) Whether to print progress messages. If `NULL`, uses the stored parameter value.
    #' @param parallel (`logical(1) | NULL`) Whether to use parallel processing. If `NULL`, uses the stored parameter value.
    #' @param ... Further arguments passed to `arf::forge()`.
    #' @return Modified copy of the input data with the feature(s) sampled conditionally
    sample = function(
      feature,
      data = self$task$data(),
      conditioning_set = NULL,
      evidence_row_mode = NULL,
      round = NULL,
      sample_NAs = NULL,
      nomatch = NULL,
      stepsize = NULL,
      verbose = NULL,
      parallel = NULL,
      ...
    ) {
      # Create a copy to avoid modifying the original data
      data_copy = data.table::copy(data)

      # Determine conditioning set
      # Priority: 1) function argument, 2) stored param_set value, 3) default (all other features)
      conditioning_set = resolve_param(
        conditioning_set,
        self$param_set$values$conditioning_set,
        setdiff(self$task$feature_names, feature)
      )

      # Determine evidence_row_mode
      evidence_row_mode = resolve_param(
        evidence_row_mode,
        self$param_set$values$evidence_row_mode,
        "separate"
      )

      # Determine arf::forge parameters using hierarchical resolution
      round = resolve_param(round, self$param_set$values$round, TRUE)
      sample_NAs = resolve_param(sample_NAs, self$param_set$values$sample_NAs, FALSE)
      nomatch = resolve_param(nomatch, self$param_set$values$nomatch, "force")
      stepsize = resolve_param(stepsize, self$param_set$values$stepsize, 0)
      verbose = resolve_param(verbose, self$param_set$values$verbose, TRUE)
      parallel = resolve_param(parallel, self$param_set$values$parallel, TRUE)

      # Set n_synth based on evidence_row_mode and data dimensions
      # This ensures we always get exactly nrow(data) samples
      if (evidence_row_mode == "separate") {
        # In separate mode: 1 sample per evidence row = nrow(data) total samples
        n_synth = 1L
      } else {
        # In "or" mode: nrow(data) total samples across all evidence
        n_synth = nrow(data)
      }

      # Create evidence data frame with conditioning set for all rows
      # Handle empty conditioning set by passing NULL to arf::forge()
      if (length(conditioning_set) == 0) {
        evidence = NULL
      } else {
        evidence = data[, .SD, .SDcols = conditioning_set]
      }

      # Generate conditional samples
      synthetic = arf::forge(
        params = self$psi,
        n_synth = n_synth,
        evidence = evidence,
        evidence_row_mode = evidence_row_mode,
        round = round,
        sample_NAs = sample_NAs,
        nomatch = nomatch,
        verbose = verbose,
        stepsize = stepsize,
        parallel = parallel,
        ...
      )

      # Replace the feature(s) with sampled values using .SDcols pattern
      # Both "separate" and "or" modes now return exactly nrow(data) samples
      data_copy[, (feature) := synthetic[, .SD, .SDcols = feature]]

      data_copy[]
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
#' sampler = KnockoffSampler$new(task, conditioning_set = "x2")
#' # Will use the stored parameters
#' sampled_data = sampler$sample("x1")
#'
#' # Example with sequential knockoffs
#' package available here: https://github.com/kormama1/seqknockoff
#' task = tgen("simplex")$generate(n = 100)
#' sampler_seq = KnockoffSampler$new(task, knockoff_fun = seqknockoff::knockoffs_seq)
#' sampled_seq = sampler_seq$sample("x1")
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
    #' @param conditioning_set (`character` | `NULL`) Default conditioning set to use in `$sample()`. This parameter only affects the sampling behavior, not the ARF model fitting.
    #' @param knockoff_fun (`function`) Step size for variance adjustment. Default are second-order Gaussian knockoffs.
    initialize = function(
    task,
    conditioning_set = NULL,
    knockoff_fun = function(x) knockoff::create.second_order(as.matrix(x))
    ) {
      super$initialize(task)
      self$label = "Knockoff sampler"

      if (!requireNamespace("knockoff", quietly = TRUE)) {
        stop(
          "Package 'knockoff' is required for KnockoffSampler. Please install it with: install.packages('knockoff')"
        )
      }

      # Override param_set to include Knockoff-specific parameters
      self$param_set = paradox::ps(
        conditioning_set = paradox::p_uty(default = NULL),
        knockoff_fun = paradox::p_uty(default = function(x) knockoff::create.second_order(as.matrix(x)),
                                      custom_check = function(x) if (is.function(x)) TRUE else "knockoff_fun must be a function.")
      )

      # Set parameter values
      values_to_set = list()
      if (!is.null(conditioning_set)) {
        values_to_set$conditioning_set = conditioning_set
      }
      values_to_set$knockoff_fun = knockoff_fun

      self$param_set$set_values(.values = values_to_set)

      # Create knockoff matrix, features only
      self$x_tilde = as.data.table(knockoff_fun(self$task$data(cols = self$task$feature_names)))
    },

    #' @description
    #' Sample values for feature(s) conditionally on other features using Knockoffs
    #' @param feature (`character`) Feature(s) of interest to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table]) Data containing conditioning features. Defaults to `$task$data()`, but typically a dedicated test set is provided.
    #' @return Modified copy of the input data with the feature(s) sampled conditionally
    sample = function(
    feature,
    data = self$task$data()
    ) {
      # Create a copy to avoid modifying the original data
      data_copy = data.table::copy(data)

      # Replace the feature(s) with knockoffs
      data_copy[, (feature) := self$x_tilde[, .SD, .SDcols = feature]]

      data_copy[]
    }
  )
)
