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

    #' @description
    #' Creates a new instance of the FeatureSampler class
    #' @param task ([mlr3::Task]) Task to sample from
    initialize = function(task) {
      self$task = mlr3::assert_task(task)
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
    },

    #' @description
    #' Sample values for feature(s) conditionally on other features
    #' @param feature (`character`) Feature name(s) to sample (can be single or multiple)
    #' @param data ([`data.table`][data.table::data.table] ) Data containing conditioning features
    #' @param conditioning_features ([character]) Features to condition on (default: all other features)
    #' @return Modified copy of the input data with the feature(s) sampled conditionally
    sample = function(feature, data, conditioning_features = NULL) {
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
#' @references
#' - Watson, D.S., Blesch, K., Kapar, J. & Wright, M.N.. (2023). Adversarial Random Forests for Density Estimation and Generative Modeling. Proceedings of The 26th International Conference on Artificial Intelligence and Statistics, in Proceedings of Machine Learning Research 206:5357-5375 Available from <https://proceedings.mlr.press/v206/watson23a.html>.
#' - Blesch, K., Koenen, N., Kapar, J., Golchian, P., Burk, L., Loecher, M. & Wright, M. N. (2025). Conditional feature importance with generative modeling using adversarial random forests. In Proceedings of the 39th AAAI Conference on Artificial Intelligence. Available from <https://arxiv.org/abs/2501.11178>
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
    #' @param arf_args,forde_args ([list]) Arguments passed to `arf::adversarial_rf` or `arf::forde` respectively.
    initialize = function(task, arf_args = NULL, forde_args = NULL) {
      super$initialize(task)
      self$label = "Adversarial Random Forest sampler"

      if (!requireNamespace("arf", quietly = TRUE)) {
        stop(
          "Package 'arf' is required for ARFSampler. Please install it with: install.packages('arf')"
        )
      }

      # Fit ARF model on the task data, features only
      task_data = self$task$data(cols = self$task$feature_names)

      # Train ARF and estimate distribution parameters
      # Default to verbose = FALSE if not provided
      arf_args$verbose %||% FALSE

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
    #' @param conditioning_features (`character(n) | NULL`) Features to condition on (default: all other features)
    #' @param n_synth (`1`) Number of samples to generate (per row of evidence, when `evidence_row_mode = "separate"`). See `arf::forge()`.
    #' @param evidence_row_mode (`"separate"`) Produce `n_synth` sample per row of evidence. See `arf::forge()`
    #' @param ... Further arguments passed to `arf::forge()`.
    #' @return Modified copy of the input data with the feature(s) sampled conditionally
    sample = function(
      feature,
      data = self$task$data(),
      conditioning_features = NULL,
      n_synth = 1,
      evidence_row_mode = "separate",
      ...
    ) {
      # Create a copy to avoid modifying the original data
      data_copy = data.table::copy(data)

      # If conditioning_features not specified, use all features except the one of interest
      if (is.null(conditioning_features)) {
        conditioning_features = setdiff(self$task$feature_names, feature)
      }

      # Create evidence data frame with conditioning features for all rows
      evidence = data[, ..conditioning_features]

      # Generate conditional samples - one sample per evidence row
      synthetic = arf::forge(
        params = self$psi,
        n_synth = n_synth,
        evidence = evidence,
        evidence_row_mode = evidence_row_mode,
        ...
      )

      # Replace the feature with sampled values
      data_copy[, (feature) := synthetic[[feature]]]

      data_copy[]
    }
  )
)
