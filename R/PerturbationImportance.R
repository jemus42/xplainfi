#' @title Perturbation Feature Importance Base Class
#'
#' @description Abstract base class for perturbation-based importance methods PFI, CFI, and RFI
#'
#' @export
PerturbationImportance = R6Class(
  "PerturbationImportance",
  inherit = FeatureImportanceMethod, # Inherit from existing base class
  public = list(
    #' @field sampler ([FeatureSampler]) Sampler object for feature perturbation
    sampler = NULL,

    #' @description
    #' Creates a new instance of the PerturbationImportance class
    #' @param task,learner,measure,resampling,features Passed to FeatureImportanceMethod
    #' @param sampler ([FeatureSampler]) Sampler to use for feature perturbation
    #' @param relation (character(1)) How to relate perturbed scores to originals. Can be overridden in `$compute()`.
    #' @param iters_perm (integer(1)) Number of permutation iterations. Can be overridden in `$compute()`.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      sampler = NULL,
      relation = "difference",
      iters_perm = 1L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        label = "Feature Importance (Abstract Class)"
      )

      # If no sampler is provided, create a default one (implementation dependent)
      self$sampler = sampler

      # Set up common parameters for all perturbation-based methods
      ps = paradox::ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1)
      )

      ps$values$relation = relation
      ps$values$iters_perm = iters_perm

      self$param_set = ps
    }
  ),

  private = list(
    # Common computation method for all perturbation-based methods
    .compute_perturbation_importance = function(
      relation = NULL,
      iters_perm = NULL,
      store_backends = TRUE,
      sampler = NULL
    ) {
      # Use provided sampler or default to self$sampler
      sampler = sampler %||% self$sampler

      # Use hierarchical parameter resolution
      relation = resolve_param(relation, self$param_set$values$relation, "difference")
      iters_perm = resolve_param(iters_perm, self$param_set$values$iters_perm, 1L)

      relation = match.arg(relation, c("difference", "ratio"))

      # Check if already computed with this relation
      if (!is.null(self$importance) & self$param_set$values$relation == relation) {
        return(self$importance)
      }

      # Store relation
      self$param_set$values$relation = relation

      # Initial resampling
      rr = resample(
        self$task,
        self$learner,
        self$resampling,
        store_models = TRUE,
        store_backends = store_backends
      )

      scores_orig = rr$score(self$measure)[, .SD, .SDcols = c("iteration", self$measure$id)]
      setnames(scores_orig, old = self$measure$id, "scores_pre")

      # Compute permuted scores
      scores = lapply(seq_len(self$resampling$iters), \(iter) {
        test_dt = self$task$data(rows = rr$resampling$test_set(iter))

        rbindlist(
          lapply(seq_len(iters_perm), \(iter_perm) {
            scores_post = vapply(
              self$features,
              \(feature) {
                # Sample feature - sampler handles conditioning appropriately
                perturbed_data = sampler$sample(feature, test_dt)

                # Predict and score
                pred = rr$learners[[iter]]$predict_newdata(
                  newdata = perturbed_data,
                  task = self$task
                )
                score = pred$score(self$measure)
                names(score) = feature
                score
              },
              FUN.VALUE = numeric(1)
            )

            data.table(
              feature = names(scores_post),
              iter_perm = iter_perm,
              scores_post = unname(scores_post)
            )
          })
        )
      })

      # Collect permuted scores, add original scores
      scores = rbindlist(scores, idcol = "iteration")
      scores = scores[scores_orig, on = "iteration"]
      setcolorder(scores, c("feature", "iteration", "iter_perm", "scores_pre", "scores_post"))

      # Calculate importance depending on relation
      scores[,
        importance := compute_score(
          scores_pre,
          scores_post,
          relation = self$param_set$values$relation,
          minimize = self$measure$minimize
        )
      ]

      # Rename columns for clarity
      setnames(
        scores,
        old = c("iteration", "scores_pre", "scores_post"),
        new = c("iter_rsmp", paste0(self$measure$id, c("_orig", "_perm")))
      )

      setkeyv(scores, c("feature", "iter_rsmp"))

      # Aggregate by feature
      scores_agg = private$.aggregate_importances(scores)

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      copy(self$importance)
    }
  )
)


#' @title Permutation Feature Importance
#'
#' @description
#' Implementation of Permutation Feature Importance (PFI) using modular sampling approach.
#' PFI measures the importance of a feature by calculating the increase in model error
#' when the feature's values are randomly permuted, breaking the relationship between
#' the feature and the target variable.
#'
#' @details
#' Permutation Feature Importance was originally introduced by Breiman (2001) as part of
#' the Random Forest algorithm. The method works by:
#' 1. Computing baseline model performance on the original dataset
#' 2. For each feature, randomly permuting its values while keeping other features unchanged
#' 3. Computing model performance on the permuted dataset
#' 4. Calculating importance as the difference (or ratio) between permuted and original performance
#'
#' @references
#' `r print_bib("breiman_2001")`
#' `r print_bib("fisher_2019")`
#' `r print_bib("strobl_2008")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3learners)
#' task = tgen("xor", d = 5)$generate(n = 100)
#' pfi = PFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce"),
#'   resampling = rsmp("cv", folds = 3),
#'   iters_perm = 3
#' )
#' pfi$compute()
#' @export
PFI = R6Class(
  "PFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the PFI class
    #' @param task,learner,measure,resampling,features Passed to PerturbationImportance
    #' @param relation (character(1)) How to relate perturbed scores to originals. Can be overridden in `$compute()`.
    #' @param iters_perm (integer(1)) Number of permutation iterations. Can be overridden in `$compute()`.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      relation = "difference",
      iters_perm = 1L
    ) {
      # Create a marginal sampler for PFI
      sampler = MarginalSampler$new(task)

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        sampler = sampler,
        relation = relation,
        iters_perm = iters_perm
      )

      self$label = "Permutation Feature Importance"
    },

    #' @description
    #' Compute PFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals. If `NULL`, uses stored value.
    #' @param iters_perm (integer(1)) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(relation = NULL, iters_perm = NULL, store_backends = TRUE) {
      # PFI uses the MarginalSampler directly
      private$.compute_perturbation_importance(
        relation = relation,
        iters_perm = iters_perm,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  )
)

#' @title Conditional Feature Importance
#'
#' @description Implementation of CFI using modular sampling approach
#'
#' @references `r print_bib("blesch_2025")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#' cfi = CFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce")
#' )
#' cfi$compute()
#' @export
CFI = R6Class(
  "CFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the CFI class
    #' @param task,learner,measure,resampling,features Passed to `PerturbationImportance`.
    #' @param relation (character(1)) How to relate perturbed scores to originals. Can be overridden in `$compute()`.
    #' @param iters_perm (integer(1)) Number of sampling iterations. Can be overridden in `$compute()`.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to instantiationg `ARFSampler` internally with default parameters.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      relation = "difference",
      iters_perm = 1L,
      sampler = NULL
    ) {
      # Use ARFSampler by default for CFI
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        sampler = sampler,
        relation = relation,
        iters_perm = iters_perm
      )

      self$label = "Conditional Feature Importance"
    },

    #' @description
    #' Compute CFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals. If `NULL`, uses stored value.
    #' @param iters_perm (integer(1)) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(relation = NULL, iters_perm = NULL, store_backends = TRUE) {
      # CFI expects sampler configured to condition on all other features for each feature
      # Default for ARFSampler
      # TODO: Needs more rigorous approach
      private$.compute_perturbation_importance(
        relation = relation,
        iters_perm = iters_perm,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  )
)

#' @title Relative Feature Importance
#'
#' @description Implementation of RFI using modular sampling approach
#'
#' @references `r print_bib("konig_2021")`
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE) && requireNamespace("arf", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 200)
#' rfi = RFI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse"),
#'   conditioning_set = c("important1")
#' )
#' rfi$compute()
#' @export
RFI = R6Class(
  "RFI",
  inherit = PerturbationImportance,
  public = list(
    #' @description
    #' Creates a new instance of the RFI class
    #' @param task,learner,measure,resampling,features Passed to PerturbationImportance
    #' @param conditioning_set ([character()]) Set of features to condition on. Can be overridden in `$compute()`.
    #'   Default (`character(0)`) is equivalent to `PFI`. In `CFI`, this would be set to all features except tat of interest.
    #' @param relation (character(1)) How to relate perturbed scores to originals. Can be overridden in `$compute()`.
    #' @param iters_perm (integer(1)) Number of permutation iterations. Can be overridden in `$compute()`.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to ARFSampler
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      conditioning_set = NULL,
      relation = "difference",
      iters_perm = 1L,
      sampler = NULL
    ) {
      # Use ARFSampler by default for RFI
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        sampler = sampler,
        relation = relation,
        iters_perm = iters_perm
      )

      # Validate and set up conditioning set after task is available
      if (!is.null(conditioning_set)) {
        conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)
      } else {
        # Default to empty set (equivalent to PFI)
        conditioning_set = character(0)
      }

      # Configure the sampler with the conditioning_set
      self$sampler$param_set$values$conditioning_set = conditioning_set

      # Create extended param_set for RFI with conditioning_set parameter
      rfi_ps = paradox::ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1),
        conditioning_set = paradox::p_uty(default = character(0))
      )

      # Set values from base param_set and add conditioning_set
      rfi_ps$values$relation = self$param_set$values$relation
      rfi_ps$values$iters_perm = self$param_set$values$iters_perm
      rfi_ps$values$conditioning_set = conditioning_set

      self$param_set = rfi_ps

      self$label = "Relative Feature Importance"
    },

    #' @description
    #' Compute RFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals. If `NULL`, uses stored value.
    #' @param conditioning_set ([character()]) Set of features to condition on. If `NULL`, uses the stored parameter value.
    #' @param iters_perm (integer(1)) Number of permutation iterations. If `NULL`, uses stored value.
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(
      relation = NULL,
      conditioning_set = NULL,
      iters_perm = NULL,
      store_backends = TRUE
    ) {
      # Handle conditioning_set parameter override
      if (!is.null(conditioning_set)) {
        # Validate the provided conditioning_set
        conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)

        # Clear cache and temporarily modify sampler's conditioning_set
        self$importance = NULL
        self$scores = NULL
        old_conditioning_set = self$sampler$param_set$values$conditioning_set
        self$sampler$param_set$values$conditioning_set = conditioning_set
        on.exit(self$sampler$param_set$values$conditioning_set <- old_conditioning_set)
      }

      # Use the (potentially modified) sampler
      private$.compute_perturbation_importance(
        relation = relation,
        iters_perm = iters_perm,
        store_backends = store_backends,
        sampler = self$sampler
      )
    }
  )
)
