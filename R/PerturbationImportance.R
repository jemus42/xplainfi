#' @title Feature Importance Base Class
#'
#' @description Abstract base class for feature importance methods
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
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      sampler = NULL
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
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
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
    #' @param iters_perm (integer(1)) Number of permutation iterations
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
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
        sampler = sampler
      )

      # Set parameters
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1)
      )

      ps$values$relation = "difference"
      ps$values$iters_perm = iters_perm

      self$param_set = ps
      self$label = "Permutation Feature Importance"
    },

    #' @description
    #' Compute PFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      relation = match.arg(relation)

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
        private$.compute_permuted_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
          iters_perm = self$param_set$values$iters_perm
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
  ),

  private = list(
    .compute_permuted_score = function(learner, test_dt, iters_perm = 1) {
      rbindlist(
        lapply(seq_len(iters_perm), \(iter) {
          scores_post = vapply(
            self$features,
            \(feature) {
              # Use the sampler to permute the feature (marginal sampling for PFI)
              perturbed_data = self$sampler$sample(feature, test_dt)

              # Predict and score
              pred = learner$predict_newdata(newdata = perturbed_data, task = self$task)
              score = pred$score(self$measure)
              names(score) = feature
              score
            },
            FUN.VALUE = numeric(1)
          )

          data.table(
            feature = names(scores_post),
            iter_perm = iter,
            scores_post = scores_post
          )
        })
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
    #' @param iters_perm (integer(1)) Number of sampling iterations.
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to instantiationg `ARFSampler` internally with default parameters.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
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
        sampler = sampler
      )

      # Set parameters
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1)
      )

      ps$values$relation = "difference"
      ps$values$iters_perm = iters_perm

      self$param_set = ps
      self$label = "Conditional Feature Importance"
    },

    #' @description
    #' Compute CFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      # Implementation largely identical to PFI
      # The key difference is in the sampler used (conditional vs marginal)
      relation = match.arg(relation)

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
        private$.compute_permuted_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
          iters_perm = self$param_set$values$iters_perm
        )
      })

      # Same post-processing as PFI
      scores = rbindlist(scores, idcol = "iteration")
      scores = scores[scores_orig, on = "iteration"]
      setcolorder(scores, c("feature", "iteration", "iter_perm", "scores_pre", "scores_post"))

      scores[,
        importance := compute_score(
          scores_pre,
          scores_post,
          relation = self$param_set$values$relation,
          minimize = self$measure$minimize
        )
      ]

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
  ),

  private = list(
    .compute_permuted_score = function(learner, test_dt, iters_perm = 1) {
      rbindlist(
        lapply(seq_len(iters_perm), \(iter) {
          scores_post = vapply(
            self$features,
            \(feature) {
              # Use the conditional sampler to perturb the feature
              perturbed_data = self$sampler$sample(
                feature,
                test_dt,
                # For CFI, we condition on all other features
                conditioning_features = setdiff(self$task$feature_names, feature)
              )

              # Predict and score
              pred = learner$predict_newdata(newdata = perturbed_data, task = self$task)
              score = pred$score(self$measure)
              names(score) = feature
              score
            },
            FUN.VALUE = numeric(1)
          )

          data.table(
            feature = names(scores_post),
            iter_perm = iter,
            scores_post = scores_post
          )
        })
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
    #' @field conditioning_set ([character()]) Features to condition on
    conditioning_set = NULL,

    #' @description
    #' Creates a new instance of the RFI class
    #' @param task,learner,measure,resampling,features Passed to PerturbationImportance
    #' @param conditioning_set ([character()]) Set of features to condition on
    #' @param iters_perm (integer(1)) Number of permutation iterations
    #' @param sampler ([ConditionalSampler]) Optional custom sampler. Defaults to ARFSampler
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      conditioning_set = NULL,
      iters_perm = 1L,
      sampler = NULL
    ) {
      # Use ARFSampler by default for RFI (RelativeSampler inherits from ConditionalSampler)
      if (is.null(sampler)) {
        sampler = ARFSampler$new(task)
      }

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        sampler = sampler
      )

      # Validate and store conditioning set
      if (!is.null(conditioning_set)) {
        self$conditioning_set = checkmate::assert_subset(conditioning_set, self$task$feature_names)
      } else {
        # Default to empty set (equivalent to PFI)
        self$conditioning_set = character(0)
      }

      # Set parameters
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1)
      )

      ps$values$relation = "difference"
      ps$values$iters_perm = iters_perm

      self$param_set = ps
      self$label = "Relative Feature Importance"
    },

    #' @description
    #' Compute RFI scores
    #' @param relation (character(1)) How to relate perturbed scores to originals
    #' @param store_backends (logical(1)) Whether to store backends
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      # Implementation identical to CFI - the key difference is in the conditioning set
      relation = match.arg(relation)

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
        private$.compute_permuted_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
          iters_perm = self$param_set$values$iters_perm
        )
      })

      # Same post-processing as PFI/CFI
      scores = rbindlist(scores, idcol = "iteration")
      scores = scores[scores_orig, on = "iteration"]
      setcolorder(scores, c("feature", "iteration", "iter_perm", "scores_pre", "scores_post"))

      scores[,
        importance := compute_score(
          scores_pre,
          scores_post,
          relation = self$param_set$values$relation,
          minimize = self$measure$minimize
        )
      ]

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
  ),

  private = list(
    .compute_permuted_score = function(learner, test_dt, iters_perm = 1) {
      rbindlist(
        lapply(seq_len(iters_perm), \(iter) {
          scores_post = vapply(
            self$features,
            \(feature) {
              # Use the sampler with the specific conditioning set for RFI
              perturbed_data = self$sampler$sample(
                feature,
                test_dt,
                conditioning_features = self$conditioning_set
              )

              # Predict and score
              pred = learner$predict_newdata(newdata = perturbed_data, task = self$task)
              score = pred$score(self$measure)
              names(score) = feature
              score
            },
            FUN.VALUE = numeric(1)
          )

          data.table(
            feature = names(scores_post),
            iter_perm = iter,
            scores_post = scores_post
          )
        })
      )
    }
  )
)
