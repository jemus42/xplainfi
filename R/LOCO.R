#' LOCO Class
#'
#' Calculates Leave-One-Covariate-Out (LOCO) scores.
#'
#' @export
#'
#' @examplesIf requireNamespace("rpart")
#'
#' library(mlr3)
#'
#' loco = LOCO$new(
#'   task = tsk("zoo"),
#'   learner = lrn("classif.rpart"),
#'   measure = msr("classif.ce")
#' )
#'
#' loco$compute()
LOCO = R6Class(
  "LOCO",
  inherit = FeatureImportanceMeasure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task,learner,measure,resampling,features Passed to `FeatureImportanceMeasure` for construction.
    initialize = function(task, learner, measure, resampling = NULL, features = NULL) {
      # params
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference")
      )
      ps$values = list(relation = "difference")

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        param_set = ps,
        label = "LOCO Feature Importance"
      )
    },

    #' @description
    #' A short description...
    #' @param relation (character(1)) Calculate `"difference"` (default) or `"ratio"` of
    #'   original scores and scores after permutation
    #' @param store_backends (logical(1): `TRUE`) Passed to [mlr3::resample] to store
    #' backends in resample result.
    #' Required for some measures, but may increase memory footprint.
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      relation = match.arg(relation)

      # Check if already compute with this relation
      # Recompute if different relation chosen
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

      scores_pre = rr$score(self$measure)[, .SD, .SDcols = c("iteration", self$measure$id)]
      setnames(scores_pre, old = self$measure$id, "scores_pre")

      scores = lapply(seq_len(self$resampling$iters), \(iter) {
        compute_loc(
          learner = rr$learners[[iter]],
          task = self$task,
          train_ids = rr$resampling$train_set(iter),
          test_ids = rr$resampling$test_set(iter),
          features = self$features,
          measure = self$measure,
          direction = "leave-out"
        )
      })

      # Collect loco's scores, add original scores
      scores = rbindlist(scores, idcol = "iteration")
      scores = scores[scores_pre, on = "iteration"]
      setcolorder(scores, c("feature", "iteration", "scores_pre", "scores_post"))

      # Calculate LOCO depending on relation(-, /), and minimize property
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
        new = c("iter_rsmp", paste0(self$measure$id, c("_orig", "_loco")))
      )

      setkeyv(scores, c("feature", "iter_rsmp"))

      # Aggregate by feature over resamplings
      scores_agg = scores[, list(importance = mean(importance)), by = "feature"]

      self$scores = scores
      self$importance = scores_agg
      self$resample_result = rr

      self$importance
    }
  ),

  private = list()
)
