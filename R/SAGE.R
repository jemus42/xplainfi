#' SAGE Class
#'
#' Calculates SAGE importance scores.
#'
#' @export
#'
#' @examplesIf requireNamespace("rpart")
#'
#' library(mlr3)
#'
#' sage = SAGE$new(
#'   task = tsk("zoo"),
#'   learner = lrn("classif.rpart"),
#'   measure = msr("classif.ce")
#' )
#'
#' sage$compute()
SAGE = R6Class("SAGE",
  inherit = FeatureImportanceLearner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task,learner,measure,resampling,features Passed to `FeatureImportanceLearner` for construction.
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
        label = "SAGE Feature Importance"
      )
    },

    #' @description
    #' A short description...
    #' @param relation (character(1)) Calculate `"difference"` (default) or `"ratio"` of
    #'   original scores and scores after permutation
    compute = function(relation = c("difference", "ratio")) {
      relation = match.arg(relation)

      # Check if already compute with this relation
      # Recompute if different relation chosen
      if (!is.null(self$importance) & self$param_set$values$relation == relation) {
        return(self$importance)
      }
      # Store relation
      self$param_set$values$relation = relation

      # Quiet down
      current_log_threshold = lgr::get_logger("mlr3")$threshold
      on.exit(lgr::get_logger("mlr3")$set_threshold(current_log_threshold))
      lgr::get_logger("mlr3")$set_threshold("warn")

      # Initial resampling
      rr = mlr3::resample(
        self$task, self$learner, self$resampling,
        store_models = TRUE, store_backends = FALSE
      )

      # Store results
      self$resample_result = rr

    }
  ),

  private = list()
)
