#' Feature Importance Learner Class
#'
#' @export
FeatureImportanceLearner = R6Class("FeatureImportanceLearner",
  public = list(
    #' @field label (character(1)) Method label
    label = NA_character_,
    #' @field task (mlr3::Task)
    task = NULL,
    #' @field learner (mlr3::Learner)
    learner = NULL,
    #' @field measure (mlr3::Measure)
    measure = NULL,
    #' @field resampling (mlr3::Resampling)
    resampling = NULL,
    #' @field resample_result (mlr3::ResampleResult)
    resample_result = NULL,
    #' @field features (list())
    features = NULL,
    #' @field param_set (paradox::ps())
    param_set = ps(),
    #' @field importance (numeric())
    importance = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' This is typically intended for use by derived classes.
    #' @param task,learner,measure,resampling,features,param_set,label Used to set fields
    initialize = function(task, learner, measure, resampling = NULL, features = NULL, param_set = paradox::ps(), label) {

      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task)
      self$measure = mlr3::assert_measure(measure)
      self$resampling = resampling %??% mlr3::assert_resampling(resampling)
      self$param_set = paradox::assert_param_set(param_set)
      self$label = checkmate::assert_string(label, min.chars = 1)
      self$features = features %??% self$task$feature_names

    },
    # #' @description
    #   #' Computes importance scores
    # compute = function() {
    # },

    #' @description
    #' Print importance scores
    #'
    #' @param ... Passed to `print()`
    print = function(...) {
      cat(self$label, "\n")
      if (!is.null(self$importance)) print(self$importance, ...)
    }
  )
)
