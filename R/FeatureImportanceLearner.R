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

    self$task = task
    self$learner = learner
    self$measure = measure
    self$resampling = resampling
    self$param_set = param_set
    self$label = label

    if (is.null(features)) {
      self$features = self$task$feature_names
    }

  },
  #' #' @description
  #'   #' Computes importance scores
  #' compute = function() {
  #' },

  #' @description
    #' Print importance scores
    #'
    #' @param ... Passed to `print()`
  print = function(...) {
    mlr3misc::catn(self$label)
    # browser()
    if (!is.null(self$importance)) print(self$importance, ...)
  }
 )
)
