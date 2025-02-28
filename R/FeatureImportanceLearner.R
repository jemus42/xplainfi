#' Feature Importance Learner Class
#'
#' @export
FeatureImportanceLearner = R6Class(
  "FeatureImportanceLearner",
  public = list(
    #' @field label ([`character(1)`]) Method label
    label = NA_character_,
    #' @field task ([`mlr3::Task`])
    task = NULL,
    #' @field learner ([`mlr3::Learner`])
    learner = NULL,
    #' @field measure ([`mlr3::Measure`])
    measure = NULL,
    #' @field resampling ([`mlr3::Resampling`])
    resampling = NULL,
    #' @field resample_result ([`mlr3::ResampleResult`])
    resample_result = NULL,
    # TODO: list of features, for grouped importance
    #' @field features ([`list()`])
    features = NULL,
    #' @field param_set ([`paradox::ps()`])
    param_set = ps(),
    #' @field importance ([`data.table()`]) Aggregated importance scores
    importance = NULL,
    #' @field scores ([`data.table()`]) Individual performance scores used to compute `$importance`
    scores = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' This is typically intended for use by derived classes.
    #' @param task,learner,measure,resampling,features,param_set,label Used to set fields
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      param_set = paradox::ps(),
      label
    ) {
      self$task = mlr3::assert_task(task)
      self$learner = mlr3::assert_learner(learner, task = task, task_type = task$task_type)
      self$measure = mlr3::assert_measure(measure, task = task, learner = learner)
      self$param_set = paradox::assert_param_set(param_set)
      self$label = checkmate::assert_string(label, min.chars = 1)
      self$features = features %||% self$task$feature_names

      # resampling: default to holdout with default ratio if NULL
      resampling = resampling %||% mlr3::rsmp("holdout")$instantiate(task)
      if (!resampling$is_instantiated) {
        resampling$instantiate(task)
      }
      self$resampling = mlr3::assert_resampling(resampling)
    },

    #' @description
    #' Combine two `FeatureImportanceLearner` objects with computed scores.
    #'
    #' @param y ([FeatureImportanceLearner]) Object to combine. Must have computed scores.
    #' @param ... (any) Unused.
    #' @return A new [FeatureImportanceLearner] of the same subclass as `x` and `y`.
    #' Currently this method merges the following:
    #' - `$scores` is combined, with `iter_rsmp` increased for `y`.
    #' - `$importance` is re-computed from the combined `$scores`.
    #' - `$resample_result` is combined to a [mlr3::BenchmarkResult]
    #' - `$resampling` is combined into a [mlr3::ResamplingCustom], again continuing te `iteration` count from `x` with that of `y`.
    combine = function(y, ...) {
      checkmate::assert_class(self, classes = "FeatureImportanceLearner")
      checkmate::assert_class(y, classes = "FeatureImportanceLearner")
      checkmate::assert_true(class(self)[[1]] == class(y)[[1]], .var.name = "Identical subclasses")
      checkmate::assert_data_table(self$importance, key = "feature")
      checkmate::assert_data_table(y$importance, key = "feature")

      checkmate::assert_true(self$task$hash == y$task$hash, .var.name = "identical tasks")
      checkmate::assert_true(self$measure$hash == y$measure$hash, .var.name = "identical measures")
      checkmate::assert_true(self$learner$hash == y$learner$hash, .var.name = "identical learners")

      # merge importance scores
      scores_y = copy(y$scores)
      # Increase iteration count for y for consistency
      scores_y[, let(iter_rsmp = iter_rsmp + self$resampling$iters)]
      scores = rbindlist(list(self$scores, scores_y))
      setkeyv(scores, c("feature", "iter_rsmp"))

      # Merge aggregated cores
      importance = scores[, list(importance = mean(importance)), by = feature]

      # Modify
      self$scores = scores
      self$importance = importance
      self$resample_result = c(self$resample_result, y$resample_result)

      # Combine resampling objects?
      rsmp_x = as.data.table(self$resampling)
      rsmp_y = as.data.table(y$resampling)
      rsmp_y[, let(iteration = iteration + self$resampling$iters)]
      rsmp_x = rbind(rsmp_x, rsmp_y)
      setkeyv(rsmp_x, c("set"))

      resampling = mlr3::ResamplingCustom$new()
      resampling$instance = list(
        train_sets = rsmp_x[list("train"), list(ids = list(row_id)), by = "iteration"]$ids,
        test_sets = rsmp_x[list("test"), list(ids = list(row_id)), by = "iteration"]$ids
      )
      self$resampling = resampling

      self
    },

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
