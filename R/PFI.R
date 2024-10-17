#' PFI Class
#'
#' Calculates Permutation Feature Importance (PFI) scores.
#'
#' @export
#'
#' @examplesIf requireNamespace("rpart")
#'
#' library(mlr3)
#'
#' pfi = PFI$new(
#'   task = tsk("zoo"),
#'   learner = lrn("classif.rpart"),
#'   measure = msr("classif.ce")
#' )
#'
#' pfi$compute()
PFI = R6Class("PFI",
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

      # resampling: default to holdout with default ratio if NULL
      resampling = resampling %||% mlr3::rsmp("holdout")$instantiate(task)

      if (!resampling$is_instantiated) {
        resampling$instantiate(task)
      }

      # measure
      mlr3::assert_measure(measure = measure, task = task, learner = learner)

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        param_set = ps,
        label = "Permutation Feature Importance"
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
      self$resample_result = rr

      scores_orig = rr$score(self$measure)[, .SD, .SDcols = c("iteration", self$measure$id)]
      data.table::setnames(scores_orig, old = self$measure$id, "scores_pre")

      scores_permuted = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_permuted_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter))
        )
      })

      # Collect permuted scores, add original scores
      scores_permuted = data.table::rbindlist(scores_permuted, idcol = "iteration")
      scores_permuted = scores_permuted[scores_orig, on = "iteration"]
      # Calculate PFI depending on relation(-, /), and minimize property
      scores_permuted[, importance := compute_score_relation(
        scores_pre, scores_post,
        relation = self$param_set$values$relation,
        minimize = self$measure$minimize
      )]
      # Aggregate by feature over resamplings
      scores_permuted_agg = scores_permuted[, .(importance = mean(importance)), by = feature]

      self$importance = scores_permuted_agg$importance
      names(self$importance) = scores_permuted_agg$feature

      self$importance
    }
  ),

  private = list(
    # TODO: Should this use self$features etc. or should these be passed as arguments?
    .compute_permuted_score = function(learner, test_dt) {

      scores_post = vapply(self$features, \(feature) {
        # Copying task for every feature, not great
        task_data = data.table::copy(test_dt)

        # Permute in-place
        task_data[, (feature) := sample(.SD[[feature]])][]

        # Use predict_newdata to avoid having to reconstruct a new Task, needs orig task
        pred = learner$predict_newdata(newdata = task_data, task = self$task)

        score = pred$score(self$measure)
        names(score) = feature
        score

      }, FUN.VALUE = numeric(1))

      data.table(
        feature = names(scores_post),
        scores_post = scores_post
      )
    }
  )
)
