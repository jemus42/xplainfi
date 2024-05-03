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
LOCO = R6Class("LOCO",
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

      # resampling
      if (is.null(resampling)) {
        resampling = mlr3::rsmp("holdout", ratio = 2/3)$instantiate(task)
      }
      resampling = resampling

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
        label = "LOCO Feature Importance"
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
        "!DEBUG Already computed with relation `relation`"
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

      scores_pre = rr$score(self$measure, predict_sets = "test")[, .SD, .SDcols = c("iteration", self$measure$id)]
      data.table::setnames(scores_pre, old = self$measure$id, "scores_pre")

      scores_loco =  lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_loco_score(
          # Clone learner ans task to prevent modifying originals
          learner = rr$learners[[iter]]$clone(),
          task = self$task$clone(),
          train_ids = rr$resampling$train_set(iter),
          test_ids = rr$resampling$test_set(iter)
        )
      })
# browser()
      # Collect permuted scores, add original scores
      scores_loco = data.table::rbindlist(scores_loco, idcol = "iteration")
      scores_loco = scores_loco[scores_pre, on = "iteration"]
      # Calculate LOCO depending on relation(-, /), and minimize property
      scores_loco[, importance := compute_score_relation(
        scores_pre, scores_post,
        relation = self$param_set$values$relation,
        minimize = self$measure$minimize
      )]

      # Aggregate by feature over resamplings
      scores_loco_agg = scores_loco[, .(importance = mean(importance)), by = feature]

      self$importance = scores_loco_agg$importance
      names(self$importance) = scores_loco_agg$feature

      self$importance
    }
  ),

  private = list(
    .compute_loco_score = function(learner, task, train_ids, test_ids) {

      # Store complete set of features as $feature_names will shrink otherwise
      features_total = task$feature_names

      scores_post = vapply(self$features, \(feature) {
        "!DEBUG Leaving out `feature`"
        # browser()

        # Get set of all features without current feature
        task$col_roles$feature = setdiff(features_total, feature)
        "!DEBUG features: `sort(task$feature_names)`"

        learner$reset()
        learner$train(task, row_ids = train_ids)
        # Use predict_newdata to avoid having to reconstruct a new Task, needs orig task
        pred = learner$predict(task, row_ids = test_ids)

        score = pred$score(self$measure)
        names(score) = feature
        score

      }, FUN.VALUE = numeric(1))

      data.table::data.table(
        feature = names(scores_post),
        scores_post = scores_post
      )
    }
  )
)
