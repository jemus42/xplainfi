#' PFI Class
#'
#' Calculates Permutation Feature Importance (PFI) scores.
#'
#' @export
#'
#' @examplesIf requireNamespace("mlr3learners") & requireNamespace("ranger")
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
        label = "Permutation Feature Importance"
      )
    },

    #' @description
    #' A short description...
    #' @param relation (character(1)) Calculate `"difference"` (default) or `"ratio"` of
    #'   original loss and loss after permutation
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

      scores_orig = rr$score(self$measure, predict_sets = "test")[, .SD, .SDcols = c("iteration", self$measure$id)]
      data.table::setnames(scores_orig, old = self$measure$id, "loss_orig")

      scores_permuted =  lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_pfi_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter))
        )
      })

      # Collect permuted scores, add original scores
      scores_permuted = data.table::rbindlist(scores_permuted, idcol = "iteration")
      scores_permuted = scores_permuted[scores_orig, on = "iteration"]
      # Calculate PFI depending on relation(-, /), and minimize property
      scores_permuted[, importance := private$.compute_pfi_relation(loss_orig, loss)]
      # Aggregate by feature over resamplings
      scores_permuted_agg = scores_permuted[, .(importance = mean(importance)), by = feature]

      self$importance = scores_permuted_agg$importance
      names(self$importance) = scores_permuted_agg$feature

      self$importance
    }
  ),

  private = list(
    .compute_pfi_relation = function(loss_orig, loss_permuted) {

      if (self$measure$minimize) {
        # Lower is better, e.g. ce
        switch(self$param_set$values$relation,
               difference = loss_permuted - loss_orig,
               ratio = loss_permuted / loss_orig
        )
      } else {
        # Higher is better, e.g. accuracy
        switch(self$param_set$values$relation,
               difference = loss_orig - loss_permuted,
               ratio = loss_orig / loss_permuted
        )
      }
    },

    .compute_pfi_score = function(learner, test_dt) {

        loss_permuted = vapply(self$features, \(feature) {
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

        data.table::data.table(
          feature = names(loss_permuted),
          loss = loss_permuted
        )
    }
  )
)
