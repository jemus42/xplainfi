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
    #' @param iters_perm `(integer(1): 1L)` Number of permutations to compute for each feature.
    #' Permutations are repeated within each resampling fold.
    initialize = function(task, learner, measure, resampling = NULL, features = NULL,
                          iters_perm = 1L) {
      # params
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_perm = paradox::p_int(lower = 1, default = 1)
      )

      ps$values$relation = "difference"
      ps$values$iters_perm = iters_perm

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
    #' @param store_backends (logical(1): `TRUE`) Passed to [mlr3::resample()] to store
    #' backends in resample result.
    #' Required for some measures, but may increase memory footprint.
    compute = function(relation = c("difference", "ratio"),
                       store_backends = TRUE
                       ) {
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
        store_models = TRUE, # Needed for predict_newdata later
        store_backends = store_backends
      )
      scores_orig = rr$score(self$measure)[, .SD, .SDcols = c("iteration", self$measure$id)]
      data.table::setnames(scores_orig, old = self$measure$id, "scores_pre")

      # TODO: Make rr reusable if measure changes
      # to re-score for different measure but not re-compute everything
      scores = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_permuted_score(
          learner = rr$learners[[iter]],
          test_dt = self$task$data(rows = rr$resampling$test_set(iter)),
          iters_perm = self$param_set$values$iters_perm
        )
      })

      # Collect permuted scores, add original scores
      scores = data.table::rbindlist(scores, idcol = "iteration")
      scores = scores[scores_orig, on = "iteration"]
      data.table::setcolorder(scores, c("feature", "iteration", "iter_perm", "scores_pre", "scores_post"))

      # Calculate PFI depending on relation(-, /), and minimize property
      scores[, importance := compute_score(
        scores_pre, scores_post,
        relation = self$param_set$values$relation,
        minimize = self$measure$minimize
      )]

      data.table::setnames(
        scores,
        old = c("iteration", "scores_pre", "scores_post"),
        new = c("iter_rsmp", paste0(self$measure$id, c("_orig", "_perm")))
      )

      data.table::setkeyv(scores, c("feature", "iter_rsmp"))

      # Aggregate by feature over resamplings and permutations
      scores_agg = scores[, list(importance = mean(importance)), by = feature]

      # Store results
      self$resample_result = rr
      self$scores = scores
      self$importance = scores_agg

      self$importance
    }
  ),

  private = list(
    # TODO: Should this use self$features etc. or should these be passed as arguments?
    .compute_permuted_score = function(learner, test_dt, iters_perm = 1) {

      data.table::rbindlist(
        lapply(seq_len(iters_perm), \(iter) {
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
            iter_perm = iter,
            scores_post = scores_post
          )
        })
      )


    }
  )
)
