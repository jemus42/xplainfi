#' @title Leave-Out/In Base Class
#'
#' @description
#' Base class for Leave-Out and Leave-In feature importance methods.
#' This is an abstract class - use LOCO or LOCI.
#'
#' @keywords internal
LeaveOutIn = R6Class(
  "LeaveOutIn",
  inherit = FeatureImportanceMethod,
  public = list(
    #' @field direction (`character(1)`) Either "leave-out" or "leave-in".
    direction = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task,learner,measure,resampling,features Passed to `FeatureImportanceMethod` for construction.
    #' @param direction (`character(1)`) Either "leave-out" or "leave-in".
    #' @param label (`character(1)`) Method label.
    #' @param iters_refit (`integer(1)`) Number of refit iterations per resampling iteration.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      direction,
      label,
      iters_refit = 1L
    ) {
      # Validate direction
      checkmate::assert_choice(direction, c("leave-out", "leave-in"))
      checkmate::assert_int(iters_refit, lower = 1L)
      self$direction = direction

      # params
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_refit = paradox::p_int(lower = 1, default = 1)
      )
      ps$values = list(relation = "difference", iters_refit = iters_refit)

      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        param_set = ps,
        label = label
      )
    },

    #' @description
    #' Computes leave-out or leave-in feature importance.
    #' @param relation (`character(1)`) Calculate `"difference"` (default) or `"ratio"` of
    #'   original scores and scores after leaving out/in features.
    #' @param store_backends (`logical(1)`) Passed to [mlr3::resample] to store
    #'   backends in resample result.
    #'   Required for some measures, but may increase memory footprint.
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      # TODO: We technically don't want the user to call this method from the base class,
      # only if LOCO or LOCI are used
      relation = match.arg(relation)

      # Check if already computed with this relation
      # Recompute if different relation chosen
      if (!is.null(self$importance) && self$param_set$values$relation == relation) {
        return(self$importance)
      }
      # Store relation
      self$param_set$values$relation = relation

      # For LOCO: use full model as baseline
      # For LOCI: use featureless model as baseline
      if (self$direction == "leave-out") {
        # For LOCO, get baseline scores by running full model across resampling
        rr_reference = resample(
          self$task,
          self$learner,
          self$resampling,
          store_models = FALSE,
          store_backends = FALSE
        )
      } else {
        # For LOCI, get baseline scores using featureless learner
        learner_featureless = switch(
          self$task$task_type,
          "classif" = mlr3::lrn("classif.featureless", predict_type = "prob"),
          "regr" = mlr3::lrn("regr.featureless")
        )

        rr_reference = resample(
          self$task,
          learner_featureless,
          self$resampling,
          store_models = FALSE,
          store_backends = FALSE
        )
      }

      scores_pre = rr_reference$score(self$measure)[,
        .SD,
        .SDcols = c("iteration", self$measure$id)
      ]
      setnames(scores_pre, old = self$measure$id, "scores_pre")

      # Compute feature-specific scores using the instantiated resampling
      scores = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_loc(
          learner = self$learner$clone(),
          task = self$task,
          train_ids = self$resampling$train_set(iter),
          test_ids = self$resampling$test_set(iter),
          features = self$features,
          measure = self$measure,
          iters_refit = self$param_set$values$iters_refit
        )
      })

      # Collect scores, add original scores
      scores = rbindlist(scores, idcol = "iteration")
      scores = scores[scores_pre, on = "iteration"]
      setcolorder(scores, c("feature", "iteration", "iter_refit", "scores_pre", "scores_post"))

      # Calculate importance depending on relation(-, /), and minimize property
      # For LOCI: importance = featureless - single_feature (so swap arguments)
      # For LOCO: importance = reduced_model - full_model
      if (self$direction == "leave-in") {
        scores[,
          importance := compute_score(
            scores_post, # single feature score (as "pre")
            scores_pre, # featureless score (as "post") - this gives featureless - single_feature
            relation = self$param_set$values$relation,
            minimize = self$measure$minimize
          )
        ]
      } else {
        scores[,
          importance := compute_score(
            scores_pre, # full model score
            scores_post, # reduced model score
            relation = self$param_set$values$relation,
            minimize = self$measure$minimize
          )
        ]
      }

      setnames(
        scores,
        old = c("iteration", "scores_pre", "scores_post"),
        new = c("iter_rsmp", paste0(self$measure$id, c("_orig", "_post")))
      )

      setkeyv(scores, c("feature", "iter_rsmp"))

      # Aggregate by feature
      scores_agg = private$.aggregate_importances(scores)

      # Store results
      # Store the baseline resample result (either full model or featureless)
      self$resample_result = rr_reference
      self$scores = scores
      self$importance = scores_agg

      copy(self$importance)
    }
  ),

  private = list(
    .compute_loc = function(
      learner,
      task,
      train_ids,
      test_ids,
      features,
      measure,
      iters_refit = 1L
    ) {
      # Store/restore complete set of features as $feature_names will shrink otherwise
      features_total = task$feature_names
      # Ensure to reassign column roles to original state even if some model fit errors
      # Avoids accidentally modifying the task (by reference) permanently
      on.exit({
        task$col_roles$feature = features_total
      })

      rbindlist(
        lapply(seq_len(iters_refit), \(iter) {
          scores_post = vapply(
            features,
            \(feature) {
              task$col_roles$feature = switch(
                self$direction,
                "leave-in" = feature,
                "leave-out" = setdiff(features_total, feature)
              )

              learner$reset()
              learner$train(task, row_ids = train_ids)
              pred = learner$predict(task, row_ids = test_ids)

              score = pred$score(measure)
              names(score) = feature
              score
            },
            FUN.VALUE = numeric(1)
          )

          data.table(
            feature = names(scores_post),
            iter_refit = iter,
            scores_post = scores_post
          )
        })
      )
    }
  )
)

#' @title Leave-One-Covariate-Out (LOCO)
#'
#' @description
#' Calculates Leave-One-Covariate-Out (LOCO) scores.
#' Despite the name, this implementation can leave out one or more features at a time.
#'
#' @details
#' LOCO measures feature importance by comparing model performance with and without
#' each feature. For each feature, the model is retrained without that feature and
#' the performance difference (reduced_model_loss - full_model_loss) indicates the
#' feature's importance. Higher values indicate more important features.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3learners)
#' task = tgen("friedman1")$generate(n = 200)
#' loco = LOCO$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse")
#' )
#' loco$compute()
#' @export
#'
#' @references `r print_bib("lei_2018")`
LOCO = R6Class(
  "LOCO",
  inherit = LeaveOutIn,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task ([mlr3::Task]) Task to compute importance for.
    #' @param learner ([mlr3::Learner]) Learner to use for prediction.
    #' @param measure ([mlr3::Measure]) Measure to use for scoring.
    #' @param resampling ([mlr3::Resampling]) Resampling strategy. Defaults to holdout.
    #' @param features (`character()`) Features to compute importance for. Defaults to all features.
    #' @param iters_refit (`integer(1)`) Number of refit iterations per resampling iteration.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      iters_refit = 1L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        direction = "leave-out",
        label = "Leave-One-Covariate-Out (LOCO)",
        iters_refit = iters_refit
      )
    }
  )
)

#' @title Leave-One-Covariate-In (LOCI)
#'
#' @description
#' Calculates Leave-One-Covariate-In (LOCI) scores.
#' Despite the name, this implementation can leave in one or more features at a time.
#'
#' @details
#' LOCI measures feature importance by training models with only each individual
#' feature (or feature subset) and comparing their performance to a featureless
#' baseline model (optimal constant prediction). The importance is calculated as
#' (featureless_model_loss - single_feature_loss). Positive values indicate the
#' feature performs better than the baseline, negative values indicate worse performance.
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' task = tgen("friedman1")$generate(n = 200)
#' loci = LOCI$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mse")
#' )
#' loci$compute()
#' @export
LOCI = R6Class(
  "LOCI",
  inherit = LeaveOutIn,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param task ([mlr3::Task]) Task to compute importance for.
    #' @param learner ([mlr3::Learner]) Learner to use for prediction.
    #' @param measure ([mlr3::Measure]) Measure to use for scoring.
    #' @param resampling ([mlr3::Resampling]) Resampling strategy. Defaults to holdout.
    #' @param features (`character()`) Features to compute importance for. Defaults to all features.
    #' @param iters_refit (`integer(1)`) Number of refit iterations per resampling iteration.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      iters_refit = 1L
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        direction = "leave-in",
        label = "Leave-One-Covariate-In (LOCI)",
        iters_refit = iters_refit
      )
    }
  )
)
