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
    #' @param obs_loss (`logical(1)`) Whether to use observation-wise loss calculation (original LOCO formulation) when supported by the measure. If `FALSE` (default), uses aggregated scores.
    #' @param aggregation_fun (`function`) Function to aggregate observation-wise losses when `obs_loss = TRUE`. Defaults to `median` for original LOCO formulation.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      direction,
      label,
      iters_refit = 1L,
      obs_loss = FALSE,
      aggregation_fun = median
    ) {
      # Validate direction
      checkmate::assert_choice(direction, c("leave-out", "leave-in"))
      checkmate::assert_int(iters_refit, lower = 1L)
      checkmate::assert_flag(obs_loss)
      checkmate::assert_function(aggregation_fun)
      self$direction = direction

      # params
      ps = ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference"),
        iters_refit = paradox::p_int(lower = 1, default = 1),
        obs_loss = paradox::p_lgl(default = FALSE),
        aggregation_fun = paradox::p_uty(default = median)
      )
      ps$values = list(
        relation = "difference",
        iters_refit = iters_refit,
        obs_loss = obs_loss,
        aggregation_fun = aggregation_fun
      )

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
      if (!is.null(self$scores) && self$param_set$values$relation == relation) {
        return(self$importance())
      }
      # Store relation
      self$param_set$values$relation = relation

      # Unified computation path
      return(private$.compute_unified(relation, store_backends))
    }
  ),

  private = list(
    # Helper for micro-averaged computation: returns aggregated scores per feature
    .compute_feature_scores = function(
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
    },

    .compute_unified = function(relation, store_backends) {
      # Unified computation dispatcher: routes to appropriate aggregation strategy
      if (self$param_set$values$obs_loss) {
        # Check if measure supports obs_loss for macro-averaged computation
        if (is.null(self$measure$obs_loss)) {
          cli::cli_abort(
            "Measure {.cls {class(self$measure)[[1]]}} does not support observation-wise loss calculation. Set obs_loss = FALSE to use aggregated scores."
          )
        }
        # Macro-averaged: custom aggregation of observation-wise differences
        return(private$.compute_macro_averaged(relation, store_backends))
      } else {
        # Micro-averaged: measure's default aggregation of score differences
        return(private$.compute_micro_averaged(relation, store_backends))
      }
    },

    .compute_micro_averaged = function(relation, store_backends) {
      # Micro-averaged approach: E[L(Y, f_-j(X_-j))] - E[L(Y, f(X))]
      # Computes aggregated scores first, then differences

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
          "classif" = mlr3::lrn("classif.featureless", predict_type = self$learner$predict_type),
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
        private$.compute_feature_scores(
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
          importance := private$.compute_score(
            scores_post, # single feature score (as "pre")
            scores_pre, # featureless score (as "post") - this gives featureless - single_feature
            relation = relation,
            minimize = self$measure$minimize
          )
        ]
      } else {
        scores[,
          importance := private$.compute_score(
            scores_pre, # full model score
            scores_post, # reduced model score
            relation = relation,
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

      # Store results
      # Store the baseline resample result (either full model or featureless)
      self$resample_result = rr_reference
      self$scores = scores
      
      # Return aggregated importance
      self$importance()
    },

    .compute_macro_averaged = function(relation, store_backends) {
      # Macro-averaged approach: F({L(y_i, f_-j(x_i,-j)) - L(y_i, f(x_i))}_{i=1}^n)
      # Computes observation-wise differences first, then custom aggregation
      # obs_loss check already done in .compute_unified

      # Get reference predictions (full model for LOCO, featureless for LOCI)
      if (self$direction == "leave-out") {
        # For LOCO, get baseline predictions by running full model across resampling
        rr_reference = resample(
          self$task,
          self$learner,
          self$resampling,
          store_models = FALSE,
          store_backends = store_backends
        )
      } else {
        # For LOCI, get baseline predictions using featureless learner
        learner_featureless = switch(
          self$task$task_type,
          "classif" = mlr3::lrn("classif.featureless", predict_type = self$learner$predict_type),
          "regr" = mlr3::lrn("regr.featureless")
        )

        rr_reference = resample(
          self$task,
          learner_featureless,
          self$resampling,
          store_models = FALSE,
          store_backends = store_backends
        )
      }

      # Get observation-wise losses for reference predictions
      ref_predictions = rr_reference$predictions()
      obs_losses_ref = lapply(seq_len(self$resampling$iters), \(iter) {
        pred_ref = ref_predictions[[iter]]
        losses_ref = pred_ref$obs_loss(self$measure)
        losses_ref[, let(
          iteration = iter,
          response_ref = response # Store reference prediction
        )]
        losses_ref
      })
      obs_losses_ref = rbindlist(obs_losses_ref)
      setnames(obs_losses_ref, old = self$measure$id, new = "loss_ref")

      # Compute feature-specific observation-wise losses and collect predictions
      results_features = lapply(seq_len(self$resampling$iters), \(iter) {
        private$.compute_obs_loss_iter(
          learner = self$learner$clone(),
          task = self$task,
          train_ids = self$resampling$train_set(iter),
          test_ids = self$resampling$test_set(iter),
          features = self$features,
          measure = self$measure,
          iters_refit = self$param_set$values$iters_refit,
          iteration = iter
        )
      })

      # Extract obs_losses and predictions
      obs_losses_features = rbindlist(lapply(results_features, `[[`, "obs_losses"))
      predictions_features = rbindlist(lapply(results_features, `[[`, "predictions"))

      # Merge reference and feature-specific losses by row_ids and iteration
      obs_losses = obs_losses_features[obs_losses_ref, on = c("row_ids", "iteration")]
      setnames(obs_losses, old = self$measure$id, new = "loss_feature")

      # Calculate observation-wise differences
      # For LOCI: importance = featureless - single_feature (so swap arguments)
      # For LOCO: importance = reduced_model - full_model
      if (self$direction == "leave-in") {
        obs_losses[, obs_diff := loss_ref - loss_feature] # featureless - single_feature
      } else {
        obs_losses[, obs_diff := loss_feature - loss_ref] # reduced_model - full_model
      }

      # Aggregate observation-wise differences using specified aggregation function
      aggregation_fun = self$param_set$values$aggregation_fun
      scores = obs_losses[,
        list(
          importance = if (relation == "difference") {
            aggregation_fun(obs_diff, na.rm = TRUE)
          } else {
            # For ratio, need to handle differently - compute ratio first, then aggregate
            if (self$direction == "leave-in") {
              aggregation_fun(loss_ref / loss_feature, na.rm = TRUE)
            } else {
              aggregation_fun(loss_feature / loss_ref, na.rm = TRUE)
            }
          }
        ),
        by = list(feature, iteration, iter_refit)
      ]

      # Rename iteration to iter_rsmp for consistency with other methods
      setnames(scores, old = "iteration", new = "iter_rsmp")
      setkeyv(scores, c("feature", "iter_rsmp"))

      # Store scores and compute aggregated importance
      scores_agg = private$.aggregate_importances(scores)

      # Prepare observation-wise losses for storage
      # Include all relevant columns for user analysis
      obs_losses_stored = obs_losses[, list(
        row_ids,
        feature,
        iteration,
        iter_refit,
        truth,
        response_ref,
        response_feature,
        loss_ref,
        loss_feature,
        obs_diff
      )]

      # Store results
      self$resample_result = rr_reference
      self$scores = scores
      self$obs_losses = obs_losses_stored
      self$predictions = predictions_features
      
      # Return aggregated importance
      self$importance()
    },

    # Helper for macro-averaged computation: returns observation-wise losses and predictions
    .compute_obs_loss_iter = function(
      learner,
      task,
      train_ids,
      test_ids,
      features,
      measure,
      iters_refit = 1L,
      iteration
    ) {
      # Store/restore complete set of features as $feature_names will shrink otherwise
      features_total = task$feature_names
      # Ensure to reassign column roles to original state even if some model fit errors
      on.exit({
        task$col_roles$feature = features_total
      })

      # Collect both obs_losses and predictions
      obs_losses_list = list()
      predictions_list = list()

      for (iter_refit in seq_len(iters_refit)) {
        for (feature in features) {
          task$col_roles$feature = switch(
            self$direction,
            "leave-in" = feature,
            "leave-out" = setdiff(features_total, feature)
          )

          learner$reset()
          learner$train(task, row_ids = train_ids)
          pred = learner$predict(task, row_ids = test_ids)

          # Store prediction object
          predictions_list[[length(predictions_list) + 1]] = data.table(
            feature = feature,
            iteration = iteration,
            iter_refit = iter_refit,
            prediction = list(pred)
          )

          # Get observation-wise losses and add prediction column
          obs_losses = pred$obs_loss(measure)
          obs_losses[, ':='(
            feature = feature,
            iteration = iteration,
            iter_refit = iter_refit,
            response_feature = response # Store feature-specific prediction
          )]
          obs_losses_list[[length(obs_losses_list) + 1]] = obs_losses
        }
      }

      list(
        obs_losses = rbindlist(obs_losses_list),
        predictions = rbindlist(predictions_list)
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
#'   measure = msr("regr.mse"), obs_loss = TRUE
#' )
#' loco$compute()
#'
#' # Using observation-wise losses to compute the median instead
#' loco_obsloss = LOCO$new(
#'   task = task,
#'   learner = lrn("regr.ranger", num.trees = 50),
#'   measure = msr("regr.mae"), # to use absolute differences observation-wise
#'   obs_loss = TRUE,
#'   aggregation_fun = median
#' )
#' loco_obsloss$compute()
#' loco_obsloss$obs_losses
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
    #' @param iters_refit (`integer(1)`: `1L`) Number of refit iterations per resampling iteration.
    #' @param obs_loss (`logical(1)`: `FALSE`) Whether to use observation-wise loss calculation (original LOCO formulation). If `FALSE`, uses aggregated scores.
    #' @param aggregation_fun (`function`) Function to aggregate observation-wise losses when `obs_loss = TRUE`. Defaults to `median` for original LOCO formulation.
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      iters_refit = 1L,
      obs_loss = FALSE,
      aggregation_fun = median
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        direction = "leave-out",
        label = "Leave-One-Covariate-Out (LOCO)",
        iters_refit = iters_refit,
        obs_loss = obs_loss,
        aggregation_fun = aggregation_fun
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
    #' @param obs_loss (`logical(1)`) Whether to use observation-wise loss calculation (analogous to [LOCO]) when supported by the measure. If `FALSE` (default), uses aggregated scores.
    #' @param aggregation_fun (`function`) Function to aggregate observation-wise losses when `obs_loss = TRUE`. Defaults to `median`, analogous to [LOCO].
    initialize = function(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      iters_refit = 1L,
      obs_loss = FALSE,
      aggregation_fun = median
    ) {
      super$initialize(
        task = task,
        learner = learner,
        measure = measure,
        resampling = resampling,
        features = features,
        direction = "leave-in",
        label = "Leave-One-Covariate-In (LOCI)",
        iters_refit = iters_refit,
        obs_loss = obs_loss,
        aggregation_fun = aggregation_fun
      )
    }
  )
)
