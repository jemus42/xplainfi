#' Feature Importance Method Class
#'
#' @export
FeatureImportanceMethod = R6Class(
  "FeatureImportanceMethod",
  public = list(
    #' @field label (`character(1)`) Method label.
    label = NA_character_,
    #' @field task ([mlr3::Task])
    task = NULL,
    #' @field learner ([mlr3::Learner])
    learner = NULL,
    #' @field measure ([mlr3::Measure])
    measure = NULL,
    #' @field resampling ([mlr3::Resampling])
    resampling = NULL,
    #' @field resample_result ([mlr3::ResampleResult])
    resample_result = NULL,
    # TODO: list of features, for grouped importance
    #' @field features (`character`)
    features = NULL,
    #' @field param_set ([paradox::ps()])
    param_set = ps(),
    #' @field importance ([data.table][data.table::data.table]) Aggregated importance scores
    importance = NULL,
    #' @field scores ([data.table][data.table::data.table]) Individual performance scores used to compute `$importance` per resampling iteration and permutation iteration.
    scores = NULL,
    #' @field obs_losses ([data.table][data.table::data.table]) Observation-wise losses when available (e.g., when using obs_loss = TRUE). Contains columns for row_ids, feature, iteration indices, individual loss values, and both reference and feature-specific predictions.
    obs_losses = NULL,
    #' @field predictions ([data.table][data.table::data.table]) Feature-specific prediction objects when using obs_loss = TRUE. Contains columns for feature, iteration, iter_refit, and prediction objects. Similar to ResampleResult$predictions() but extended for feature-specific models.
    predictions = NULL,

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
      if (is.null(measure)) {
        self$measure = switch(
          task$task_type,
          "classif" = mlr3::msr("classif.ce"),
          "regr" = mlr3::msr("regr.mse")
        )
        cli::cli_alert_info(
          "No {.cls Measure} provided, using {.code measure = msr({self$measure$id})}"
        )
      } else {
        self$measure = mlr3::assert_measure(measure, task = task, learner = learner)
      }
      self$param_set = paradox::assert_param_set(param_set)
      self$label = checkmate::assert_string(label, min.chars = 1)
      self$features = features %||% self$task$feature_names

      # resampling: default to holdout with default ratio if NULL
      if (is.null(resampling)) {
        resampling = mlr3::rsmp("holdout")$instantiate(task)
        cli::cli_inform(c(
          i = "No {.cls Resampling} provided",
          "Using {.code resampling = rsmp(\"holdout\")} with default {.code ratio = {round(resampling$param_set$values$ratio, 2)}}."
        ))
      }
      if (!resampling$is_instantiated) {
        resampling$instantiate(task)
      }
      self$resampling = mlr3::assert_resampling(resampling)
    },

    #' @description
    #' Compute feature importance scores
    #' @param relation (`character(1): "difference"`) How to relate perturbed scores to originals ("difference" or "ratio")
    #' @param store_backends (`logical(1): TRUE`) Whether to store backends.
    compute = function(relation = c("difference", "ratio"), store_backends = TRUE) {
      stop("Abstract method. Use a concrete implementation.")
    },

    #' @description
    #' Combine two `FeatureImportanceMethod` objects with computed scores.
    #'
    #' @param y ([FeatureImportanceMethod]) Object to combine. Must have computed scores.
    #' @param ... (any) Unused.
    #' @return A new [FeatureImportanceMethod] of the same subclass as `x` and `y`.
    #' Currently this method merges the following:
    #' - `$scores` is combined, with `iter_rsmp` increased for `y`.
    #' - `$importance` is re-computed from the combined `$scores`.
    #' - `$resample_result` is combined to a [mlr3::BenchmarkResult]
    #' - `$resampling` is combined into a [mlr3::ResamplingCustom], again continuing te `iteration` count from `x` with that of `y`.
    combine = function(y, ...) {
      checkmate::assert_class(self, classes = "FeatureImportanceMethod")
      checkmate::assert_class(y, classes = "FeatureImportanceMethod")
      checkmate::assert_true(class(self)[[1]] == class(y)[[1]], .var.name = "Identical subclasses")
      checkmate::assert_data_table(self$importance, key = "feature")
      checkmate::assert_data_table(y$importance, key = "feature")

      checkmate::assert_true(self$task$hash == y$task$hash, .var.name = "identical tasks")
      checkmate::assert_true(self$measure$hash == y$measure$hash, .var.name = "identical measures")
      checkmate::assert_true(self$learner$hash == y$learner$hash, .var.name = "identical learners")

      # merge importance scores
      scores_y = copy(y$scores)
      # Increase iteration count for y for consistency
      scores_y[, iter_rsmp := iter_rsmp + self$resampling$iters]
      scores = rbindlist(list(self$scores, scores_y))
      setkeyv(scores, c("feature", "iter_rsmp"))

      # Merge aggregated cores
      importance = scores[, list(importance = mean(importance)), by = feature]

      # Combine obs_losses if available
      obs_losses = NULL
      if (!is.null(self$obs_losses) || !is.null(y$obs_losses)) {
        if (!is.null(self$obs_losses) && !is.null(y$obs_losses)) {
          obs_losses_y = copy(y$obs_losses)
          # Increase iteration count for y for consistency
          obs_losses_y[, iteration := iteration + self$resampling$iters]
          obs_losses = rbindlist(list(self$obs_losses, obs_losses_y))
        } else if (!is.null(self$obs_losses)) {
          obs_losses = copy(self$obs_losses)
        } else {
          obs_losses = copy(y$obs_losses)
        }
      }

      # Combine predictions if available
      predictions = NULL
      if (!is.null(self$predictions) || !is.null(y$predictions)) {
        if (!is.null(self$predictions) && !is.null(y$predictions)) {
          predictions_y = copy(y$predictions)
          # Increase iteration count for y for consistency
          predictions_y[, iteration := iteration + self$resampling$iters]
          predictions = rbindlist(list(self$predictions, predictions_y))
        } else if (!is.null(self$predictions)) {
          predictions = copy(self$predictions)
        } else {
          predictions = copy(y$predictions)
        }
      }

      # Modify
      self$scores = scores
      self$importance = importance
      self$obs_losses = obs_losses
      self$predictions = predictions
      self$resample_result = c(self$resample_result, y$resample_result)

      # Combine resampling objects
      rsmp_x = as.data.table(self$resampling)
      rsmp_y = as.data.table(y$resampling)
      rsmp_y[, iteration := iteration + self$resampling$iters]
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
    #' Resets all stored fields populated by `$compute`: `$resample_result`, `$importance`, `$scores`, `$obs_losses`, and `$predictions`.
    reset = function() {
      self$resample_result = NULL
      self$importance = NULL
      self$scores = NULL
      self$obs_losses = NULL
      self$predictions = NULL
    },

    #' @description
    #' Print importance scores
    #'
    #' @param ... Passed to `print()`
    print = function(...) {
      cli::cli_h2(self$label)
      cli::cli_ul()
      cli::cli_li("Feature{?s} of interest: {.val {self$features}}")
      cli::cli_li("Parameters:")

      pidx = seq_along(self$param_set$values)
      sapply(pidx, \(i) {
        cli::cli_ul("{.code {names(self$param_set$values)[i]}}: {.val {self$param_set$values[i]}}")
      })

      cli::cli_end()
      if (!is.null(self$importance)) {
        print(self$importance, ...)
      } else {
        cli::cli_inform("No importances computed yet.")
      }
    }
  ),
  private = list(
    .aggregate_importances = function(xdf, include_sd = TRUE) {
      checkmate::assert_data_table(xdf)
      checkmate::assert_subset(c("feature", "importance"), choices = colnames(xdf))

      # Skip aggregation if only one row per feature anyway
      if (nrow(xdf) == length(unique(xdf$feature))) {
        res = xdf[, list(feature, importance)]
        setkeyv(res, "feature")
        return(res)
      }

      res = xdf[, list(importance = mean(importance)), by = feature]

      if (include_sd) {
        sd = xdf[, list(sd = sd(importance)), by = feature]

        res = res[sd, on = "feature"]
      }

      setkeyv(res, "feature")
      res
    },

    # Scoring utility for computing importance scores
    # Computes the relation of score before a change (e.g. PFI, LOCO, ...) and after.
    # If minimize == TRUE, then scores_post - scores_pre is computed for
    # relation == "difference", otherwise scores_pre - scores_post is given.
    # If minimize == FALSE, then scores_pre - scores_post is computed.
    .compute_score = function(
      scores_pre,
      scores_post,
      relation = c("difference", "ratio"),
      minimize = TRUE
    ) {
      checkmate::assert_numeric(scores_pre, any.missing = FALSE)
      checkmate::assert_numeric(scores_post, any.missing = FALSE)
      checkmate::assert_true(length(scores_pre) == length(scores_post))
      checkmate::assert_flag(minimize)
      relation = match.arg(relation)

      if (minimize) {
        # Lower is better, e.g. ce
        switch(relation, difference = scores_post - scores_pre, ratio = scores_post / scores_pre)
      } else {
        # Higher is better, e.g. accuracy
        switch(relation, difference = scores_pre - scores_post, ratio = scores_pre / scores_post)
      }
    }
  )
)
