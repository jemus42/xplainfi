#' PFI Class
#'
#' Calculates Permutation Feature Importance (PFI) scores.
#'
#' @export
#'
#' @examplesIf requireNamespace("mlr3learners")
#'
#' library(mlr3)
#' library(mlr3learners)
#'
#' task = tsk("zoo")
#' learner = lrn("classif.ranger", num.trees = 100)
#' resampling = rsmp("holdout")
#' resampling$instantiate(task)
#' measure = msr("classif.ce")
#'
#' learner$train(task, row_ids = resampling$train_set(1))
#'
#' pfi = PFI$new(
#'   task = task,
#'   learner = learner,
#'   resampling = resampling,
#'   measure = measure
#' )
#'
#' pfi$compute()
PFI = R6::R6Class("PFI",
  inherit = FeatureImportanceLearner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param ... Passed to super class.
    initialize = function(...) {

      ps = paradox::ps(
        relation = paradox::p_fct(c("difference", "ratio"), default = "difference")
      )

      ps$values = list(relation = "difference")

      super$initialize(
        ...,
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
        message("Already computed")
        return(self$importance)
      }
      # Store relation
      self$param_set$values$relation = relation

      loss_orig = self$learner$predict(task)$score(measure)

      loss_permuted = vapply(self$features, \(feature) {

        # Copying task for every feature, not great
        task_data = task$data(data_format = "data.table")

        # Permute in-place
        task_data[, (feature) := sample(.SD[[feature]])][]

        # Use predict_newdata to avoid having to reconstruct a new Task
        pred = self$learner$predict_newdata(newdata = task_data, task = self$task)

        score = pred$score(self$measure)
        names(score) = feature
        score

      }, FUN.VALUE = numeric(1))

      if (self$measure$minimize) {
        # Smaller is better, e.g. ce
        self$importance = switch(relation,
                            difference = loss_permuted - loss_orig,
                            ratio = loss_permuted / loss_orig
        )
      } else {
        # Higher is better, e.g. accuracy
        self$importance = switch(relation,
                            difference = loss_orig - loss_permuted,
                            ratio = loss_orig / loss_permuted
        )
      }

      self
    }
  ),

  private = list()
)
