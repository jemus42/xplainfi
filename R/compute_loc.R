#' LOCO and LOCI workhorse
#'
#' Computes either LOCO or LOCI depending on whether feature of interest
#' is designated to be left out (LOCO) or left in (LOCI)
#' @param learner ([mlr3::Learner]) An untrained learner to use for prediction.
#' @param task ([mlr3::Task]) The task of interest.
#' @param train_ids,test_ids ([integer()]) Row IDs for train- and test splits.
#' @param measure ([mlr3::Measure]) The measure to use for scoring. May require observation-wise loss.
#' @param features ([character()]) Feature names of interest.
#' @param direction (character(1)) Either `"leave-in"` or `"leave-out"`.
#'
#' @return A [data.table] with columns `feature` and `<measure$id>_post`.
#' @export
#' @keywords internal
#' @note These functions are used for internal computations and are likely not exported in the future.
#'
#' @examples
#' library(mlr3)
#'
#' learner = lrn("classif.rpart")
#' task = tsk("breast_cancer")
#' measure = msr("classif.ce")
#' split = partition(task)
#' train_ids = split$train
#' test_ids = split$test
#' # Computing scores for all features
#' features = task$feature_names
#'
#' # LOCO
#' compute_loc(
#'   learner,
#'   task,
#'   train_ids = split$train,
#'   test_ids = split$test,
#'   features,
#'   measure,
#'   direction = "leave-out"
#' )
#' # LOCI
#' compute_loc(
#'   learner,
#'   task,
#'   train_ids = split$train,
#'   test_ids = split$test,
#'   features,
#'   measure,
#'   direction = "leave-in"
#' )
#'
compute_loc = function(
  learner,
  task,
  train_ids,
  test_ids,
  features,
  measure,
  direction
) {
  checkmate::assert_subset(direction, c("leave-in", "leave-out"))

  # Store/restore complete set of features as $feature_names will shrink otherwise
  features_total = task$feature_names
  on.exit({
    task$col_roles$feature = features_total
  })

  scores_post = vapply(
    features,
    \(feature) {
      # Get set of all features without current feature
      # task$col_roles$feature = setdiff(features_total, feature)

      task$col_roles$feature = switch(
        direction,
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

  res = data.table(
    feature = names(scores_post),
    scores_post = scores_post
  )
  res
}
