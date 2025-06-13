#' Combine two FeatureImportanceMeasure objects
#'
#' @param x,y `([FeatureImportanceMeasure])` Objects to combine. Must have computed scores.
#' @param ... (any) Ignored.
#' @return New object of the same subclass as `x` and `y`.
#' @export
#'
#' @examplesIf requireNamespace("ranger", quietly = TRUE) && requireNamespace("mlr3learners", quietly = TRUE)
#' library(mlr3)
#' task = tgen("2dnormals")$generate(n = 100)
#'
#' pfi1 = PFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce"),
#'   features = "x1"
#' )
#' pfi1$compute()
#'
#' pfi2 = PFI$new(
#'   task = task,
#'   learner = lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
#'   measure = msr("classif.ce"),
#'   features = "x2"
#' )
#' pfi2$compute()
#'
#' combined = c(pfi1, pfi2)
c.FeatureImportanceMeasure = function(x, y, ...) {
  x$combine(y)
}
