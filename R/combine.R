#' Combine two FeatureImportanceLearner objects
#'
#' @param x,y `([FeatureImportanceLearner])` Objects to combine. Must have computed scores.
#' @param ... (any) Ignored.
#' @return New object of the same subclass as `x` and `y`.
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#' }
c.FeatureImportanceLearner = function(x, y, ...) {
  x$combine(y)
}
