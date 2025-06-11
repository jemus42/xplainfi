#' Combine two FeatureImportanceMeasure objects
#'
#' @param x,y `([FeatureImportanceMeasure])` Objects to combine. Must have computed scores.
#' @param ... (any) Ignored.
#' @return New object of the same subclass as `x` and `y`.
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#' }
c.FeatureImportanceMeasure = function(x, y, ...) {
  x$combine(y)
}
