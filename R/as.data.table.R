#' @export
as.data.table.FeatureImportanceLearner = function(x, ...) {
  checkmate::assert_numeric(x$importance)

  data.table(
    feature = names(x$importance),
    importance = x$importance
  )
}
