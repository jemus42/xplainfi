#' @export
as.data.table.FeatureImportanceLearner = function(x, ...) {
  if (!checkmate::test_numeric(x$importance)) {
    warning("No importance scores found. Did you $compute()?")
    return(data.table(feature = character(0), importance = numeric(0)))
  }

  data.table(
    feature = names(x$importance),
    importance = x$importance
  )
}
