#' @param x (numeric()) Vector of importance scores to test
#' @param task (character()) Feature names used to test names and order of importance scores.
expect_importance_vec = function(x, features) {

  if (!requireNamespace("testthat")) {
    stop("Install {testthat} for this expectation.")
  }

  checkmate::expect_numeric(
    x,
    finite = TRUE,
    any.missing = FALSE,
    len = length(features),
  )

  testthat::expect_identical(names(x), features)

}
