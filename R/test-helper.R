#' Expectation for standard importance score vector
#'
#' Validates value
#' - is numeric vector without missings or infinite values (TODO: too strict?)
#' - has same length as feature vector `features`
#' -
#' @note This should probably be written like a custom expectation
#' (see [testthat docs](https://testthat.r-lib.org/articles/custom-expectation.html)).
#'
#'
#' @param x (numeric()) Vector of importance scores to test
#' @param task (character()) Feature names used to test names and order of importance scores.
#' @noRd
expect_importance_vec = function(x, features) {
  checkmate::expect_numeric(x, finite = TRUE, any.missing = FALSE, len = length(features))
  checkmate::expect_names(names(x), identical.to = features)
}

#' Expectation for standard importance score table
#'
#' Validates columns
#' - `feature` is a character value without missings
#' - `importance` is numeric vector without missings or infinite values (TODO: too strict?)
#' -
#' @note This should probably be written like a custom expectation
#' (see [testthat docs](https://testthat.r-lib.org/articles/custom-expectation.html)).
#'
#'
#' @param x (numeric()) Vector of importance scores to test
#' @param features (character()) Feature names used to test names and order of importance scores.
#' @noRd
expect_importance_dt = function(x, features) {
  checkmate::expect_data_table(
    x,
    types = c("character", "numeric"),
    nrows = length(features),
    ncols = 2,
    any.missing = FALSE
  )

  checkmate::expect_character(x$feature, any.missing = FALSE)
  checkmate::expect_numeric(x$importance, any.missing = FALSE)

}
