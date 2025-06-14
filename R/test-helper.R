# TODO: These should most likely be assertions that take a full
# FeatureImportanceMeasure as an input to validate things more thoroughly.

#' Expectation for aggregated importance score tables
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
    min.cols = 2,
    any.missing = FALSE
  )

  checkmate::expect_character(x$feature, any.missing = FALSE)
  checkmate::expect_numeric(x$importance, any.missing = FALSE)

  if ("sd" %in% colnames(x)) checkmate::expect_numeric(x$sd, any.missing = FALSE)
}

#' Expectation for individual importance score tables
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
expect_score_dt = function(x, features) {
  checkmate::expect_data_table(
    x,
    types = c("character", "numeric"),
    min.rows = length(features),
    min.cols = 5,
    any.missing = FALSE,
    key = c("feature", "iter_rsmp")
  )

  checkmate::expect_character(x$feature, any.missing = FALSE)
  checkmate::expect_numeric(x$importance, any.missing = FALSE)
}
