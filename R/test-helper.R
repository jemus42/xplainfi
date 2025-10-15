# TODO: These should most likely be assertions that take a full
# FeatureImportanceMethod as an input to validate things more thoroughly.

#' Expectation for aggregated importance score tables
#'
#' Validates columns
#' - `feature` is a character value without missings
#' - `importance` is numeric vector without missings or infinite values (TODO: too strict?)
#' - Variance-related columns (se, estimate, conf_lower, conf_upper, statistic, p.value) may contain NA
#'
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
		min.cols = 2
	)

	# Core columns must not have missing values
	checkmate::expect_character(x$feature, any.missing = FALSE)
	checkmate::expect_numeric(x$importance, any.missing = FALSE)

	# Variance-related columns may contain NA (e.g., CPI test statistics can fail for some features)
	variance_cols = c("se", "sd", "estimate", "conf_lower", "conf_upper", "statistic", "p.value")
	for (col in intersect(variance_cols, colnames(x))) {
		checkmate::expect_numeric(x[[col]], any.missing = TRUE)
	}
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
