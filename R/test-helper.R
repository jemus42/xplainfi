#' Expectation for FeatureSampler output
#'
#' Validates that sampler output has the correct structure:
#' - Is a data.table
#' - Has correct number of columns (target + features)
#' - Has correct column names matching task
#' - Optionally validates that only specified features were changed
#' - Optionally validates that non-sampled features remain unchanged
#'
#' @param sampled_data (data.table()) Output from sampler's $sample() or $sample_newdata() method.
#' @param task (mlr3::Task) The task used for sampling.
#' @param original_data (data.table() | NULL) Optional original data to compare against.
#'   If provided, the function checks that non-sampled features match the original.
#' @param sampled_features (character()) Feature name(s) that were sampled.
#'   If provided along with original_data, validates these features differ from original.
#' @param nrows (integer(1) | NULL) Expected number of rows. If NULL, no check is performed.
#'
#' @return Invisibly returns TRUE if all checks pass, otherwise throws an error.
#' @noRd
expect_sampler_output = function(
	sampled_data,
	task,
	original_data = NULL,
	sampled_features = NULL,
	nrows = NULL
) {
	# Check data.table structure
	checkmate::expect_data_table(sampled_data)

	# Expected column structure: target + features
	expected_cols = c(task$target_names, task$feature_names)
	expected_ncol = length(expected_cols)

	# Check column count
	checkmate::expect_int(ncol(sampled_data), lower = 1)
	if (ncol(sampled_data) != expected_ncol) {
		cli::cli_abort(c(
			"Sampled data has wrong number of columns",
			"x" = "Expected {expected_ncol} columns (target + features), got {ncol(sampled_data)}",
			"i" = "Expected: {.val {expected_cols}}",
			"i" = "Got: {.val {names(sampled_data)}}"
		))
	}

	# Check column names match exactly
	if (!identical(names(sampled_data), expected_cols)) {
		cli::cli_abort(c(
			"Sampled data has wrong column names",
			"x" = "Column names do not match task structure",
			"i" = "Expected: {.val {expected_cols}}",
			"i" = "Got: {.val {names(sampled_data)}}"
		))
	}

	# Check row count if specified
	if (!is.null(nrows)) {
		checkmate::expect_int(nrow(sampled_data))
		if (nrow(sampled_data) != nrows) {
			cli::cli_abort(c(
				"Sampled data has wrong number of rows",
				"x" = "Expected {nrows} rows, got {nrow(sampled_data)}"
			))
		}
	}

	# If original data is provided, validate changes
	if (!is.null(original_data)) {
		checkmate::expect_data_table(original_data)

		if (nrow(sampled_data) != nrow(original_data)) {
			cli::cli_abort(c(
				"Sampled and original data have different number of rows",
				"x" = "Sampled: {nrow(sampled_data)}, Original: {nrow(original_data)}"
			))
		}

		# Check that sampled features actually differ
		if (!is.null(sampled_features)) {
			checkmate::expect_character(sampled_features, min.len = 1)
			checkmate::expect_subset(sampled_features, task$feature_names)

			for (feat in sampled_features) {
				if (identical(sampled_data[[feat]], original_data[[feat]])) {
					cli::cli_warn(c(
						"Sampled feature {.val {feat}} is identical to original",
						"i" = "This might indicate the sampler did not modify the feature"
					))
				}
			}

			# Check that non-sampled features remain unchanged
			non_sampled_features = setdiff(task$feature_names, sampled_features)
			for (feat in non_sampled_features) {
				if (!identical(sampled_data[[feat]], original_data[[feat]])) {
					cli::cli_abort(c(
						"Non-sampled feature {.val {feat}} was unexpectedly modified",
						"x" = "Only {.val {sampled_features}} should have been sampled"
					))
				}
			}

			# Check that target remains unchanged
			for (target_col in task$target_names) {
				if (!identical(sampled_data[[target_col]], original_data[[target_col]])) {
					cli::cli_abort(c(
						"Target column {.val {target_col}} was unexpectedly modified",
						"x" = "Target columns should never be modified by samplers"
					))
				}
			}
		}
	}

	invisible(TRUE)
}

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
