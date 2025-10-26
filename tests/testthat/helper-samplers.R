# Helper functions for testing samplers

#' Test conditioning_set parameter behavior for conditional samplers
#'
#' Verifies that a conditional sampler correctly:
#' 1. Stores conditioning_set in param_set when provided during initialization
#' 2. Can sample without specifying conditioning_set (uses stored value)
#' 3. Can override conditioning_set in $sample() calls
#' 4. Handles NULL conditioning_set (defaults to all other features - critical for CFI)
#' 5. Handles empty conditioning_set character(0) (marginal sampling - no conditioning)
#'
#' @param sampler_class R6 class for the sampler to test
#' @param task mlr3 Task to use for testing (must have at least 3 features)
#' @param ... Additional arguments passed to sampler constructor
#'
#' @return NULL (used for side effects via testthat expectations)
expect_conditioning_set_behavior = function(sampler_class, task, ...) {
	# Get feature names for testing
	features = task$feature_names
	checkmate::assert_true(length(features) >= 3, .var.name = "task must have at least 3 features")

	target_feature = features[1]
	cond_set_1 = features[2]
	cond_set_2 = features[3]
	other_features = setdiff(features, target_feature)

	# Test 1: conditioning_set stored in param_set when provided
	sampler_with_cond = sampler_class$new(task, conditioning_set = cond_set_1, ...)
	testthat::expect_identical(
		sampler_with_cond$param_set$values$conditioning_set,
		cond_set_1,
		info = "conditioning_set should be stored in param_set"
	)

	# Test 2: Can sample using stored conditioning_set
	original_data = task$data(rows = 1:5)
	result_stored = sampler_with_cond$sample(
		feature = target_feature,
		row_ids = 1:5
	)
	checkmate::expect_data_table(
		result_stored,
		nrows = 5,
		info = "Should sample successfully using stored conditioning_set"
	)

	# Verify conditioning features remain unchanged
	testthat::expect_identical(
		result_stored[[cond_set_1]],
		original_data[[cond_set_1]],
		info = "Conditioning features should remain unchanged"
	)

	# Verify target feature was actually sampled (likely different from original)
	# Note: This could theoretically fail if sampled values happen to match original,
	# but probability is very low with sufficient feature variability
	if (is.numeric(original_data[[target_feature]])) {
		# For numeric features, expect at least some values to differ
		n_different = sum(result_stored[[target_feature]] != original_data[[target_feature]])
		testthat::expect_true(
			n_different > 0,
			info = "Target feature should be sampled (at least some values should differ)"
		)
	}

	# Test 3: Can override conditioning_set in $sample() call
	result_override = sampler_with_cond$sample(
		feature = target_feature,
		row_ids = 1:5,
		conditioning_set = cond_set_2
	)
	checkmate::expect_data_table(
		result_override,
		nrows = 5,
		info = "Should sample successfully when overriding conditioning_set"
	)

	# Verify the overridden conditioning feature remains unchanged
	testthat::expect_identical(
		result_override[[cond_set_2]],
		original_data[[cond_set_2]],
		info = "Overridden conditioning feature should remain unchanged"
	)

	# Verify the original conditioning feature may change (it's now a target)
	# This demonstrates that the override actually took effect

	# Test 4: NULL conditioning_set during initialization
	sampler_no_cond = sampler_class$new(task, ...)
	testthat::expect_null(
		sampler_no_cond$param_set$values$conditioning_set,
		info = "conditioning_set should be NULL when not provided"
	)

	# Test 5: Can specify conditioning_set in $sample() when not set during init
	result_specified = sampler_no_cond$sample(
		feature = target_feature,
		row_ids = 1:5,
		conditioning_set = cond_set_1
	)
	checkmate::expect_data_table(
		result_specified,
		nrows = 5,
		info = "Should sample successfully when specifying conditioning_set in $sample()"
	)

	# Verify conditioning feature remains unchanged even when specified at call time
	testthat::expect_identical(
		result_specified[[cond_set_1]],
		original_data[[cond_set_1]],
		info = "Conditioning feature specified in $sample() should remain unchanged"
	)

	# Test 6: NULL conditioning_set should default to all other features
	# This is critical for CFI - when no conditioning_set is specified,
	# condition on everything except the target feature
	#
	# Strategy: Use debug mode to directly verify the resolved conditioning_set value
	# This is much more reliable than checking stochastic sampling behavior

	# Test with conditioning_set = NULL (should resolve to all other features)
	messages_null = capture.output(
		withr::with_options(
			list(xplain.debug = TRUE),
			sampler_no_cond$sample(
				feature = target_feature,
				row_ids = 1:5,
				conditioning_set = NULL
			)
		),
		type = "message"
	)

	# Extract the resolved conditioning_set from debug output
	# Expected format: "Resolved conditioning_set: other_features"
	resolved_null = paste(messages_null, collapse = " ")

	testthat::expect_true(
		nchar(resolved_null) > 0 && grepl("Resolved conditioning_set:", resolved_null, fixed = TRUE),
		info = glue::glue("Debug output should contain resolved conditioning_set for NULL. Got: {resolved_null}")
	)

	# Verify that with NULL, all other features are in the conditioning set
	for (feat in other_features) {
		testthat::expect_true(
			grepl(feat, resolved_null, fixed = TRUE),
			info = glue::glue(
				"With conditioning_set=NULL, '{feat}' should be in the resolved conditioning_set. ",
				"Debug output: {resolved_null}"
			)
		)
	}

	# Test with conditioning_set = character(0) (should remain empty for marginal sampling)
	messages_empty = capture.output(
		withr::with_options(
			list(xplain.debug = TRUE),
			sampler_no_cond$sample(
				feature = target_feature,
				row_ids = 1:5,
				conditioning_set = character(0)
			)
		),
		type = "message"
	)

	# Extract the resolved conditioning_set from debug output
	resolved_empty = paste(messages_empty, collapse = " ")

	testthat::expect_true(
		nchar(resolved_empty) > 0 && grepl("Resolved conditioning_set:", resolved_empty, fixed = TRUE),
		info = glue::glue("Debug output should contain resolved conditioning_set for character(0). Got: {resolved_empty}")
	)

	# Verify that character(0) results in empty/no conditioning features
	# With character(0), none of the other features should appear in the debug output
	for (feat in other_features) {
		testthat::expect_false(
			grepl(feat, resolved_empty, fixed = TRUE),
			info = glue::glue(
				"With conditioning_set=character(0), '{feat}' should NOT be in the resolved conditioning_set. ",
				"Debug output: {resolved_empty}"
			)
		)
	}

	invisible(NULL)
}

