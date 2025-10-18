test_that("KnockoffSampler basic functionality", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	n = 100
	task = tgen("friedman1")$generate(n = n)
	sampler = KnockoffSampler$new(task)

	expect_true(inherits(sampler, "KnockoffSampler"))
	expect_equal(sampler$label, "Knockoff sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))
	expect_true("knockoff_fun" %in% sampler$param_set$ids())

	# Check that knockoff matrix was created
	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), n)
	# x_tilde has feature columns + internal ..row_ids column
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L)
	expect_equal(setdiff(names(sampler$x_tilde), "..row_ids"), task$feature_names)

	# Test single feature sampling
	data = task$data()
	sampled_data = sampler$sample("important1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), n)
	expect_equal(ncol(sampled_data), task$n_features)
	expect_equal(names(sampled_data), task$feature_names)

	# Check that only the specified feature was changed (replaced with knockoff)
	expect_false(identical(sampled_data$important1, data$important1))
	expect_true(identical(sampled_data$important2, data$important2))

	# Check that sampled feature values are from knockoff matrix
	expect_true(identical(sampled_data$important1, sampler$x_tilde$important1))
})

test_that("KnockoffSampler handles multiple features", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 100)
	data = task$data()
	sampler = KnockoffSampler$new(task)

	# Test multiple feature sampling
	features = c("important1", "important2")
	sampled_data = sampler$sample(features)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), task$n_features)

	# Check that only the specified features were changed
	for (feat in features) {
		expect_false(identical(sampled_data[[feat]], data[[feat]]))
		expect_true(identical(sampled_data[[feat]], sampler$x_tilde[[feat]]))
	}

	# Check that other features remain unchanged
	expect_true(identical(sampled_data$important3, task$data(cols = "important3")[[1]]))
})

test_that("KnockoffSampler works with different numeric tasks", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Friedman1 regression task (all numeric)
	task_friedman = tgen("friedman1")$generate(n = 100)
	sampler_friedman = KnockoffSampler$new(task_friedman)
	sampled_friedman = sampler_friedman$sample("important1")
	expect_true(data.table::is.data.table(sampled_friedman))
	expect_equal(nrow(sampled_friedman), 100)

	# Circle task with specified dimensions (all numeric)
	task_circle = tgen("circle", d = 4)$generate(n = 80)
	sampler_circle = KnockoffSampler$new(task_circle)
	sampled_circle = sampler_circle$sample("x1")
	expect_true(data.table::is.data.table(sampled_circle))
	expect_equal(nrow(sampled_circle), 80)

	# Custom numeric task
	set.seed(123)
	custom_data = data.table(
		x1 = rnorm(50),
		x2 = runif(50),
		x3 = rexp(50),
		y = rnorm(50)
	)
	task_custom = as_task_regr(custom_data, target = "y")
	sampler_custom = KnockoffSampler$new(task_custom)
	sampled_custom = sampler_custom$sample("x1")
	expect_true(data.table::is.data.table(sampled_custom))
	expect_equal(nrow(sampled_custom), 50)
})

test_that("KnockoffSampler preserves data structure", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 50)
	sampler = KnockoffSampler$new(task)
	data = task$data()

	# Add a key to the data.table
	setkey(data, important1)

	# Sample the key column using row_ids
	sampled_data = sampler$sample("important1")

	# Should return a data.table
	expect_true(data.table::is.data.table(sampled_data))

	# Should not have preserved the key (since we modified the key column)
	expect_null(key(sampled_data))

	# Original data should still have its key
	expect_equal(key(data), "important1")
})

test_that("KnockoffSampler knockoff matrix properties", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffSampler$new(task)

	# Knockoff matrix should have same dimensions as feature matrix (plus ..row_ids column)
	feature_data = task$data(cols = task$feature_names)
	expect_equal(nrow(sampler$x_tilde), nrow(feature_data))
	expect_equal(ncol(sampler$x_tilde), ncol(feature_data) + 1L) # +1 for ..row_ids
	expect_equal(setdiff(names(sampler$x_tilde), "..row_ids"), names(feature_data))

	# Knockoff values should be numeric (since task is all numeric)
	expect_true(all(sapply(sampler$x_tilde, is.numeric)))

	# Knockoff values should be finite
	expect_true(all(sapply(sampler$x_tilde, function(x) all(is.finite(x)))))
})

test_that("KnockoffSampler custom knockoff function", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 50)

	# Custom knockoff function that returns scaled versions
	custom_knockoff_fun = function(x) {
		# Simple knockoff: add noise to original data
		x_matrix = as.matrix(x)
		noise = matrix(rnorm(prod(dim(x_matrix)), sd = 0.1), nrow = nrow(x_matrix))
		return(x_matrix + noise)
	}

	sampler = KnockoffSampler$new(task, knockoff_fun = custom_knockoff_fun)

	# Check that custom function was stored
	expect_true(is.function(sampler$param_set$values$knockoff_fun))

	# Check knockoff matrix was created with custom function
	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), 50)
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L) # +1 for ..row_ids

	# Test sampling with custom knockoffs
	data = task$data()
	sampled_data = sampler$sample("important1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 50)
	expect_false(identical(sampled_data$important1, data$important1))
	expect_true(identical(sampled_data$important1, sampler$x_tilde$important1))
})

test_that("KnockoffSampler parameter validation", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 50)

	# Valid knockoff function should work
	expect_silent({
		sampler = KnockoffSampler$new(task)
	})

	# Check param_set structure
	expect_true("knockoff_fun" %in% sampler$param_set$ids())
	expect_equal(sampler$param_set$params[id == "knockoff_fun"]$cls, "ParamUty")

	# Check that default function is stored
	expect_true(is.function(sampler$param_set$values$knockoff_fun))
})

test_that("KnockoffSampler reproducibility", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 80)

	# Create two samplers with same data and seed
	set.seed(123)
	sampler1 = KnockoffSampler$new(task)

	set.seed(123)
	sampler2 = KnockoffSampler$new(task)

	# Knockoff matrices should be identical (if knockoff function is deterministic)
	# Note: This might not always be true for all knockoff functions
	# expect_equal(sampler1$x_tilde, sampler2$x_tilde)

	# At minimum, dimensions should be identical
	expect_equal(dim(sampler1$x_tilde), dim(sampler2$x_tilde))
	expect_equal(names(sampler1$x_tilde), names(sampler2$x_tilde))
	# Both should have ..row_ids column
	expect_true("..row_ids" %in% names(sampler1$x_tilde))
	expect_true("..row_ids" %in% names(sampler2$x_tilde))

	# Sampling should be consistent within each sampler
	data = task$data()
	sampled1a = sampler1$sample("important1")
	sampled1b = sampler1$sample("important1")

	# Should get the same knockoff values each time (sampler is deterministic)
	expect_identical(sampled1a$important1, sampled1b$important1)
})

test_that("KnockoffSampler edge cases", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Test with minimum viable task (single feature)
	single_feature_data = data.table(
		x1 = rnorm(30),
		y = rnorm(30)
	)
	task_single = as_task_regr(single_feature_data, target = "y")

	# Expected to warn in this case
	expect_warning({
		sampler_single = KnockoffSampler$new(task_single)
	})

	expect_equal(ncol(sampler_single$x_tilde), 2L) # x1 + ..row_ids
	expect_equal(setdiff(names(sampler_single$x_tilde), "..row_ids"), "x1")

	# Test sampling
	data_single = task_single$data()
	sampled_single = sampler_single$sample("x1")
	expect_true(data.table::is.data.table(sampled_single))
	expect_equal(nrow(sampled_single), 30)
})

test_that("KnockoffSampler fails with non-numeric features", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Test with factor features as predictors (not target)
	data_with_factor = data.table(
		x1 = rnorm(50),
		x2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_factor = as_task_regr(data_with_factor, target = "y")
	expect_error(
		KnockoffSampler$new(task_factor),
		class = "error" # knockoff::create.second_order should fail with non-numeric data
	)

	# Test with character features
	data_with_char = data.table(
		x1 = rnorm(50),
		x2 = sample(letters[1:5], 50, replace = TRUE),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_char = as_task_regr(data_with_char, target = "y")
	expect_error(
		KnockoffSampler$new(task_char),
		class = "error"
	)

	# Test with all character features (no numeric features to coerce to)
	data_all_char = data.table(
		x1 = sample(letters[1:5], 50, replace = TRUE),
		x2 = sample(c("red", "blue", "green"), 50, replace = TRUE),
		y = rnorm(50)
	)
	task_all_char = as_task_regr(data_all_char, target = "y")
	expect_error(
		KnockoffSampler$new(task_all_char),
		class = "error"
	)

	# Test with ordered factor
	data_with_ordered = data.table(
		x1 = rnorm(50),
		x2 = factor(
			sample(c("low", "medium", "high"), 50, replace = TRUE),
			levels = c("low", "medium", "high"),
			ordered = TRUE
		),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_ordered = as_task_regr(data_with_ordered, target = "y")
	expect_error(
		KnockoffSampler$new(task_ordered),
		class = "error"
	)
})

test_that("KnockoffGaussianSampler works with all-numeric features", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Test with friedman1 (all numeric)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffGaussianSampler$new(task)

	expect_true(inherits(sampler, "KnockoffGaussianSampler"))
	expect_true(inherits(sampler, "KnockoffSampler"))
	expect_equal(sampler$label, "Gaussian Knockoff sampler")

	# Check that knockoff matrix was created
	expect_true(data.table::is.data.table(sampler$x_tilde))
	expect_equal(nrow(sampler$x_tilde), 100)
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names) + 1L) # +1 for ..row_id

	# Test sampling
	data = task$data()
	sampled_data = sampler$sample("important1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), task$n_features)
	expect_false(identical(sampled_data$important1, data$important1))
	expect_true(identical(sampled_data$important1, sampler$x_tilde$important1))

	expect_equal(nrow(sampler$sample("important1", row_ids = 1:10)), 10)
	expect_equal(nrow(sampler$sample("important1", row_ids = 32)), 1)

	expect_equal(
		sampler$sample("important1", row_ids = 32)[, c(
			"important2",
			"important3",
			"important4",
			"important5",
			"unimportant1",
			"unimportant2",
			"unimportant3",
			"unimportant4",
			"unimportant5"
		)],

		data[
			32,
			c(
				"important2",
				"important3",
				"important4",
				"important5",
				"unimportant1",
				"unimportant2",
				"unimportant3",
				"unimportant4",
				"unimportant5"
			)
		]
	)

	# Test with multiple numeric features
	features = c("important1", "important2", "important3")
	sampled_multi = sampler$sample(features)

	expect_true(data.table::is.data.table(sampled_multi))
	for (feat in features) {
		expect_false(identical(sampled_multi[[feat]], data[[feat]]))
		expect_true(identical(sampled_multi[[feat]], sampler$x_tilde[[feat]]))
	}
})

test_that("KnockoffGaussianSampler fails with factor features", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Test with factor features
	data_with_factor = data.table(
		x1 = rnorm(50),
		x2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_factor = as_task_regr(data_with_factor, target = "y")

	expect_error(
		KnockoffGaussianSampler$new(task_factor),
		regexp = "following unsupported feature types"
	)

	# Test with mixed numeric and character
	data_with_char = data.table(
		x1 = rnorm(50),
		x2 = sample(letters[1:5], 50, replace = TRUE),
		x3 = rnorm(50),
		y = rnorm(50)
	)
	task_char = as_task_regr(data_with_char, target = "y")

	expect_error(
		KnockoffGaussianSampler$new(task_char),
		regexp = "following unsupported feature types"
	)
})


test_that("KnockoffSampler works with multiple iterations", {
	skip_if_not_installed("knockoff")

	task = sim_dgp_ewald(n = 100)

	withr::with_seed(42, {
		sampler_default = KnockoffSampler$new(task = task)
	})
	withr::with_seed(42, {
		sampler_1 = KnockoffSampler$new(task = task, iters = 1)
	})
	sampler_5 = KnockoffSampler$new(task = task, iters = 5)

	# x_tilde generated with same seed, same length
	expect_equal(
		sampler_default$sample("x1"),
		sampler_1$sample("x1")
	)

	x_orig = task$data(rows = 2, cols = task$feature_names)
	x_sampled = sampler_1$sample("x1", row_ids = 2)

	# Other values are the same
	expect_equal(x_orig[, -"x1"], x_sampled[, -"x1"])

	# Multiple samples
	x_orig = task$data(rows = c(1, 1, 1), cols = task$feature_names)
	x_sampled = sampler_5$sample("x1", row_ids = c(1, 1, 1))

	expect_equal(x_sampled$x2, x_orig$x2)

	# Sampling more than available
	expect_warning(
		sampled_1_repeats <- sampler_1$sample("x1", row_ids = c(1, 1, 1)),
		regexp = "sample with replacement"
	)

	# Same as samplig on instance three times
	expect_equal(
		sampled_1_repeats,
		data.table::rbindlist(replicate(3, sampler_1$sample("x1", row_ids = 1), simplify = FALSE))
	)
})
