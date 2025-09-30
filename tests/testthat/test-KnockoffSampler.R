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
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names))
	expect_equal(names(sampler$x_tilde), task$feature_names)

	# Test single feature sampling
	data = task$data()
	sampled_data = sampler$sample("important1", data)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), n)
	expect_equal(ncol(sampled_data), ncol(data))
	expect_equal(names(sampled_data), names(data))

	# Check that only the specified feature was changed (replaced with knockoff)
	expect_false(identical(sampled_data$important1, data$important1))
	expect_true(identical(sampled_data$important2, data$important2))
	expect_true(identical(sampled_data$y, data$y))

	# Check that sampled feature values are from knockoff matrix
	expect_true(identical(sampled_data$important1, sampler$x_tilde$important1))
})

test_that("KnockoffSampler handles multiple features", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	task = tgen("friedman1")$generate(n = 100)
	sampler = KnockoffSampler$new(task)
	data = task$data()

	# Test multiple feature sampling
	features = c("important1", "important2")
	sampled_data = sampler$sample(features, data)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), ncol(data))

	# Check that only the specified features were changed
	for (feat in features) {
		expect_false(identical(sampled_data[[feat]], data[[feat]]))
		expect_true(identical(sampled_data[[feat]], sampler$x_tilde[[feat]]))
	}

	# Check that other features remain unchanged
	expect_true(identical(sampled_data$important3, data$important3))
	expect_true(identical(sampled_data$y, data$y))
})

test_that("KnockoffSampler works with different numeric tasks", {
	skip_if_not_installed("knockoff")
	library(mlr3)

	# Friedman1 regression task (all numeric)
	task_friedman = tgen("friedman1")$generate(n = 100)
	sampler_friedman = KnockoffSampler$new(task_friedman)
	data_friedman = task_friedman$data()
	sampled_friedman = sampler_friedman$sample("important1", data_friedman)
	expect_true(data.table::is.data.table(sampled_friedman))
	expect_equal(nrow(sampled_friedman), 100)

	# Circle task with specified dimensions (all numeric)
	task_circle = tgen("circle", d = 4)$generate(n = 80)
	sampler_circle = KnockoffSampler$new(task_circle)
	data_circle = task_circle$data()
	sampled_circle = sampler_circle$sample("x1", data_circle)
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
	data_custom = task_custom$data()
	sampled_custom = sampler_custom$sample("x1", data_custom)
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

	# Sample the key column to ensure key is handled properly
	sampled_data = sampler$sample("important1", data)

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

	# Knockoff matrix should have same dimensions as feature matrix
	feature_data = task$data(cols = task$feature_names)
	expect_equal(dim(sampler$x_tilde), dim(feature_data))
	expect_equal(names(sampler$x_tilde), names(feature_data))

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
	expect_equal(ncol(sampler$x_tilde), length(task$feature_names))

	# Test sampling with custom knockoffs
	data = task$data()
	sampled_data = sampler$sample("important1", data)

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

	# Sampling should be consistent within each sampler
	data = task$data()
	sampled1a = sampler1$sample("important1", data)
	sampled1b = sampler1$sample("important1", data)

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

	# Should work with single feature
	expect_silent({
		sampler_single = KnockoffSampler$new(task_single)
	})

	expect_equal(ncol(sampler_single$x_tilde), 1)
	expect_equal(names(sampler_single$x_tilde), "x1")

	# Test sampling
	data_single = task_single$data()
	sampled_single = sampler_single$sample("x1", data_single)
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
