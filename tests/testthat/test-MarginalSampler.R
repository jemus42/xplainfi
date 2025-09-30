test_that("MarginalSampler works correctly", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalSampler$new(task)

	expect_true(inherits(sampler, "MarginalSampler"))
	expect_equal(sampler$label, "Marginal sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Test single feature sampling
	data = task$data()
	sampled_data = sampler$sample("x1", data)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), ncol(data))
	expect_equal(names(sampled_data), names(data))

	# Check that only the specified feature was permuted
	expect_false(identical(sampled_data$x1, data$x1))
	expect_true(identical(sampled_data$x2, data$x2))
	expect_true(identical(sampled_data$y, data$y))

	# Check that the permuted values come from the original distribution
	expect_setequal(sampled_data$x1, data$x1)
})

test_that("MarginalSampler handles multiple features", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalSampler$new(task)
	data = task$data()

	# Test multiple feature sampling
	features = c("x1", "x2", "x3")
	sampled_data = sampler$sample(features, data)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), ncol(data))

	# Check that only the specified features were permuted
	for (feat in features) {
		expect_false(identical(sampled_data[[feat]], data[[feat]]))
		expect_setequal(sampled_data[[feat]], data[[feat]])
	}

	# Check that other features remain unchanged
	expect_true(identical(sampled_data$x4, data$x4))
	expect_true(identical(sampled_data$y, data$y))
})

test_that("MarginalSampler preserves data.table properties", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 50)
	sampler = MarginalSampler$new(task)
	data = task$data()

	# Add a key to the data.table
	setkey(data, x1)

	# Sample the key column to ensure key is removed
	sampled_data = sampler$sample("x1", data)

	# Should return a data.table
	expect_true(data.table::is.data.table(sampled_data))

	# Should not have preserved the key (since we modified the key column)
	expect_null(key(sampled_data))

	# Original data should still have its key
	expect_equal(key(data), "x1")
})

test_that("MarginalSampler works with different task types", {
	library(mlr3)
	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = MarginalSampler$new(task_regr)
	data_regr = task_regr$data()
	sampled_regr = sampler_regr$sample("x1", data_regr)
	expect_true(data.table::is.data.table(sampled_regr))
	expect_equal(nrow(sampled_regr), 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalSampler$new(task_classif)
	data_classif = task_classif$data()
	sampled_classif = sampler_classif$sample("V1", data_classif)
	expect_true(data.table::is.data.table(sampled_classif))
	expect_equal(nrow(sampled_classif), nrow(data_classif))

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalSampler$new(task_multi)
	data_multi = task_multi$data()
	sampled_multi = sampler_multi$sample("Sepal.Length", data_multi)
	expect_true(data.table::is.data.table(sampled_multi))
	expect_equal(nrow(sampled_multi), 150)
})
