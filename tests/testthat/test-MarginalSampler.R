test_that("MarginalSampler works correctly", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalSampler$new(task)

	expect_true(inherits(sampler, "MarginalSampler"))
	expect_equal(sampler$label, "Marginal sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Test single feature sampling using row_ids (default: all rows)
	data = task$data()
	sampled_data = sampler$sample("x1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), task$n_features)
	expect_equal(names(sampled_data), task$feature_names)

	# Check that only the specified feature was permuted
	expect_false(identical(sampled_data$x1, data$x1))
	expect_true(identical(sampled_data$x2, data$x2))

	# Check that the permuted values come from the original distribution
	expect_setequal(sampled_data$x1, data$x1)
})

test_that("MarginalSampler handles multiple features", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalSampler$new(task)
	data = task$data()

	# Test multiple feature sampling using row_ids
	features = c("x1", "x2", "x3")
	sampled_data = sampler$sample(features)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), task$n_features)

	# Check that only the specified features were permuted
	for (feat in features) {
		expect_false(identical(sampled_data[[feat]], data[[feat]]))
		expect_setequal(sampled_data[[feat]], data[[feat]])
	}

	# Check that other features remain unchanged
	expect_true(identical(sampled_data$x4, data$x4))
})

test_that("MarginalSampler preserves data.table properties", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 50)
	sampler = MarginalSampler$new(task)
	data = task$data()

	# Add a key to the data.table
	setkey(data, x1)

	# Sample the key column using row_ids
	sampled_data = sampler$sample("x1")

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
	sampled_regr = sampler_regr$sample("x1")
	expect_true(data.table::is.data.table(sampled_regr))
	expect_equal(nrow(sampled_regr), 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_true(data.table::is.data.table(sampled_classif))
	expect_equal(nrow(sampled_classif), task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_true(data.table::is.data.table(sampled_multi))
	expect_equal(nrow(sampled_multi), 150)
})
