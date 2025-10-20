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

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		original_data = data,
		sampled_features = "x1",
		nrows = 100
	)

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

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		original_data = data,
		sampled_features = features,
		nrows = 100
	)

	# Check that the permuted values come from the original distribution
	for (feat in features) {
		expect_setequal(sampled_data[[feat]], data[[feat]])
	}
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

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 50
	)

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
	expect_sampler_output(sampled_regr, task_regr, nrows = 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_sampler_output(sampled_classif, task_classif, nrows = task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_sampler_output(sampled_multi, task_multi, nrows = 150)
})
