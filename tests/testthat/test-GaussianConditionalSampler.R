test_that("GaussianConditionalSampler initialization works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)

	expect_true(inherits(sampler, "GaussianConditionalSampler"))
	expect_true(inherits(sampler, "ConditionalSampler"))
	expect_equal(sampler$label, "Gaussian Conditional Sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Check stored statistics
	checkmate::expect_numeric(sampler$mu, len = task$n_features, any.missing = FALSE)
	checkmate::expect_matrix(sampler$sigma, nrows = task$n_features, ncols = task$n_features)
	expect_true(isSymmetric(sampler$sigma))
})

test_that("GaussianConditionalSampler rejects non-numeric tasks", {
	library(mlr3)
	task = tsk("penguins")  # Has factor features

	expect_error(
		GaussianConditionalSampler$new(task),
		"unsupported feature types"
	)
})

test_that("GaussianConditionalSampler marginal sampling works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)
	data = task$data()

	# Sample single feature without conditioning
	sampled_data = sampler$sample("important1", row_ids = 1:50)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 50
	)

	# important1 should be different from original (stochastic test)
	expect_false(identical(sampled_data$important1, data$important1[1:50]))
})

test_that("GaussianConditionalSampler conditional sampling with single conditioning feature", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)
	data = task$data()

	# Sample important2 | important1
	sampled_data = sampler$sample(
		feature = "important2",
		row_ids = 1:50,
		conditioning_set = "important1"
	)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		original_data = data[1:50],
		sampled_features = "important2",
		nrows = 50
	)

	# Conditioning feature should be unchanged
	expect_identical(sampled_data$important1, data$important1[1:50])
})

test_that("GaussianConditionalSampler conditional sampling with multiple features", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)
	data = task$data()

	# Sample important2, important3 | important1
	sampled_data = sampler$sample(
		feature = c("important2", "important3"),
		row_ids = 1:50,
		conditioning_set = "important1"
	)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		original_data = data[1:50],
		sampled_features = c("important2", "important3"),
		nrows = 50
	)

	# Conditioning feature unchanged
	expect_identical(sampled_data$important1, data$important1[1:50])
})

test_that("GaussianConditionalSampler sample_newdata works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)

	# Create external test data
	test_data = task$data(rows = 1:10)

	# Sample with conditioning
	sampled = sampler$sample_newdata(
		feature = c("important2", "important3"),
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(
		sampled_data = sampled,
		task = task,
		original_data = test_data,
		sampled_features = c("important2", "important3"),
		nrows = 10
	)

	# Conditioning feature unchanged
	expect_identical(sampled$important1, test_data$important1)
})

test_that("GaussianConditionalSampler handles single observation", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)

	test_data = task$data(rows = 1)

	sampled = sampler$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(sampled, task, nrows = 1)
	expect_identical(sampled$important1, test_data$important1)
})

test_that("GaussianConditionalSampler is reproducible with seed", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = GaussianConditionalSampler$new(task)
	test_data = task$data(rows = 1:10)

	sampled1 = withr::with_seed(123, {
		sampler$sample_newdata(
			feature = "important2",
			newdata = test_data,
			conditioning_set = "important1"
		)
	})

	sampled2 = withr::with_seed(123, {
		sampler$sample_newdata(
			feature = "important2",
			newdata = test_data,
			conditioning_set = "important1"
		)
	})

	expect_identical(sampled1$important2, sampled2$important2)
})

test_that("GaussianConditionalSampler conditioning_set parameter behavior", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	expect_conditioning_set_behavior(
		sampler_class = GaussianConditionalSampler,
		task = task
	)
})
