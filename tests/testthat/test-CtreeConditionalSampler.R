test_that("CtreeConditionalSampler initialization works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)

	expect_true(inherits(sampler, "CtreeConditionalSampler"))
	expect_true(inherits(sampler, "ConditionalSampler"))
	expect_equal(sampler$label, "Conditional Inference Tree Conditional Sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Check default parameters
	expect_equal(sampler$param_set$values$mincriterion, 0.95)
	expect_equal(sampler$param_set$values$minsplit, 20L)
	expect_equal(sampler$param_set$values$minbucket, 7L)
	expect_equal(sampler$param_set$values$use_cache, TRUE)

	# Check tree cache initialized
	expect_true(inherits(sampler$tree_cache, "environment"))
})

test_that("CtreeConditionalSampler works with custom parameters", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	sampler = CtreeConditionalSampler$new(
		task,
		mincriterion = 0.90,
		minsplit = 10L,
		minbucket = 5L,
		use_cache = FALSE
	)

	expect_equal(sampler$param_set$values$mincriterion, 0.90)
	expect_equal(sampler$param_set$values$minsplit, 10L)
	expect_equal(sampler$param_set$values$minbucket, 5L)
	expect_equal(sampler$param_set$values$use_cache, FALSE)
})

test_that("CtreeConditionalSampler marginal sampling works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)
	data = task$data()

	# Sample without conditioning
	sampled_data = sampler$sample("important1", row_ids = 1:50)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 50
	)

	# Values should come from training data (hot-deck property)
	expect_true(all(sampled_data$important1 %in% data$important1))
})

test_that("CtreeConditionalSampler conditional sampling with single feature", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)
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

	# Conditioning feature unchanged
	expect_identical(sampled_data$important1, data$important1[1:50])

	# Sampled values should come from training data
	expect_true(all(sampled_data$important2 %in% data$important2))
})

test_that("CtreeConditionalSampler handles multiple features", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)
	data = task$data()

	# Sample multiple features conditionally
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

test_that("CtreeConditionalSampler sample_newdata works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)

	test_data = task$data(rows = 1:10)

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

test_that("CtreeConditionalSampler caching works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	# With caching enabled
	sampler_cached = CtreeConditionalSampler$new(task, use_cache = TRUE)
	test_data = task$data(rows = 1:10)

	# First call should build tree
	expect_equal(length(ls(sampler_cached$tree_cache)), 0)
	sampled1 = sampler_cached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_equal(length(ls(sampler_cached$tree_cache)), 1)

	# Second call should use cached tree
	sampled2 = sampler_cached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_equal(length(ls(sampler_cached$tree_cache)), 1)

	# Without caching
	sampler_uncached = CtreeConditionalSampler$new(task, use_cache = FALSE)
	sampled3 = sampler_uncached$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)
	expect_equal(length(ls(sampler_uncached$tree_cache)), 0)
})

test_that("CtreeConditionalSampler handles different parameter values", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	# More restrictive parameters (fewer splits)
	sampler_restrictive = CtreeConditionalSampler$new(
		task,
		mincriterion = 0.99,
		minsplit = 50L,
		minbucket = 20L
	)

	# Less restrictive parameters (more splits)
	sampler_permissive = CtreeConditionalSampler$new(
		task,
		mincriterion = 0.90,
		minsplit = 10L,
		minbucket = 5L
	)

	test_data = task$data(rows = 1:10)

	# Both should work
	sampled_restrictive = sampler_restrictive$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	sampled_permissive = sampler_permissive$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(sampled_restrictive, task, nrows = 10)
	expect_sampler_output(sampled_permissive, task, nrows = 10)
})

test_that("CtreeConditionalSampler handles single observation", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)

	test_data = task$data(rows = 1)

	sampled = sampler$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(sampled, task, nrows = 1)
	expect_identical(sampled$important1, test_data$important1)
})

test_that("CtreeConditionalSampler is reproducible", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)
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

test_that("CtreeConditionalSampler works with mixed feature types", {
	library(mlr3)

	# Create task with mixed types
	task = tgen("circle", d = 5)$generate(n = 100)

	# ctree supports mixed types
	sampler = CtreeConditionalSampler$new(task)

	expect_true(inherits(sampler, "CtreeConditionalSampler"))

	test_data = task$data(rows = 1:10)

	sampled = sampler$sample_newdata(
		feature = "x2",
		newdata = test_data,
		conditioning_set = "x1"
	)

	expect_sampler_output(sampled, task, nrows = 10)
})

test_that("CtreeConditionalSampler handles multiple conditioning features", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = CtreeConditionalSampler$new(task)
	data = task$data()

	# Sample one feature conditioned on multiple features
	sampled_data = sampler$sample(
		feature = "important3",
		row_ids = 1:50,
		conditioning_set = c("important1", "important2")
	)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		original_data = data[1:50],
		sampled_features = "important3",
		nrows = 50
	)

	# Conditioning features unchanged
	expect_identical(sampled_data$important1, data$important1[1:50])
	expect_identical(sampled_data$important2, data$important2[1:50])
})
