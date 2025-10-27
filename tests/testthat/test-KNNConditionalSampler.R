test_that("KNNConditionalSampler initialization works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)

	expect_true(inherits(sampler, "KNNConditionalSampler"))
	expect_true(inherits(sampler, "ConditionalSampler"))
	expect_equal(sampler$label, "k-Nearest Neighbors Conditional Sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Check k parameter
	expect_equal(sampler$param_set$values$k, 5L)
})

test_that("KNNConditionalSampler works with default k", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task) # Default k = 5L

	expect_equal(sampler$param_set$values$k, 5L)
})

test_that("KNNConditionalSampler marginal sampling works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)
	data = task$data()

	# Sample without conditioning
	sampled_data = sampler$sample("important1", row_ids = 1:50)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 50
	)

	# Values should come from training data
	expect_true(all(sampled_data$important1 %in% data$important1))
})

test_that("KNNConditionalSampler conditional sampling with single feature", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)
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

test_that("KNNConditionalSampler handles multiple features", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)
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

test_that("KNNConditionalSampler sample_newdata works", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)

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

test_that("KNNConditionalSampler handles different k values", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	# Test with k=1 (nearest neighbor)
	sampler_k1 = KNNConditionalSampler$new(task, k = 1L)
	expect_equal(sampler_k1$param_set$values$k, 1L)

	# Test with k=20
	sampler_k20 = KNNConditionalSampler$new(task, k = 20L)
	expect_equal(sampler_k20$param_set$values$k, 20L)

	test_data = task$data(rows = 1:10)

	# Both should work
	sampled_k1 = sampler_k1$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	sampled_k20 = sampler_k20$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(sampled_k1, task, nrows = 10)
	expect_sampler_output(sampled_k20, task, nrows = 10)
})

test_that("KNNConditionalSampler handles k > n_train", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 50)

	# k larger than training data size
	sampler = KNNConditionalSampler$new(task, k = 100L)

	test_data = task$data(rows = 1:5)

	# Should still work (uses all available training data)
	sampled = sampler$sample_newdata(
		feature = "important2",
		newdata = test_data,
		conditioning_set = "important1"
	)

	expect_sampler_output(sampled, task, nrows = 5)
})

test_that("KNNConditionalSampler is reproducible", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)
	sampler = KNNConditionalSampler$new(task, k = 5L)
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

test_that("KNNConditionalSampler works with mixed feature types", {
	library(mlr3)

	# Create task with mixed types
	task = tgen("circle", d = 5)$generate(n = 100)

	# kNN supports mixed types
	sampler = KNNConditionalSampler$new(task, k = 5L)

	expect_true(inherits(sampler, "KNNConditionalSampler"))

	test_data = task$data(rows = 1:10)

	sampled = sampler$sample_newdata(
		feature = "x2",
		newdata = test_data,
		conditioning_set = "x1"
	)

	expect_sampler_output(sampled, task, nrows = 10)
})

test_that("KNNConditionalSampler conditioning_set parameter behavior", {
	library(mlr3)
	task = tgen("friedman1")$generate(n = 100)

	expect_conditioning_set_behavior(
		sampler_class = KNNConditionalSampler,
		task = task,
		k = 5L
	)
})

test_that("KNNConditionalSampler works with categorical features using Gower distance", {
	skip_if_not_installed("gower")
	library(mlr3)

	# Use penguins task which has factor features
	task = tsk("penguins")
	data = task$data()

	# Create sampler
	sampler = KNNConditionalSampler$new(task, k = 5L)

	# Sample conditioning on categorical feature
	test_data = task$data(rows = 1:10)
	sampled = sampler$sample_newdata(
		feature = "bill_length",
		newdata = test_data,
		conditioning_set = "island" # factor feature
	)

	expect_sampler_output(
		sampled_data = sampled,
		task = task,
		original_data = test_data,
		sampled_features = "bill_length",
		nrows = 10
	)

	# Conditioning feature unchanged
	expect_identical(sampled$island, test_data$island)

	# Sampled values should come from training data
	expect_true(all(sampled$bill_length %in% data$bill_length))
})

test_that("KNNConditionalSampler works with mixed numeric and categorical conditioning", {
	skip_if_not_installed("gower")
	library(mlr3)

	task = tsk("penguins")
	data = task$data()

	sampler = KNNConditionalSampler$new(task, k = 5L)

	# Sample conditioning on both numeric and factor features
	test_data = task$data(rows = 1:10)
	sampled = sampler$sample_newdata(
		feature = "bill_length",
		newdata = test_data,
		conditioning_set = c("island", "body_mass") # factor + integer
	)

	expect_sampler_output(
		sampled_data = sampled,
		task = task,
		original_data = test_data,
		sampled_features = "bill_length",
		nrows = 10
	)

	# Conditioning features unchanged
	expect_identical(sampled$island, test_data$island)
	expect_identical(sampled$body_mass, test_data$body_mass)
})
