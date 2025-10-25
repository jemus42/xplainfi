test_that("MarginalReferenceSampler works correctly", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)

	expect_true(inherits(sampler, "MarginalReferenceSampler"))
	expect_equal(sampler$label, "Marginal reference sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))

	# Reference data should be stored
	expect_true(inherits(sampler$reference_data, "data.table"))
	expect_equal(nrow(sampler$reference_data), 100)
	expect_equal(names(sampler$reference_data), task$feature_names)

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

	# Sampled values should come from reference data
	expect_true(all(sampled_data$x1 %in% sampler$reference_data$x1))
})

test_that("MarginalReferenceSampler handles multiple features", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)
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

	# Sampled values should come from reference data
	for (feat in features) {
		expect_true(all(sampled_data[[feat]] %in% sampler$reference_data[[feat]]))
	}
})

test_that("MarginalReferenceSampler with n_samples", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)

	# Create sampler with subsampled reference data
	sampler = MarginalReferenceSampler$new(task, n_samples = 50L)

	expect_equal(nrow(sampler$reference_data), 50)

	# Sample and verify
	sampled_data = sampler$sample("x1", row_ids = 1:10)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 10
	)

	# All sampled values should be from the task (reference is subset of task)
	expect_true(all(sampled_data$x1 %in% task$data()$x1))
})

test_that("MarginalReferenceSampler sample_newdata works", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = MarginalReferenceSampler$new(task)

	# Create new test data
	newdata = tgen("circle", d = 5)$generate(n = 20)$data()
	sampled_data = sampler$sample_newdata("x1", newdata = newdata)

	expect_sampler_output(
		sampled_data = sampled_data,
		task = task,
		nrows = 20
	)

	# Sampled values should come from reference data
	expect_true(all(sampled_data$x1 %in% sampler$reference_data$x1))

	# Original newdata should be unchanged
	expect_equal(nrow(newdata), 20)
})

test_that("MarginalReferenceSampler preserves within-row dependencies", {
	library(mlr3)
	# Create a task where features are perfectly correlated within reference data
	set.seed(123)
	n = 100
	x1 = rnorm(n)
	data = data.table(
		x1 = x1,
		x2 = x1 * 2, # Perfect correlation
		x3 = x1 * 3,
		y = x1 + rnorm(n, sd = 0.1)
	)
	task = as_task_regr(data, target = "y")

	sampler = MarginalReferenceSampler$new(task)

	# Sample multiple correlated features
	sampled_data = sampler$sample(c("x1", "x2", "x3"), row_ids = 1:50)

	# Within each sampled row, the relationships should be preserved
	# Because we sample complete rows from reference data
	expect_true(all(abs(sampled_data$x2 - sampled_data$x1 * 2) < 1e-10))
	expect_true(all(abs(sampled_data$x3 - sampled_data$x1 * 3) < 1e-10))
})

test_that("MarginalReferenceSampler vs PermutationSampler difference", {
	library(mlr3)
	# Create a task where features are correlated
	set.seed(123)
	n = 100
	x1 = rnorm(n)
	data = data.table(
		x1 = x1,
		x2 = x1 * 2 + rnorm(n, sd = 0.1),
		y = x1 + rnorm(n, sd = 0.1)
	)
	task = as_task_regr(data, target = "y")

	# MarginalReferenceSampler preserves within-row correlation
	marginal_ref = MarginalReferenceSampler$new(task)
	sampled_ref = marginal_ref$sample(c("x1", "x2"))

	# Within-row correlation should be preserved (approximately)
	cor_ref = cor(sampled_ref$x1, sampled_ref$x2)

	# PermutationSampler breaks all correlations
	permutation = PermutationSampler$new(task)
	set.seed(456)
	sampled_perm = permutation$sample(c("x1", "x2"))

	# Correlation should be much weaker (permutation breaks it)
	cor_perm = cor(sampled_perm$x1, sampled_perm$x2)

	# MarginalReferenceSampler should preserve correlation better than PermutationSampler
	# (though not perfectly since we're sampling with replacement)
	expect_gt(abs(cor_ref), abs(cor_perm) * 0.5) # At least half the correlation preserved
})

test_that("MarginalReferenceSampler works with different task types", {
	library(mlr3)
	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = MarginalReferenceSampler$new(task_regr)
	sampled_regr = sampler_regr$sample("x1")
	expect_sampler_output(sampled_regr, task_regr, nrows = 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = MarginalReferenceSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_sampler_output(sampled_classif, task_classif, nrows = task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = MarginalReferenceSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_sampler_output(sampled_multi, task_multi, nrows = 150)
})

test_that("MarginalReferenceSampler handles n_samples edge cases", {
	library(mlr3)
	task = tgen("circle", d = 5)$generate(n = 100)

	# n_samples larger than task size
	sampler = MarginalReferenceSampler$new(task, n_samples = 200L)
	expect_equal(nrow(sampler$reference_data), 100) # Capped at task size

	# n_samples = 1
	sampler_small = MarginalReferenceSampler$new(task, n_samples = 1L)
	expect_equal(nrow(sampler_small$reference_data), 1)
})
