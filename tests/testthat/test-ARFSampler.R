test_that("ARFSampler basic functionality", {
	skip_if_not_installed("arf")
	library(mlr3)
	n = 100
	task = tgen("circle", d = 5)$generate(n = n)
	sampler = ARFSampler$new(task)

	expect_true(inherits(sampler, "ARFSampler"))
	expect_equal(sampler$label, "Adversarial Random Forest sampler")
	expect_true(inherits(sampler$param_set, "ParamSet"))
	expect_true("conditioning_set" %in% sampler$param_set$ids())
	expect_true("finite_bounds" %in% sampler$param_set$ids())
	expect_true("round" %in% sampler$param_set$ids())
	expect_true("stepsize" %in% sampler$param_set$ids())
	expect_true("verbose" %in% sampler$param_set$ids())
	expect_true("parallel" %in% sampler$param_set$ids())

	# Check that ARF model was fitted
	expect_true(inherits(sampler$arf_model, "ranger"))
	expect_true(is.list(sampler$psi))

	# Test single feature sampling with default conditioning (all other features)
	data = task$data()
	sampled_data = sampler$sample("x1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), n)
	expect_equal(ncol(sampled_data), ncol(data))
	expect_equal(names(sampled_data), names(data))

	# Check that only the specified feature was changed
	expect_false(identical(sampled_data$x1, data$x1))
	expect_true(identical(sampled_data$x2, data$x2))
	expect_true(identical(sampled_data$y, data$y))
})

test_that("ARFSampler with conditioning_set parameter at initialization", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)

	# Initialize with specific conditioning features
	sampler = ARFSampler$new(task, conditioning_set = c("x2", "x3"))

	expect_equal(sampler$param_set$values$conditioning_set, c("x2", "x3"))

	data = task$data()
	# Should use the stored conditioning features
	sampled_data = sampler$sample("x1")

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), ncol(data))
	expect_false(identical(sampled_data$x1, data$x1))
	expect_true(identical(sampled_data$x2, data$x2))
	expect_true(identical(sampled_data$x3, data$x3))
})

test_that("ARFSampler with empty conditioning set behaves like marginal sampling", {
	skip_if_not_installed("arf")
	library(mlr3)

	set.seed(123)
	task = tgen("circle", d = 5)$generate(n = 100)

	# ARF sampler with empty conditioning set
	sampler_arf = ARFSampler$new(task, conditioning_set = character(0))

	# Marginal sampler for comparison
	sampler_marginal = MarginalSampler$new(task)

	data = task$data()

	original_x1 = data$x1

	# Sample with ARF (empty conditioning)
	sampled_arf = sampler_arf$sample("x1")

	# Sample with MarginalSampler for comparison
	sampled_marginal = sampler_marginal$sample("x1")

	# Basic checks for ARF sampler
	expect_false(identical(sampled_arf$x1, original_x1))
	expect_true(all(is.finite(sampled_arf$x1)))
	expect_equal(length(sampled_arf$x1), length(original_x1))

	# Basic checks for MarginalSampler
	expect_false(identical(sampled_marginal$x1, original_x1))
	expect_setequal(sampled_marginal$x1, original_x1) # Marginal uses permutation

	# Compare distributional properties
	# Both should have similar ranges (ARF might be slightly different due to model approximation)
	arf_range = range(sampled_arf$x1)
	marginal_range = range(sampled_marginal$x1)
	original_range = range(original_x1)

	# ARF range should be reasonable compared to original
	# Allow more extrapolation since ARF is generative and may go beyond training range
	expect_true(arf_range[1] >= original_range[1] - 1.5) # Allow more extrapolation
	expect_true(arf_range[2] <= original_range[2] + 1.5)

	# Marginal range should exactly match original (it's a permutation)
	expect_equal(marginal_range, original_range)

	# Both methods should leave other features unchanged
	expect_true(identical(sampled_arf$x2, data$x2)) # Other features unchanged
	expect_true(identical(sampled_marginal$x2, data$x2))

	# Verify empty conditioning is being used in ARF
	expect_equal(sampler_arf$param_set$values$conditioning_set, character(0))
})

test_that("ARFSampler conditioning features priority", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)
	data = task$data()

	# Initialize with default conditioning features
	sampler = ARFSampler$new(task, conditioning_set = c("x2"))

	# Test 1: Use stored conditioning features
	sampled1 = sampler$sample("x1")
	expect_false(identical(sampled1$x1, data$x1))

	# Test 2: Override with function argument
	sampled2 = sampler$sample("x1", conditioning_set = c("x3", "x4"))
	expect_false(identical(sampled2$x1, data$x1))

	# Test 3: Explicit NULL should use stored value
	sampled3 = sampler$sample("x1", conditioning_set = NULL)
	expect_false(identical(sampled3$x1, data$x1))

	# The stored value should remain unchanged
	expect_equal(sampler$param_set$values$conditioning_set, c("x2"))
})

test_that("ARFSampler handles multiple features", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)
	sampler = ARFSampler$new(task)
	data = task$data()

	# Test multiple feature sampling
	features = c("x1", "x2")
	sampled_data = sampler$sample(features)

	expect_true(data.table::is.data.table(sampled_data))
	expect_equal(nrow(sampled_data), 100)
	expect_equal(ncol(sampled_data), ncol(data))

	# Check that only the specified features were changed
	for (feat in features) {
		expect_false(identical(sampled_data[[feat]], data[[feat]]))
	}

	# Check that other features remain unchanged
	expect_true(identical(sampled_data$x3, data$x3))
	expect_true(identical(sampled_data$y, data$y))
})

test_that("ARFSampler works with different task types", {
	skip_if_not_installed("arf")
	library(mlr3)

	# Regression task
	task_regr = tgen("circle", d = 4)$generate(n = 100)
	sampler_regr = ARFSampler$new(task_regr)
	sampled_regr = sampler_regr$sample("x1")
	expect_true(data.table::is.data.table(sampled_regr))
	expect_equal(nrow(sampled_regr), 100)

	# Binary classification task
	task_classif = tsk("sonar")
	sampler_classif = ARFSampler$new(task_classif)
	sampled_classif = sampler_classif$sample("V1")
	expect_true(data.table::is.data.table(sampled_classif))
	expect_equal(nrow(sampled_classif), task_classif$nrow)

	# Multiclass classification task
	task_multi = tsk("iris")
	sampler_multi = ARFSampler$new(task_multi)
	sampled_multi = sampler_multi$sample("Sepal.Length")
	expect_true(data.table::is.data.table(sampled_multi))
	expect_equal(nrow(sampled_multi), 150)
})

test_that("ARFSampler parameter validation", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 100)

	# Test with invalid conditioning features should still create the sampler
	# (validation happens at sample time)
	sampler = ARFSampler$new(task, conditioning_set = c("nonexistent_feature"))
	expect_true(inherits(sampler, "ARFSampler"))
	expect_equal(sampler$param_set$values$conditioning_set, c("nonexistent_feature"))
})

test_that("ARFSampler finite_bounds parameter", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)

	# Test initialization with no bounds (default)
	sampler = ARFSampler$new(task, finite_bounds = "no")
	expect_equal(sampler$param_set$values$finite_bounds, "no")

	# Test "local" bounds initialization
	sampler_local = ARFSampler$new(task, finite_bounds = "local")
	expect_equal(sampler_local$param_set$values$finite_bounds, "local")

	# Test no bounds (default)
	sampled_no = sampler$sample("x1")
	expect_equal(nrow(sampled_no), 50)

	# Test local bounds
	sampled_local = sampler_local$sample("x1")
	expect_equal(nrow(sampled_local), 50)
})


test_that("ARFSampler parameter priority and storage", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)
	data = task$data()

	# Initialize with stored parameters
	sampler = ARFSampler$new(
		task,
		conditioning_set = "x2",
		verbose = FALSE,
		parallel = FALSE
	)

	# Test that stored parameters are used when not specified in sample()
	sampled1 = sampler$sample("x1")
	expect_equal(nrow(sampled1), 50)

	# Test that function arguments override stored parameters
	sampled2 = sampler$sample("x1", verbose = FALSE)
	expect_equal(nrow(sampled2), 50)

	# Test overriding verbose parameter
	sampled3 = sampler$sample("x1", verbose = TRUE)
	expect_equal(nrow(sampled3), 50)

	# Stored parameters should remain unchanged
	expect_equal(sampler$param_set$values$verbose, FALSE)
	expect_equal(sampler$param_set$values$parallel, FALSE)
})

test_that("ARFSampler param_set structure", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)
	sampler = ARFSampler$new(task, finite_bounds = "no")

	# Check param_set has the correct parameters
	expect_true("conditioning_set" %in% sampler$param_set$ids())
	expect_true("finite_bounds" %in% sampler$param_set$ids())
	expect_true("round" %in% sampler$param_set$ids())
	expect_true("stepsize" %in% sampler$param_set$ids())
	expect_true("verbose" %in% sampler$param_set$ids())
	expect_true("parallel" %in% sampler$param_set$ids())

	# Check that removed parameters are not in param_set
	expect_false("evidence_row_mode" %in% sampler$param_set$ids()) # evidence_row_mode is hardcoded
	expect_false("sample_NAs" %in% sampler$param_set$ids()) # sample_NAs is hardcoded
	expect_false("nomatch" %in% sampler$param_set$ids()) # nomatch is hardcoded
	expect_false("n_synth" %in% sampler$param_set$ids()) # n_synth is hardcoded

	# Check parameter types
	expect_equal(sampler$param_set$params[id == "finite_bounds"]$cls, "ParamFct")

	# Check default values
	expect_equal(sampler$param_set$params[id == "finite_bounds"]$default[[1]], "no")
	expect_equal(sampler$param_set$params[id == "round"]$default[[1]], TRUE)
	expect_equal(sampler$param_set$params[id == "stepsize"]$default[[1]], 0)
	expect_equal(sampler$param_set$params[id == "verbose"]$default[[1]], FALSE)
	expect_equal(sampler$param_set$params[id == "parallel"]$default[[1]], FALSE)

	# Check stored values
	expect_equal(sampler$param_set$values$finite_bounds, "no")
})

test_that("ARFSampler additional arf::forge parameters", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle", d = 5)$generate(n = 50)

	# Test initialization with additional parameters
	sampler = ARFSampler$new(
		task,
		round = FALSE,
		finite_bounds = "no",
		verbose = FALSE,
		parallel = FALSE
	)

	# Check parameter storage
	expect_equal(sampler$param_set$values$round, FALSE)
	expect_equal(sampler$param_set$values$stepsize, 0)
	expect_equal(sampler$param_set$values$verbose, FALSE)
	expect_equal(sampler$param_set$values$parallel, FALSE)
	expect_equal(sampler$param_set$values$finite_bounds, "no")

	# Check param_set structure for parameters
	expect_true("finite_bounds" %in% sampler$param_set$ids())
	expect_true("round" %in% sampler$param_set$ids())
	expect_true("stepsize" %in% sampler$param_set$ids())
	expect_true("verbose" %in% sampler$param_set$ids())
	expect_true("parallel" %in% sampler$param_set$ids())

	data = task$data()

	# Test sampling with stored parameters (should work without warnings)
	sampled1 = sampler$sample("x1")
	expect_equal(nrow(sampled1), 50)

	# Test overriding parameters
	sampled2 = sampler$sample("x1", round = TRUE, verbose = TRUE)
	expect_equal(nrow(sampled2), 50)

	# Stored parameters should remain unchanged
	expect_equal(sampler$param_set$values$round, FALSE)
	expect_equal(sampler$param_set$values$verbose, FALSE)
})

test_that("ARFSampler default parameter values match arf::forge", {
	skip_if_not_installed("arf")
	library(mlr3)

	task = tgen("circle")$generate(n = 50)
	sampler = ARFSampler$new(task)

	# Check that defaults are reasonable for xplainfi usage
	expect_equal(sampler$param_set$params[id == "finite_bounds"]$default[[1]], "no")
	expect_equal(sampler$param_set$params[id == "round"]$default[[1]], TRUE)
	expect_equal(sampler$param_set$params[id == "stepsize"]$default[[1]], 0)
	expect_equal(sampler$param_set$params[id == "verbose"]$default[[1]], FALSE)
	expect_equal(sampler$param_set$params[id == "parallel"]$default[[1]], FALSE)
})
