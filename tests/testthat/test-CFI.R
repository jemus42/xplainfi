test_that("CFI can't be constructed without args", {
	expect_error(CFI$new())
})

test_that("CFI can be constructed with simple objects", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)

	checkmate::expect_r6(cfi, c("FeatureImportanceMethod", "PerturbationImportance", "CFI"))

	cfi$compute()
	expect_importance_dt(cfi$importance(), features = cfi$features)
	# Test that default is "difference"
	expect_identical(cfi$importance(), cfi$importance(relation = "difference"))
})

test_that("CFI uses ARFSampler by default", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 100)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)

	# Should have ARFSampler
	checkmate::expect_r6(cfi$sampler, "ARFSampler")
	expect_equal(cfi$label, "Conditional Feature Importance")
})

test_that("CFI with custom ARF sampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("spirals")$generate(n = 100)
	custom_sampler = ARFSampler$new(task)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		sampler = custom_sampler
	)

	# Should use the custom sampler
	checkmate::expect_r6(cfi$sampler, "ARFSampler")
	cfi$compute()
	expect_importance_dt(cfi$importance(), features = cfi$features)
})

test_that("CFI null result for featureless learner", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 200)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.featureless"),
		measure = mlr3::msr("classif.ce")
		# Uses ARFSampler by default
	)

	cfi$compute()

	expected = data.table::data.table(
		feature = cfi$features,
		importance = 0,
		key = "feature"
	)

	expect_identical(cfi$importance(), expected)
})

test_that("CFI multiple perms", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2
	)

	cfi$compute()

	expect_importance_dt(cfi$importance(), features = cfi$features)

	checkmate::expect_data_table(
		cfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = cfi$resampling$iters *
			cfi$param_set$values$iters_perm *
			length(cfi$features),
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})

test_that("CFI only one feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2,
		features = "important4"
	)

	cfi$compute()

	expect_importance_dt(cfi$importance(), features = "important4")

	checkmate::expect_data_table(
		cfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = cfi$resampling$iters *
			cfi$param_set$values$iters_perm,
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})

test_that("CFI with friedman1 produces sensible results", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		iters_perm = 2
	)

	cfi$compute()
	result = cfi$importance()
	expect_importance_dt(result, features = cfi$features)

	# Check that important features (important1-5) generally have higher scores
	# than unimportant features (unimportant1-5)
	important_features = grep("^important", result$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result$feature, value = TRUE)

	important_scores = result[feature %in% important_features]$importance
	unimportant_scores = result[feature %in% unimportant_features]$importance

	# On average, important features should have higher CFI values
	expect_gt(mean(important_scores), mean(unimportant_scores))

	# Check that scores are finite and not all zero
	expect_true(all(is.finite(result$importance)))
	expect_gt(max(abs(result$importance)), 0)
})

test_that("CFI with resampling", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor", d = 5)$generate(n = 200)
	learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
	resampling = mlr3::rsmp("cv", folds = 3)
	measure = mlr3::msr("classif.ce")

	cfi = CFI$new(
		task = task,
		learner = learner,
		resampling = resampling,
		measure = measure,
		iters_perm = 2
	)

	cfi$compute()
	res_1 = cfi$importance()
	expect_importance_dt(res_1, cfi$features)
})

test_that("CFI parameter validation", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)
	learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	# iters_perm must be positive integer
	expect_error(CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		iters_perm = 0L
	))

	expect_error(CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		iters_perm = -1L
	))
})
