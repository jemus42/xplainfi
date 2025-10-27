test_that("RFI can't be constructed without args", {
	expect_error(RFI$new())
})

test_that("RFI can be constructed with simple objects", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = "x2"
	)

	checkmate::expect_r6(rfi, c("FeatureImportanceMethod", "PerturbationImportance", "RFI"))

	rfi$compute()
	expect_importance_dt(rfi$importance(), features = rfi$features)
	# Test that default is "difference"
	expect_identical(rfi$importance(), rfi$importance(relation = "difference"))
})

test_that("RFI uses ConditionalARFSampler by default", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = "x2"
	)

	# Should have ConditionalARFSampler
	checkmate::expect_r6(rfi$sampler, "ConditionalARFSampler")
	expect_equal(rfi$label, "Relative Feature Importance")
})

test_that("RFI with custom conditioning set", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	# Use only the first two important features as conditioning set
	conditioning_set = c("important1", "important2")

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		conditioning_set = conditioning_set
	)

	expect_identical(rfi$param_set$values$conditioning_set, conditioning_set)

	rfi$compute()
	result = rfi$importance()
	expect_importance_dt(result, features = rfi$features)
})

test_that("RFI with empty conditioning set (equivalent to PFI)", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200) # Use friedman1 with more features for better ranking comparison
	learner = mlr3::lrn("regr.rpart")
	measure = mlr3::msr("regr.mse")

	# RFI with empty conditioning set warns
	expect_warning({
		rfi = RFI$new(
			task = task,
			learner = learner,
			measure = measure
		)
	})

	expect_equal(length(rfi$param_set$values$conditioning_set), 0)

	rfi$compute()
	rfi_result = rfi$importance()

	expect_importance_dt(rfi_result, features = rfi$features)
})

test_that("RFI with single conditioning feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = "x1" # Single conditioning feature
	)

	expect_equal(length(rfi$param_set$values$conditioning_set), 1)
	expect_equal(rfi$param_set$values$conditioning_set, "x1")

	rfi$compute()
	result = rfi$importance()
	expect_importance_dt(result, features = rfi$features)
})

test_that("RFI with custom ARF sampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("spirals")$generate(n = 100)
	custom_sampler = ConditionalARFSampler$new(task)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = c("x1"), # Use one feature as conditioning set
		sampler = custom_sampler
	)

	# Should use the custom sampler
	checkmate::expect_r6(rfi$sampler, "ConditionalARFSampler")
	rfi$compute()
	expect_importance_dt(rfi$importance(), features = rfi$features)
})

test_that("RFI null result for featureless learner", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 200)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.featureless"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = "x1"
	)

	rfi$compute()

	expected = data.table::data.table(
		feature = rfi$features,
		importance = 0,
		key = "feature"
	)

	expect_identical(rfi$importance(), expected)
})

test_that("RFI multiple perms", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		conditioning_set = "important1",
		n_repeats = 2
	)

	rfi$compute()

	expect_importance_dt(rfi$importance(), features = rfi$features)

	checkmate::expect_data_table(
		rfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = rfi$resampling$iters *
			rfi$param_set$values$n_repeats *
			length(rfi$features),
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})

test_that("RFI only one feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		conditioning_set = c("important1", "important2"),
		n_repeats = 2,
		features = "important4"
	)

	rfi$compute()

	expect_importance_dt(rfi$importance(), features = "important4")

	checkmate::expect_data_table(
		rfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = rfi$resampling$iters *
			rfi$param_set$values$n_repeats,
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})

test_that("RFI with friedman1 produces sensible results", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	rfi = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = c("important1"), # Condition on one feature
		n_repeats = 2
	)

	rfi$compute()
	result = rfi$importance()
	expect_importance_dt(result, features = rfi$features)

	# Check that important features (important1-5) generally have higher scores
	# than unimportant features (unimportant1-5)
	important_features = grep("^important", result$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result$feature, value = TRUE)

	important_scores = result[feature %in% important_features]$importance
	unimportant_scores = result[feature %in% unimportant_features]$importance

	# On average, important features should have higher RFI values
	expect_gt(mean(important_scores), mean(unimportant_scores))

	# Check that scores are finite and not all zero
	expect_true(all(is.finite(result$importance)))
	expect_gt(max(abs(result$importance)), 0)
})

test_that("RFI different relations (difference vs ratio)", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		conditioning_set = character(0) # Empty conditioning set (as oppsosed to NULL -> condition on all features)
	)

	# Default behavior should be sane
	rfi$compute()
	res_1 = rfi$importance()
	expect_importance_dt(res_1, rfi$features)

	res_2 = rfi$importance(relation = "difference")
	expect_identical(res_1, res_2)

	res_3 = rfi$importance(relation = "ratio")
	res_4 = rfi$importance(relation = "difference")

	expect_error(expect_equal(res_3, res_4))

	expect_importance_dt(res_2, rfi$features)
	expect_importance_dt(res_3, rfi$features)
	expect_importance_dt(res_4, rfi$features)
})

test_that("RFI parameter validation", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)
	learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	# n_repeats must be positive integer
	expect_error(RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		n_repeats = 0L
	))

	expect_error(RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		n_repeats = -1L
	))

	# conditioning_set must be valid feature names
	expect_error(RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = c("nonexistent_feature")
	))
})

test_that("RFI different conditioning sets produce different results", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	# RFI with empty conditioning set (equivalent to PFI)
	rfi_empty = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = character(0),
		n_repeats = 2
	)

	# RFI with one conditioning feature
	rfi_one = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = "important1",
		n_repeats = 2
	)

	# RFI with multiple conditioning features
	rfi_multi = RFI$new(
		task = task,
		learner = learner,
		measure = measure,
		conditioning_set = c("important1", "important2"),
		n_repeats = 2
	)

	rfi_empty$compute()
	result_empty = rfi_empty$importance()
	rfi_one$compute()
	result_one = rfi_one$importance()
	rfi_multi$compute()
	result_multi = rfi_multi$importance()

	# All should be valid importance tables
	expect_importance_dt(result_empty, features = rfi_empty$features)
	expect_importance_dt(result_one, features = rfi_one$features)
	expect_importance_dt(result_multi, features = rfi_multi$features)

	# Results should generally be different (allowing for some tolerance due to randomness)
	# We don't expect exact differences but the conditioning should have some effect
	expect_false(all(abs(result_empty$importance - result_one$importance) < 1e-10))
	expect_false(all(abs(result_one$importance - result_multi$importance) < 1e-10))
})

test_that("RFI with groups", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	# Define feature groups
	groups = list(
		early_important = c("important1", "important2"),
		late_important = c("important3", "important4"),
		noise = c("unimportant1", "unimportant2")
	)

	rfi = RFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		conditioning_set = "important5",
		groups = groups
	)

	checkmate::expect_r6(rfi, c("FeatureImportanceMethod", "PerturbationImportance", "RFI"))
	expect_false(is.null(rfi$groups))
	expect_equal(names(rfi$groups), c("early_important", "late_important", "noise"))

	rfi$compute()
	result = rfi$importance()

	# Should have one row per group
	expect_equal(nrow(result), length(groups))
	expect_equal(result$feature, names(groups))
	expect_importance_dt(result, features = names(groups))
})
