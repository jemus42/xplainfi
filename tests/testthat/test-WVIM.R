test_that("WVIM can't be constructed without args", {
	expect_error(WVIM$new())
})

test_that("LOCO can't be constructed without args", {
	expect_error(LOCO$new())
})

test_that("LOCO works with regression task", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	loco = LOCO$new(
		task = task,
		learner = learner,
		measure = measure
	)

	checkmate::expect_r6(loco, c("FeatureImportanceMethod", "WVIM", "LOCO"))
	expect_equal(loco$direction, "leave-out")
	expect_equal(loco$label, "Leave-One-Covariate-Out (LOCO)")

	loco$compute()
	expect_importance_dt(loco$importance(), features = loco$features)

	# Scores should be a method
	scores = loco$scores()
	checkmate::expect_data_table(scores, min.rows = length(loco$features))
	expect_true(all(c("feature", "iter_rsmp", "iter_refit") %in% names(scores)))
})

test_that("LOCO works with classification task", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("simplex", d = 5)$generate(n = 200)
	learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	loco = LOCO$new(
		task = task,
		learner = learner,
		measure = measure
	)

	loco$compute()
	expect_importance_dt(loco$importance(), features = loco$features)
})

test_that("LOCO works with features = NULL (all features)", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 100)
	learner = mlr3::lrn("regr.ranger", num.trees = 20)
	measure = mlr3::msr("regr.mse")

	loco = LOCO$new(task, learner, measure)
	expect_equal(loco$features, task$feature_names)

	loco$compute()
	expect_importance_dt(loco$importance(), features = task$feature_names)
})

test_that("LOCO works with subset of features", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 100)
	learner = mlr3::lrn("regr.ranger", num.trees = 20)
	measure = mlr3::msr("regr.mse")

	features_subset = task$feature_names[1:3]
	loco = LOCO$new(task, learner, measure, features = features_subset)
	expect_equal(loco$features, features_subset)

	loco$compute()
	expect_importance_dt(loco$importance(), features = features_subset)
})

test_that("LOCO works with multiple refits", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)
	learner = mlr3::lrn("regr.ranger", num.trees = 20)
	measure = mlr3::msr("regr.mse")

	loco = LOCO$new(
		task = task,
		learner = learner,
		measure = measure,
		features = task$feature_names[1:3],
		iters_refit = 3L
	)

	loco$compute()
	expect_importance_dt(loco$importance(), features = loco$features)

	# Scores should have multiple refits
	scores = loco$scores()
	# With holdout resampling, we get 1 iteration * 3 refits * 3 features = 9 rows
	expect_gte(nrow(scores), length(loco$features))
	expect_true(all(scores$iter_refit %in% 1:3))
})

test_that("LOCO works with cross-validation", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)
	learner = mlr3::lrn("regr.ranger", num.trees = 20)
	measure = mlr3::msr("regr.mse")
	resampling = mlr3::rsmp("cv", folds = 3)

	loco = LOCO$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		features = task$feature_names[1:2]
	)

	loco$compute()
	importance = loco$importance()
	expect_importance_dt(importance, features = loco$features)

	# With multiple resampling folds, scores should have multiple iter_rsmp values
	scores = loco$scores()
	expect_gt(length(unique(scores$iter_rsmp)), 1L)
})
