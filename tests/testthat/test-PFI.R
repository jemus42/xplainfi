test_that("can't be constructed without args", {
	expect_error(PFI$new())
})

test_that("can be constructed with simple objects", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)

	checkmate::expect_r6(pfi, c("FeatureImportanceMethod", "PFI"))

	pfi$compute()
	expect_importance_dt(pfi$importance(), features = pfi$features)
	pfi$compute(relation = "difference")
	expect_importance_dt(pfi$importance(), features = pfi$features)
})

test_that("null result for featureless learner", {
	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("classif.featureless"),
		measure = mlr3::msr("classif.ce")
	)

	pfi$compute()

	expected = data.table::data.table(
		feature = pfi$features,
		importance = 0,
		key = "feature"
	)

	expect_identical(pfi$importance(), expected)
})

test_that("multiple perms", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2
	)

	pfi$compute()

	expect_importance_dt(pfi$importance(), features = pfi$features)

	checkmate::expect_data_table(
		pfi$scores,
		types = c("character", "integer", "numeric"),
		nrows = pfi$resampling$iters *
			pfi$param_set$values$iters_perm *
			length(pfi$features),
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})

test_that("only one feature", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.ranger", num.trees = 50),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2,
		features = "important4"
	)

	pfi$compute()

	expect_importance_dt(pfi$importance(), features = "important4")

	checkmate::expect_data_table(
		pfi$scores,
		types = c("character", "integer", "numeric"),
		nrows = pfi$resampling$iters *
			pfi$param_set$values$iters_perm,
		ncols = 6,
		any.missing = FALSE,
		min.cols = 6
	)
})


test_that("PFI different relations (difference vs ratio)", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce")
	)

	# Default behavior should be sane
	pfi$compute()
	res_1 = pfi$importance()
	expect_importance_dt(res_1, pfi$features)

	pfi$compute()
	res_2 = pfi$importance()
	expect_identical(res_1, res_2)

	pfi$compute("difference")
	res_3 = pfi$importance()
	expect_identical(res_1, res_3)

	pfi$compute("ratio")
	res_4 = pfi$importance()
	pfi$compute("difference")
	res_5 = pfi$importance()

	expect_error(expect_equal(res_4, res_5))

	expect_importance_dt(res_2, pfi$features)
	expect_importance_dt(res_3, pfi$features)
	expect_importance_dt(res_4, pfi$features)
	expect_importance_dt(res_5, pfi$features)
})

test_that("PFI with resampling", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("xor")$generate(n = 200)
	learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
	resampling = mlr3::rsmp("cv", folds = 3)
	measure = mlr3::msr("classif.ce")

	pfi = PFI$new(
		task = task,
		learner = learner,
		resampling = resampling,
		measure = measure,
		iters_perm = 2
	)

	pfi$compute()
	res_1 = pfi$importance()
	expect_importance_dt(res_1, pfi$features)

	pfi$compute("ratio")
	res_2 = pfi$importance()
	expect_importance_dt(res_2, pfi$features)

	expect_error(expect_equal(res_1, res_2))
})
