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
	# Test that default is "difference"
	expect_identical(pfi$importance(), pfi$importance(relation = "difference"))
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
		pfi$scores(),
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
		pfi$scores(),
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

	# Compute once, test different relations
	pfi$compute()
	res_diff = pfi$importance(relation = "difference")
	res_ratio = pfi$importance(relation = "ratio")
	res_default = pfi$importance()

	expect_importance_dt(res_diff, pfi$features)
	expect_importance_dt(res_ratio, pfi$features)

	# Default should be "difference"
	expect_identical(res_default, res_diff)

	# Different relations should give different results
	expect_false(isTRUE(all.equal(res_diff, res_ratio)))
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
	res_diff = pfi$importance(relation = "difference")
	res_ratio = pfi$importance(relation = "ratio")

	expect_importance_dt(res_diff, pfi$features)
	expect_importance_dt(res_ratio, pfi$features)

	# Different relations should give different results
	expect_false(isTRUE(all.equal(res_diff, res_ratio)))
})

test_that("scores and obs_losses agree", {
	skip_if_not_installed("mlr3learners")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2
	)

	pfi$compute()

	importance_agg = pfi$importance()
	importance_scores = pfi$scores()[, .(iter_rsmp, iter_perm, feature, importance)][
		order(iter_rsmp, iter_perm, feature)
	]
	importance_obs_loss = pfi$obs_loss()

	expect_equal(
		importance_agg,
		importance_scores[, list(importance = mean(importance)), by = "feature"],
		ignore_attr = TRUE # ignore sorting by feature
	)

	# Aggregate squared errors to get mse per iteration, should be same as $scores()
	# up to numerical error
	obs_agg = importance_obs_loss[,
		list(importance = mean(obs_importance)),
		by = c("iter_rsmp", "iter_perm", "feature")
	][order(iter_rsmp, iter_perm, feature)]

	expect_equal(importance_scores, obs_agg, tolerance = sqrt(.Machine$double.eps))
})
