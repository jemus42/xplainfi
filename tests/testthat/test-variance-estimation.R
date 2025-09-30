test_that("importance() accepts all variance_method values", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 5),
		iters_perm = 2
	)

	pfi$compute()

	# Test that all variance methods work
	imp_none = pfi$importance(variance_method = "none")
	imp_raw = pfi$importance(variance_method = "raw")
	imp_nb = pfi$importance(variance_method = "nadeau_bengio")

	expect_importance_dt(imp_none, features = pfi$features)
	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)
})

test_that("variance_method='none' produces no variance columns", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 5),
		iters_perm = 2
	)

	pfi$compute()
	imp_none = pfi$importance(variance_method = "none")

	# Check that only feature and importance columns exist
	expect_equal(names(imp_none), c("feature", "importance"))
})

test_that("raw CIs are narrower than nadeau_bengio corrected CIs", {
	set.seed(123)
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 10, ratio = 0.8),
		iters_perm = 5
	)

	pfi$compute()

	imp_raw = pfi$importance(variance_method = "raw")
	imp_nb = pfi$importance(variance_method = "nadeau_bengio")

	# Calculate CI widths
	width_raw = imp_raw$conf_upper - imp_raw$conf_lower
	width_nb = imp_nb$conf_upper - imp_nb$conf_lower

	# Raw CIs should be narrower than corrected ones on average
	# Compare the mean widths instead of individual features
	# The nadeau_bengio correction factor should make CIs wider on average
	expect_true(mean(width_nb) > mean(width_raw))
})

test_that("nadeau_bengio correction requires appropriate resampling", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	# Cross-validation is not supported for nadeau_bengio
	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		iters_perm = 2
	)

	pfi$compute()

	# Should error for unsupported resampling
	expect_error(
		pfi$importance(variance_method = "nadeau_bengio"),
		regexp = "Must be a subset of"
	)

	# But raw variance should still work
	imp_raw = pfi$importance(variance_method = "raw")
	expect_importance_dt(imp_raw, features = pfi$features)
})

test_that("confidence level parameter works correctly", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 5),
		iters_perm = 2
	)

	pfi$compute()

	# Test different confidence levels
	imp_90 = pfi$importance(variance_method = "raw", conf_level = 0.90)
	imp_95 = pfi$importance(variance_method = "raw", conf_level = 0.95)
	imp_99 = pfi$importance(variance_method = "raw", conf_level = 0.99)

	# Calculate CI widths
	width_90 = imp_90$conf_upper - imp_90$conf_lower
	width_95 = imp_95$conf_upper - imp_95$conf_lower
	width_99 = imp_99$conf_upper - imp_99$conf_lower

	# Higher confidence level should produce wider CIs (on average)
	expect_true(mean(width_90) < mean(width_95))
	expect_true(mean(width_95) < mean(width_99))
})

test_that("variance estimation works with bootstrap resampling", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("bootstrap", repeats = 5),
		iters_perm = 2
	)

	pfi$compute()

	# Both raw and nadeau_bengio should work with bootstrap
	imp_raw = pfi$importance(variance_method = "raw")
	imp_nb = pfi$importance(variance_method = "nadeau_bengio")

	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)

	# Verify variance columns exist
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_raw)))
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_nb)))
})
