test_that("importance() accepts all ci_method values", {
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
	imp_none = pfi$importance(ci_method = "none")
	imp_raw = pfi$importance(ci_method = "raw")
	imp_nb = pfi$importance(ci_method = "nadeau_bengio")
	imp_quantile = pfi$importance(ci_method = "quantile")

	expect_importance_dt(imp_none, features = pfi$features)
	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)
	expect_importance_dt(imp_quantile, features = pfi$features)
})

test_that("ci_method='none' produces no variance columns", {
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
	imp_none = pfi$importance(ci_method = "none")

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

	imp_raw = pfi$importance(ci_method = "raw")
	imp_nb = pfi$importance(ci_method = "nadeau_bengio")

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
	expect_warning(
		pfi$importance(ci_method = "nadeau_bengio"),
		regexp = "recommended for resampling types"
	)

	# But raw variance should still work
	imp_raw = pfi$importance(ci_method = "raw")
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
	imp_90 = pfi$importance(ci_method = "raw", conf_level = 0.90)
	imp_95 = pfi$importance(ci_method = "raw", conf_level = 0.95)
	imp_99 = pfi$importance(ci_method = "raw", conf_level = 0.99)

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
	imp_raw = pfi$importance(ci_method = "raw")
	imp_nb = pfi$importance(ci_method = "nadeau_bengio")

	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)

	# Verify variance columns exist
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_raw)))
	expect_true(all(c("se", "conf_lower", "conf_upper") %in% names(imp_nb)))
})

test_that("quantile variance method works", {
	set.seed(123)
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 10),
		iters_perm = 2
	)

	pfi$compute()

	imp_quantile = pfi$importance(ci_method = "quantile")

	# Check structure
	expect_importance_dt(imp_quantile, features = pfi$features)

	# Verify CI columns exist (no se for quantile method)
	expected_cols = c("feature", "importance", "conf_lower", "conf_upper")
	expect_true(all(expected_cols %in% names(imp_quantile)))
	expect_false("se" %in% names(imp_quantile))

	# All CIs should be valid intervals
	expect_true(all(imp_quantile$conf_lower <= imp_quantile$conf_upper))

	# Point estimates should be between lower and upper bounds (or close)
	# Due to using mean vs quantiles, this is not guaranteed but usually holds
	expect_true(all(
		imp_quantile$importance >= imp_quantile$conf_lower |
			abs(imp_quantile$importance - imp_quantile$conf_lower) < 0.01
	))
	expect_true(all(
		imp_quantile$importance <= imp_quantile$conf_upper |
			abs(imp_quantile$importance - imp_quantile$conf_upper) < 0.01
	))
})

test_that("quantile CIs differ from parametric methods", {
	set.seed(123)
	task = sim_dgp_independent(n = 200)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 15),
		iters_perm = 3
	)

	pfi$compute()

	imp_raw = pfi$importance(ci_method = "raw")
	imp_quantile = pfi$importance(ci_method = "quantile")

	# Point estimates should be the same (both use mean)
	expect_equal(imp_raw$importance, imp_quantile$importance)

	# CIs should generally differ between methods
	# (quantile is non-parametric, raw assumes normality)
	expect_false(all(imp_raw$conf_lower == imp_quantile$conf_lower))
	expect_false(all(imp_raw$conf_upper == imp_quantile$conf_upper))
})
