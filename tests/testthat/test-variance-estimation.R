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

	expect_importance_dt(imp_none, features = pfi$features)
	expect_importance_dt(imp_raw, features = pfi$features)
	expect_importance_dt(imp_nb, features = pfi$features)
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
	expect_error(
		pfi$importance(ci_method = "nadeau_bengio"),
		regexp = "Must be a subset of"
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

test_that("wilcoxon variance method works", {
	set.seed(123)
	task = sim_dgp_independent(n = 100)

	pfi = PFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("subsampling", repeats = 10),
		iters_perm = 2
	)

	pfi$compute()

	imp_wilcox = pfi$importance(ci_method = "wilcoxon")

	# Check structure (NA values are allowed for features with zero/constant importance)
	checkmate::expect_data_table(imp_wilcox, nrows = length(pfi$features))
	expect_setequal(imp_wilcox$feature, pfi$features)

	# Verify CI columns exist (no se for wilcoxon)
	expect_true(all(c("feature", "importance", "conf_lower", "conf_upper") %in% names(imp_wilcox)))
	expect_false("se" %in% names(imp_wilcox))

	# For features with non-NA CIs, they should be valid intervals
	valid_cis = imp_wilcox[!is.na(conf_lower) & !is.na(conf_upper)]
	if (nrow(valid_cis) > 0) {
		expect_true(all(valid_cis$conf_lower < valid_cis$conf_upper))
	}
})

test_that("wilcoxon CIs differ from parametric methods", {
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
	imp_wilcox = pfi$importance(ci_method = "wilcoxon")

	# Point estimates should be the same (both use mean)
	expect_equal(imp_raw$importance, imp_wilcox$importance)

	# For features with valid wilcoxon CIs, they should differ from parametric CIs
	# (some may be NA due to zero/tied scores)
	valid_wilcox = imp_wilcox[!is.na(conf_lower) & !is.na(conf_upper)]
	if (nrow(valid_wilcox) > 0) {
		# At least some features should have different CIs
		merged = imp_raw[valid_wilcox, on = "feature"]
		expect_false(all(merged$conf_lower == merged$i.conf_lower, na.rm = TRUE))
	}
})
