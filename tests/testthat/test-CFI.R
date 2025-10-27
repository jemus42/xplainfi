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

test_that("CFI uses ConditionalARFSampler by default", {
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

	# Should have ConditionalARFSampler
	checkmate::expect_r6(cfi$sampler, "ConditionalARFSampler")
	expect_equal(cfi$label, "Conditional Feature Importance")
})

test_that("CFI with custom ARF sampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("spirals")$generate(n = 100)
	custom_sampler = ConditionalARFSampler$new(task)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		sampler = custom_sampler
	)

	# Should use the custom sampler
	checkmate::expect_r6(cfi$sampler, "ConditionalARFSampler")
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
		# Uses ConditionalARFSampler by default
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
		n_repeats = 2
	)

	cfi$compute()

	expect_importance_dt(cfi$importance(), features = cfi$features)

	checkmate::expect_data_table(
		cfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = cfi$resampling$iters *
			cfi$param_set$values$n_repeats *
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
		n_repeats = 2,
		features = "important4"
	)

	cfi$compute()

	expect_importance_dt(cfi$importance(), features = "important4")

	checkmate::expect_data_table(
		cfi$scores(),
		types = c("character", "integer", "numeric"),
		nrows = cfi$resampling$iters *
			cfi$param_set$values$n_repeats,
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
		n_repeats = 2
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
		n_repeats = 2
	)

	cfi$compute()
	res_1 = cfi$importance()
	expect_importance_dt(res_1, cfi$features)
})

test_that("CFI parameter validation", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)
	learner = mlr3::lrn("classif.rpart", predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	# n_repeats must be positive integer
	expect_error(CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		n_repeats = 0L
	))

	expect_error(CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		n_repeats = -1L
	))
})

test_that("CFI with groups", {
	skip_if_not_installed("arf")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)

	# Define feature groups
	groups = list(
		important_features = c("important1", "important2", "important3"),
		unimportant_features = c("unimportant1", "unimportant2", "unimportant3")
	)

	cfi = CFI$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		groups = groups
	)

	checkmate::expect_r6(cfi, c("FeatureImportanceMethod", "PerturbationImportance", "CFI"))
	expect_false(is.null(cfi$groups))
	expect_equal(names(cfi$groups), c("important_features", "unimportant_features"))

	cfi$compute()
	result = cfi$importance()

	# Should have one row per group
	expect_equal(nrow(result), length(groups))
	expect_equal(result$feature, names(groups))
	expect_importance_dt(result, features = names(groups))
})

test_that("CFI with KnockoffSampler and KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	# Test with KnockoffSampler
	knockoff_sampler = KnockoffSampler$new(task)
	cfi_knockoff = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		sampler = knockoff_sampler
	)

	checkmate::expect_r6(cfi_knockoff$sampler, "KnockoffSampler")
	cfi_knockoff$compute()
	result_knockoff = cfi_knockoff$importance()
	expect_importance_dt(result_knockoff, features = cfi_knockoff$features)

	# Test with KnockoffGaussianSampler (convenience wrapper)
	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi_gaussian = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		sampler = gaussian_sampler
	)

	checkmate::expect_r6(cfi_gaussian$sampler, "KnockoffGaussianSampler")
	cfi_gaussian$compute()
	result_gaussian = cfi_gaussian$importance()
	expect_importance_dt(result_gaussian, features = cfi_gaussian$features)

	# Both should produce valid importance scores
	expect_true(all(is.finite(result_knockoff$importance)))
	expect_true(all(is.finite(result_gaussian$importance)))

	# Check that important features generally have higher scores
	important_features = grep("^important", result_knockoff$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result_knockoff$feature, value = TRUE)

	important_scores_ko = result_knockoff[feature %in% important_features]$importance
	unimportant_scores_ko = result_knockoff[feature %in% unimportant_features]$importance

	important_scores_gauss = result_gaussian[feature %in% important_features]$importance
	unimportant_scores_gauss = result_gaussian[feature %in% unimportant_features]$importance

	# On average, important features should have higher CFI values
	expect_gt(mean(important_scores_ko), mean(unimportant_scores_ko))
	expect_gt(mean(important_scores_gauss), mean(unimportant_scores_gauss))
})

test_that("CFI with KnockoffSequentialSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("seqknockoff")

	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 150)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	# Test with KnockoffSequentialSampler
	seq_sampler = KnockoffSequentialSampler$new(task)
	cfi_seq = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		sampler = seq_sampler
	)

	checkmate::expect_r6(cfi_seq$sampler, "KnockoffSequentialSampler")
	cfi_seq$compute()
	result_seq = cfi_seq$importance()
	expect_importance_dt(result_seq, features = cfi_seq$features)

	# Should produce valid importance scores
	expect_true(all(is.finite(result_seq$importance)))

	# Check that important features generally have higher scores
	important_features = grep("^important", result_seq$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result_seq$feature, value = TRUE)

	important_scores = result_seq[feature %in% important_features]$importance
	unimportant_scores = result_seq[feature %in% unimportant_features]$importance

	# On average, important features should have higher CFI values
	expect_gt(mean(important_scores), mean(unimportant_scores))
})

test_that("CFI with CPI variance method using KnockoffGaussianSampler", {
	skip_if_not_installed("ranger")
	skip_if_not_installed("mlr3learners")
	skip_if_not_installed("knockoff")

	set.seed(123)
	# Use correlated DGP to test CPI - x1 and x2 are correlated, x1 is important, x2 is not
	task = sim_dgp_correlated(n = 300, r = 0.7)
	learner = mlr3::lrn("regr.ranger", num.trees = 100)
	measure = mlr3::msr("regr.mse")

	# CPI requires cross-validation or similar resampling with multiple folds
	resampling = mlr3::rsmp("cv", folds = 5)

	# Create CFI with KnockoffGaussianSampler
	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler
	)

	# Check that CPI is in the variance methods registry
	expect_true("cpi" %in% cfi$.__enclos_env__$private$.ci_methods)

	cfi$compute()

	# Test CPI variance method
	cpi_result = cfi$importance(ci_method = "cpi")

	# Check structure
	expect_importance_dt(cpi_result, features = cfi$features)
	expected_cols = c(
		"feature",
		"importance",
		"se",
		"statistic",
		"p.value",
		"conf_lower",
		"conf_upper"
	)
	expect_true(all(expected_cols %in% names(cpi_result)))

	# Check that all values are finite
	expect_true(all(is.finite(cpi_result$importance)))
	expect_true(all(is.finite(cpi_result$se)))
	expect_true(all(is.finite(cpi_result$statistic)))
	expect_true(all(is.finite(cpi_result$p.value)))
	expect_true(all(is.finite(cpi_result$conf_lower)))

	# Upper confidence bound should be Inf for one-sided test
	expect_true(all(is.infinite(cpi_result$conf_upper)))

	# P-values should be in [0, 1]
	expect_true(all(cpi_result$p.value >= 0 & cpi_result$p.value <= 1))

	# SE should be positive
	expect_true(all(cpi_result$se > 0))

	# For correlated DGP, x1 and x3 should have lower p-values (more important)
	# than x2 and x4 (unimportant)
	important_pvals = cpi_result[feature %in% c("x1", "x3")]$p.value
	unimportant_pvals = cpi_result[feature %in% c("x2", "x4")]$p.value

	# On average, important features should have smaller p-values
	expect_lt(mean(important_pvals), mean(unimportant_pvals))
})

test_that("CFI with CPI warning on problematic resampling", {
	skip_if_not_installed("knockoff")

	set.seed(123)
	# Use correlated DGP to test CPI - x1 and x2 are correlated, x1 is important, x2 is not
	task = sim_dgp_correlated(n = 50)
	learner = mlr3::lrn("regr.rpart")
	measure = mlr3::msr("regr.mse")

	# CPI requires cross-validation or similar resampling with multiple folds
	resampling = mlr3::rsmp("subsampling", repeats = 5)

	# Create CFI with KnockoffGaussianSampler
	gaussian_sampler = KnockoffGaussianSampler$new(task)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler
	)
	cfi$compute()

	# Test CPI variance method
	expect_warning(
		cpi_result <- cfi$importance(ci_method = "cpi"),
		regexp = "duplicated observation"
	)

	# Check structure
	expect_importance_dt(cpi_result, features = cfi$features)
	expected_cols = c(
		"feature",
		"importance",
		"se",
		"statistic",
		"p.value",
		"conf_lower",
		"conf_upper"
	)

	resampling = mlr3::rsmp("cv", folds = 5)
	cfi = CFI$new(
		task = task,
		learner = learner,
		measure = measure,
		resampling = resampling,
		sampler = gaussian_sampler
	)
	cfi$compute()
	expect_silent(
		cfi$importance(ci_method = "cpi")
	)
})
