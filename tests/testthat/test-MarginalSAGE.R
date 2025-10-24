test_that("MarginalSAGE can't be constructed without args", {
	expect_error(MarginalSAGE$new())
})

test_that("MarginalSAGE can be constructed with simple objects", {
	# Test with binary classification
	set.seed(123)
	task_binary = mlr3::tgen("2dnormals")$generate(n = 100)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Test with multiclass classification
	set.seed(123)
	task_multi = mlr3::tgen("cassini")$generate(n = 100)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_multi, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)

	# Test with regression
	set.seed(123)
	task_regr = mlr3::tgen("friedman1")$generate(n = 100)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		n_permutations = 2L
	)
	checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
})

test_that("MarginalSAGE null result for featureless learner", {
	set.seed(123)

	# Test with binary classification
	task_binary = mlr3::tgen("xor")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_permutations = 2L
	)
	sage_binary$compute()
	expected_binary = data.table::data.table(
		feature = sage_binary$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_binary$importance(), expected_binary)

	# Test with multiclass classification
	task_multi = mlr3::tgen("cassini")$generate(n = 200)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = mlr3::lrn("classif.featureless", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_permutations = 2L
	)
	sage_multi$compute()
	expected_multi = data.table::data.table(
		feature = sage_multi$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_multi$importance(), expected_multi)

	# Test with regression
	task_regr = mlr3::tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = mlr3::lrn("regr.featureless"),
		measure = mlr3::msr("regr.mse"),
		n_permutations = 2L
	)
	sage_regr$compute()
	expected_regr = data.table::data.table(
		feature = sage_regr$features,
		importance = 0,
		key = "feature"
	)
	expect_identical(sage_regr$importance(), expected_regr)
})

test_that("MarginalSAGE with friedman1 produces sensible results", {
	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 200)
	learner = mlr3::lrn("regr.rpart")
	measure = mlr3::msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L, # Keep small for fast testing
		n_samples = 50L
	)

	sage$compute()
	result = sage$importance()
	expect_importance_dt(result, features = sage$features)

	# Check that important features (important1-5) generally have higher scores
	# than unimportant features (unimportant1-5)
	important_features = grep("^important", result$feature, value = TRUE)
	unimportant_features = grep("^unimportant", result$feature, value = TRUE)

	important_scores = result[feature %in% important_features]$importance
	unimportant_scores = result[feature %in% unimportant_features]$importance

	# On average, important features should have higher SAGE values
	expect_gt(mean(important_scores), mean(unimportant_scores))

	# Check that scores are finite and not all zero
	expect_true(all(is.finite(result$importance)))
	expect_gt(max(abs(result$importance)), 0)
})

test_that("MarginalSAGE with multiple resampling iterations", {
	set.seed(123)

	# Test with binary classification
	task_binary = mlr3::tgen("xor")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		resampling = mlr3::rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)
	checkmate::expect_data_table(
		sage_binary$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage_binary$resampling$iters * length(sage_binary$features),
		ncols = 3,
		any.missing = FALSE,
		min.cols = 3
	)

	# Test with regression
	task_regr = mlr3::tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		resampling = mlr3::rsmp("cv", folds = 3),
		n_permutations = 2L
	)
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)
	checkmate::expect_data_table(
		sage_regr$scores(),
		types = c("integer", "character", "numeric"),
		nrows = sage_regr$resampling$iters * length(sage_regr$features),
		ncols = 3,
		any.missing = FALSE,
		min.cols = 3
	)
})

test_that("MarginalSAGE only one feature", {
	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 100)

	sage = MarginalSAGE$new(
		task = task,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		features = "important4",
		n_permutations = 2L
	)

	sage$compute()
	expect_importance_dt(sage$importance(), features = "important4")

	# Should only have one feature
	expect_equal(nrow(sage$importance()), 1L)
	expect_equal(sage$importance()$feature, "important4")
})

test_that("MarginalSAGE with n_samples parameter", {
	set.seed(123)

	# Test with binary classification
	task_binary = mlr3::tgen("2dnormals")$generate(n = 200)
	sage_binary = MarginalSAGE$new(
		task = task_binary,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_binary$compute()
	expect_importance_dt(sage_binary$importance(), features = sage_binary$features)

	# Test with regression
	task_regr = mlr3::tgen("friedman1")$generate(n = 200)
	sage_regr = MarginalSAGE$new(
		task = task_regr,
		learner = mlr3::lrn("regr.rpart"),
		measure = mlr3::msr("regr.mse"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_regr$compute()
	expect_importance_dt(sage_regr$importance(), features = sage_regr$features)

	# Test with multiclass classification
	task_multi = mlr3::tgen("cassini")$generate(n = 200)
	sage_multi = MarginalSAGE$new(
		task = task_multi,
		learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
		measure = mlr3::msr("classif.ce"),
		n_samples = 30L,
		n_permutations = 2L
	)
	sage_multi$compute()
	expect_importance_dt(sage_multi$importance(), features = sage_multi$features)
})

test_that("MarginalSAGE reproducibility with same seed", {
	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 100)
	learner = mlr3::lrn("classif.rpart", predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	set.seed(42)
	sage1 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage1$compute()
	result1 = sage1$importance()

	set.seed(42)
	sage2 = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 3L
	)
	sage2$compute()
	result2 = sage2$importance()

	# Results should be identical with same seed
	expect_equal(result1$importance, result2$importance, tolerance = 1e-10)
})

test_that("MarginalSAGE parameter validation", {
	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)
	learner = mlr3::lrn("classif.rpart", predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	# n_permutations must be positive integer
	expect_error(MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 0L
	))

	expect_error(MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = -1L
	))
})

test_that("MarginalSAGE requires predict_type='prob' for classification", {
	set.seed(123)
	task = mlr3::tgen("2dnormals")$generate(n = 50)
	learner = mlr3::lrn("classif.rpart", predict_type = "response") # Default is response
	measure = mlr3::msr("classif.ce")

	# Should error for classification without predict_type = "prob"
	expect_error(
		MarginalSAGE$new(
			task = task,
			learner = learner,
			measure = measure
		),
		"Classification learners require probability predictions for SAGE."
	)

	# Should work fine for regression
	task_regr = mlr3::tgen("friedman1")$generate(n = 50)
	learner_regr = mlr3::lrn("regr.rpart")

	expect_silent(
		MarginalSAGE$new(
			task = task_regr,
			learner = learner_regr,
			resampling = mlr3::rsmp("holdout"),
			measure = mlr3::msr("regr.mse")
		)
	)
})

test_that("MarginalSAGE works with multiclass classification", {
	set.seed(123)
	task = mlr3::tgen("cassini")$generate(n = 50)
	learner = mlr3::lrn("classif.rpart", predict_type = "prob")
	measure = mlr3::msr("classif.ce")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 2L,
		n_samples = 30L
	)

	sage$compute()
	result = sage$importance()
	expect_importance_dt(result, features = sage$features)

	# Check that scores are finite and not all zero
	expect_true(all(is.finite(result$importance)))
	expect_gt(max(abs(result$importance)), 0)

	# Verify task has 3 classes
	expect_equal(length(task$class_names), 3L)
})


test_that("MarginalSAGE SE tracking in convergence_history", {
	set.seed(123)
	task = mlr3::tgen("friedman1")$generate(n = 30)
	learner = mlr3::lrn("regr.rpart")
	measure = mlr3::msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 6L,
		n_samples = 20L
	)

	# Compute with early stopping to get convergence history
	result = sage$compute(early_stopping = TRUE, se_threshold = 0.05, check_interval = 2L)

	# Check that convergence_history exists and has SE column
	expect_false(is.null(sage$convergence_history))
	expect_true("se" %in% colnames(sage$convergence_history))

	# Check structure of convergence_history
	expected_cols = c("n_permutations", "feature", "importance", "se")
	expect_equal(sort(colnames(sage$convergence_history)), sort(expected_cols))

	# SE values should be non-negative and finite
	se_values = sage$convergence_history$se
	expect_true(all(se_values >= 0, na.rm = TRUE))
	expect_true(all(is.finite(se_values)))

	# For each feature, SE should generally decrease with more permutations
	# Since this is stochastic, we just check that SEs are reasonable (not increasing drastically)
	for (feat in unique(sage$convergence_history$feature)) {
		feat_data = sage$convergence_history[feature == feat]
		feat_data = feat_data[order(n_permutations)]

		if (nrow(feat_data) > 1) {
			# Just check that SE values are in a reasonable range and not exploding
			expect_true(all(feat_data$se < 10)) # Reasonable upper bound
			expect_true(all(diff(feat_data$se) < 5)) # No huge jumps in SE
		}
	}

	# All features should be represented in convergence history
	expect_equal(
		sort(unique(sage$convergence_history$feature)),
		sort(sage$features)
	)
})

test_that("MarginalSAGE SE-based convergence detection", {
	set.seed(123)
	skip_if_not_installed("ranger")

	skip_if_not_installed("mlr3learners")
	task = mlr3::tgen("friedman1")$generate(n = 100)
	learner = mlr3::lrn("regr.ranger", num.trees = 50)
	measure = mlr3::msr("regr.mse")

	sage = MarginalSAGE$new(
		task = task,
		learner = learner,
		measure = measure,
		n_permutations = 10L,
		n_samples = 20L
	)

	# Test with very loose SE threshold (should not trigger convergence)
	result_loose = sage$compute(
		early_stopping = TRUE,
		convergence_threshold = 0.001, # Very strict relative change
		se_threshold = 100.0, # Very loose SE threshold
		min_permutations = 10L,
		check_interval = 2L
	)

	# Should not converge early due to loose SE threshold
	expect_false(sage$converged)
	expect_equal(sage$n_permutations_used, 10L)

	# Reset for next test
	sage$reset()

	# Test with very strict SE threshold (should trigger convergence quickly)
	result_strict = sage$compute(
		early_stopping = TRUE,
		convergence_threshold = 1.0, # Very loose relative change
		se_threshold = 0.001, # Very strict SE threshold
		min_permutations = 5L,
		check_interval = 2L
	)

	# With very strict SE threshold, should not converge early
	# (realistic SE values are usually larger than 0.001)
	expect_false(sage$converged)

	# Test with moderate thresholds where both criteria might be met
	sage$reset()

	result_moderate = sage$compute(
		early_stopping = TRUE,
		convergence_threshold = 0.05, # Moderate relative change threshold
		se_threshold = 0.1, # Moderate SE threshold
		min_permutations = 6L,
		check_interval = 2L
	)

	# Should have convergence history with SE tracking regardless of convergence
	expect_false(is.null(sage$convergence_history))
	expect_true("se" %in% colnames(sage$convergence_history))
})
