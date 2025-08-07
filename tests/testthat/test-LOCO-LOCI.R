test_that("LeaveOutIn base class works correctly", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  task = mlr3::tgen("cassini")$generate(n = 180)

  # LeaveOutIn should work but is intended to be used via LOCO/LOCI
  loi = LeaveOutIn$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    direction = "leave-out",
    label = "test"
  )

  checkmate::expect_r6(loi, c("FeatureImportanceMethod", "LeaveOutIn"))
  expect_equal(loi$direction, "leave-out")

  # Should be able to compute
  loi$compute()
  expect_importance_dt(loi$importance(), features = loi$features)
})

test_that("LOCO can't be constructed without args", {
  expect_error(LOCO$new())
})

test_that("LOCO can be constructed with simple objects", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("simplex", d = 5)$generate(n = 200)

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  checkmate::expect_r6(loco, c("FeatureImportanceMethod", "LeaveOutIn", "LOCO"))
  expect_equal(loco$direction, "leave-out")
  expect_equal(loco$label, "Leave-One-Covariate-Out (LOCO)")

  loco$compute()
  expect_importance_dt(loco$importance(), features = loco$features)
})

test_that("LOCI can't be constructed without args", {
  expect_error(LOCI$new())
})

test_that("LOCI can be constructed with simple objects", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("simplex", d = 5)$generate(n = 200)

  loci = LOCI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  checkmate::expect_r6(loci, c("FeatureImportanceMethod", "LeaveOutIn", "LOCI"))
  expect_equal(loci$direction, "leave-in")
  expect_equal(loci$label, "Leave-One-Covariate-In (LOCI)")

  loci$compute()
  expect_importance_dt(loci$importance(), features = loci$features)
})

test_that("LOCO and LOCI produce different results", {
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

  loci = LOCI$new(
    task = task,
    learner = learner,
    measure = measure
  )

  loco$compute()
  loco_results = loco$importance()
  loci$compute()
  loci_results = loci$importance()

  expect_importance_dt(loco_results, features = loco$features)
  expect_importance_dt(loci_results, features = loci$features)

  # Results should be different
  expect_false(all(loco_results$importance == loci_results$importance))
})

test_that("LOCO null result for featureless learner", {
  set.seed(123)
  task = mlr3::tgen("xor", d = 4)$generate(n = 200)

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("classif.featureless"),
    measure = mlr3::msr("classif.ce")
  )

  loco$compute()

  expected = data.table::data.table(
    feature = loco$features,
    importance = 0,
    key = "feature"
  )

  expect_identical(loco$importance(), expected)
})

test_that("LOCI for featureless learner shows all features as equally bad", {
  set.seed(123)
  task = mlr3::tgen("xor", d = 4)$generate(n = 200)

  loci = LOCI$new(
    task = task,
    learner = mlr3::lrn("classif.featureless"),
    measure = mlr3::msr("classif.ce")
  )

  loci$compute()

  # All features should have the same (zero) importance
  expect_equal(length(unique(loci$importance()$importance)), 1)
})

test_that("LOCO with friedman1 produces sensible results", {
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

  loco$compute()
  result = loco$importance()
  expect_importance_dt(result, features = loco$features)

  # Check that important features (important1-5) generally have higher LOCO values
  # than unimportant features (unimportant1-5)
  important_features = grep("^important", result$feature, value = TRUE)
  unimportant_features = grep("^unimportant", result$feature, value = TRUE)

  important_scores = result[feature %in% important_features]$importance
  unimportant_scores = result[feature %in% unimportant_features]$importance

  # On average, important features should have higher LOCO values
  expect_gt(mean(important_scores), mean(unimportant_scores))

  # Check that scores are finite and not all zero
  expect_true(all(is.finite(result$importance)))
  expect_gt(max(abs(result$importance)), 0)
})

test_that("LOCI with friedman1 produces sensible results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  loci = LOCI$new(
    task = task,
    learner = learner,
    measure = measure
  )

  loci$compute()
  result = loci$importance()
  expect_importance_dt(result, features = loci$features)

  # Check that important features (important1-5) generally have lower LOCI values
  # (lower error when only using that feature) than unimportant features
  important_features = grep("^important", result$feature, value = TRUE)
  unimportant_features = grep("^unimportant", result$feature, value = TRUE)

  important_scores = result[feature %in% important_features]$importance
  unimportant_scores = result[feature %in% unimportant_features]$importance

  # For LOCI with difference relation and minimize=TRUE measure,
  # positive values mean the feature alone performs better than featureless baseline
  # So important features should have higher (more positive) scores than unimportant features
  expect_gt(mean(important_scores), mean(unimportant_scores))
})

test_that("LOCO/LOCI different relations (difference vs ratio)", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("cassini")$generate(n = 180)

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  # Default behavior should be sane
  set.seed(123)
  loco$compute()
  res_1 = loco$importance()
  expect_importance_dt(res_1, loco$features)

  set.seed(123)
  loco$compute()
  res_2 = loco$importance()
  expect_identical(res_1, res_2)

  set.seed(123)
  loco$compute("difference")
  res_3 = loco$importance()
  expect_identical(res_1, res_3)

  loco$compute("ratio")
  res_4 = loco$importance()
  loco$compute("difference")
  res_5 = loco$importance()

  expect_error(expect_equal(res_4, res_5))

  expect_importance_dt(res_2, loco$features)
  expect_importance_dt(res_3, loco$features)
  expect_importance_dt(res_4, loco$features)
  expect_importance_dt(res_5, loco$features)
})

test_that("LOCO/LOCI with resampling", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("xor", d = 4)$generate(n = 200)
  learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
  resampling = mlr3::rsmp("cv", folds = 3)
  measure = mlr3::msr("classif.ce")

  loco = LOCO$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure
  )

  loci = LOCI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure
  )

  loco$compute()
  res_loco = loco$importance()
  loci$compute()
  res_loci = loci$importance()

  expect_importance_dt(res_loco, loco$features)
  expect_importance_dt(res_loci, loci$features)

  # Check scores structure
  checkmate::expect_data_table(
    loco$scores,
    types = c("character", "integer", "integer", "numeric", "numeric", "numeric"),
    nrows = loco$resampling$iters * length(loco$features),
    ncols = 6,
    any.missing = FALSE,
    min.cols = 6
  )
})

test_that("LOCO/LOCI with single feature", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    features = "important1"
  )

  loci = LOCI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    features = "important1"
  )

  loco$compute()
  loci$compute()

  expect_importance_dt(loco$importance(), features = "important1")
  expect_importance_dt(loci$importance(), features = "important1")

  expect_equal(nrow(loco$importance()), 1L)
  expect_equal(nrow(loci$importance()), 1L)
})

test_that("LOCO/LOCI with subset of features", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  features_subset = c("important1", "important2", "unimportant1")

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    features = features_subset
  )

  loci = LOCI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    features = features_subset
  )

  loco$compute()
  loci$compute()

  expect_importance_dt(loco$importance(), features = features_subset)
  expect_importance_dt(loci$importance(), features = features_subset)

  expect_equal(nrow(loco$importance()), length(features_subset))
  expect_equal(nrow(loci$importance()), length(features_subset))
  expect_setequal(loco$importance()$feature, features_subset)
  expect_setequal(loci$importance()$feature, features_subset)
})

test_that("LOCO/LOCI with multiple refits (iters_refit)", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 100)
  learner = mlr3::lrn("regr.ranger", num.trees = 10)
  measure = mlr3::msr("regr.mse")

  loco = LOCO$new(
    task = task,
    learner = learner,
    measure = measure,
    features = c("important1", "unimportant1"),
    iters_refit = 3L
  )

  loci = LOCI$new(
    task = task,
    learner = learner,
    measure = measure,
    features = c("important1", "unimportant1"),
    iters_refit = 3L
  )

  loco$compute()
  loci$compute()

  # Check that scores include iter_refit column
  checkmate::expect_data_table(
    loco$scores,
    types = c("character", "integer", "integer", "numeric", "numeric", "numeric"),
    nrows = 6, # 2 features × 1 resampling iter × 3 refits
    ncols = 6,
    any.missing = FALSE
  )

  checkmate::expect_data_table(
    loci$scores,
    types = c("character", "integer", "integer", "numeric", "numeric", "numeric"),
    nrows = 6, # 2 features × 1 resampling iter × 3 refits
    ncols = 6,
    any.missing = FALSE
  )

  # Check that iter_refit column has expected values
  expect_setequal(loco$scores$iter_refit, rep(1:3, 2))
  expect_setequal(loci$scores$iter_refit, rep(1:3, 2))

  # Importance should still be aggregated to one value per feature
  expect_equal(nrow(loco$importance()), 2L)
  expect_equal(nrow(loci$importance()), 2L)
})

test_that("LOCI uses featureless baseline", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 100)

  # Create a simple feature that's very predictive
  task$cbind(data.frame(perfect_feature = task$data()[[task$target_names]]))

  loci = LOCI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    features = "perfect_feature"
  )

  loci$compute()

  # A perfect feature should have very positive LOCI value
  # (much better than featureless baseline)
  expect_gt(loci$importance()$importance, 15)

  # Now test with a useless feature
  set.seed(123)
  task2 = mlr3::tgen("friedman1")$generate(n = 100)
  task2$cbind(data.frame(random_feature = rnorm(100)))

  loci2 = LOCI$new(
    task = task2,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    features = "random_feature"
  )

  loci2$compute()

  # A random feature should perform similar to or worse than featureless baseline
  # (LOCI value near 0 or negative)
  expect_gt(loci2$importance()$importance, -20)
  expect_lt(loci2$importance()$importance, 5)
})

test_that("LOCO/LOCI with obs_loss original formulation", {
  skip_if_not_installed("mlr3learners")

  set.seed(42)
  # Simple regression task
  n = 50
  x1 = runif(n, max = 10)
  x2 = runif(n, max = 10)
  y = x1 + x2 + rnorm(n, sd = 0.1)

  task = mlr3::TaskRegr$new("test", data.table::data.table(x1 = x1, x2 = x2, y = y), target = "y")
  learner = mlr3::lrn("regr.lm")

  # Test LOCO with obs_loss=TRUE using median aggregation
  measure_mae_median = mlr3::msr("regr.mae")
  measure_mae_median$aggregator = median
  loco_obs = LOCO$new(
    task = task,
    learner = learner,
    measure = measure_mae_median,
    obs_loss = TRUE
  )
  loco_obs$compute()
  result_obs = loco_obs$importance()
  expect_importance_dt(result_obs, features = c("x1", "x2"))
  expect_true(all(is.finite(result_obs$importance)))
  expect_gt(max(result_obs$importance), 0) # Should show some importance

  # Test LOCI with obs_loss=TRUE using median aggregation
  measure_mae_median2 = mlr3::msr("regr.mae")
  measure_mae_median2$aggregator = median
  loci_obs = LOCI$new(
    task = task,
    learner = learner,
    measure = measure_mae_median2,
    obs_loss = TRUE
  )
  loci_obs$compute()
  result_loci = loci_obs$importance()
  expect_importance_dt(result_loci, features = c("x1", "x2"))
  expect_true(all(is.finite(result_loci$importance)))
  expect_gt(max(result_loci$importance), 0) # Should show some importance

  # Test different aggregation function (uses measure's default mean)
  loco_mean = LOCO$new(
    task = task,
    learner = learner,
    measure = mlr3::msr("regr.mae"),
    obs_loss = TRUE
  )
  loco_mean$compute()
  result_mean = loco_mean$importance()
  expect_importance_dt(result_mean, features = c("x1", "x2"))

  # Results should be different with different aggregation functions
  expect_false(all(result_obs$importance == result_mean$importance))
})

test_that("LOCO/LOCI obs_loss error handling", {
  skip_if_not_installed("mlr3learners")

  set.seed(42)
  task = mlr3::tgen("friedman1")$generate(n = 50)
  learner = mlr3::lrn("regr.lm")

  # Create a measure without obs_loss support
  measure_no_obs = mlr3::msr("regr.mse")
  measure_no_obs$obs_loss = NULL

  # Should error when obs_loss=TRUE but measure doesn't support it
  expect_error(
    LOCO$new(
      task = task,
      learner = learner,
      measure = measure_no_obs,
      obs_loss = TRUE
    )$compute(),
    "does not support observation-wise loss calculation"
  )
})

test_that("LOCO/LOCI obs_loss vs micro-averaged results differ appropriately", {
  skip_if_not_installed("mlr3learners")

  set.seed(42)
  task = mlr3::tgen("friedman1")$generate(n = 80)
  learner = mlr3::lrn("regr.lm")

  # Micro-averaged LOCO with MSE
  loco_micro = LOCO$new(
    task = task,
    learner = learner,
    measure = mlr3::msr("regr.mse"),
    obs_loss = FALSE
  )
  loco_micro$compute()
  result_micro = loco_micro$importance()

  # Macro-averaged LOCO with MAE and median
  measure_mae_median3 = mlr3::msr("regr.mae")
  measure_mae_median3$aggregator = median
  loco_macro = LOCO$new(
    task = task,
    learner = learner,
    measure = measure_mae_median3,
    obs_loss = TRUE
  )
  loco_macro$compute()
  result_macro = loco_macro$importance()

  # Both should be finite and positive for important features
  expect_true(all(is.finite(result_micro$importance)))
  expect_true(all(is.finite(result_macro$importance)))

  # Important features should have higher importance than unimportant ones in both methods
  important_micro = result_micro[grepl("^important", feature)]$importance
  unimportant_micro = result_micro[grepl("^unimportant", feature)]$importance
  expect_gt(mean(important_micro), mean(unimportant_micro))

  important_macro = result_macro[grepl("^important", feature)]$importance
  unimportant_macro = result_macro[grepl("^unimportant", feature)]$importance
  expect_gt(mean(important_macro), mean(unimportant_macro))
})

test_that("LOCO/LOCI obs_losses field functionality", {
  skip_if_not_installed("mlr3learners")

  set.seed(42)
  # Small dataset for easier testing
  n = 30
  x1 = runif(n, max = 10)
  x2 = runif(n, max = 10)
  y = x1 + x2 + rnorm(n)

  task = mlr3::TaskRegr$new("test", data.table::data.table(x1 = x1, x2 = x2, y = y), target = "y")
  learner = mlr3::lrn("regr.lm")

  # Test LOCO with obs_loss=TRUE stores obs_losses
  measure_mae_median4 = mlr3::msr("regr.mae")
  measure_mae_median4$aggregator = median
  loco_obs = LOCO$new(
    task = task,
    learner = learner,
    measure = measure_mae_median4,
    obs_loss = TRUE
  )
  loco_obs$compute()

  # Should have obs_losses data.table
  expect_false(is.null(loco_obs$obs_losses))
  checkmate::expect_data_table(loco_obs$obs_losses, min.rows = 1, min.cols = 10)

  # Should have expected columns
  expected_cols = c(
    "row_ids",
    "feature",
    "iteration",
    "iter_refit",
    "truth",
    "response_ref",
    "response_feature",
    "loss_ref",
    "loss_feature",
    "obs_diff"
  )
  expect_true(all(expected_cols %in% colnames(loco_obs$obs_losses)))

  # Should have predictions data.table
  expect_false(is.null(loco_obs$predictions))
  checkmate::expect_data_table(loco_obs$predictions, min.rows = 1, min.cols = 4)
  expected_pred_cols = c("feature", "iteration", "iter_refit", "prediction")
  expect_true(all(expected_pred_cols %in% colnames(loco_obs$predictions)))

  # Should have entries for all features
  expect_setequal(unique(loco_obs$obs_losses$feature), c("x1", "x2"))

  # All values should be finite
  numeric_cols = c(
    "truth",
    "response_ref",
    "response_feature",
    "loss_ref",
    "loss_feature",
    "obs_diff"
  )
  for (col in numeric_cols) {
    expect_true(all(is.finite(loco_obs$obs_losses[[col]])))
  }

  # Test micro-averaged LOCO doesn't store obs_losses
  loco_micro = LOCO$new(
    task = task,
    learner = learner,
    measure = mlr3::msr("regr.mse"),
    obs_loss = FALSE
  )
  loco_micro$compute()

  # Should not have obs_losses
  expect_true(is.null(loco_micro$obs_losses))

  # Test LOCI with obs_loss=TRUE
  measure_mae_median5 = mlr3::msr("regr.mae")
  measure_mae_median5$aggregator = median
  loci_obs = LOCI$new(
    task = task,
    learner = learner,
    measure = measure_mae_median5,
    obs_loss = TRUE
  )
  loci_obs$compute()

  # Should have obs_losses data.table
  expect_false(is.null(loci_obs$obs_losses))
  checkmate::expect_data_table(loci_obs$obs_losses, min.rows = 1, min.cols = 10)
  expect_true(all(expected_cols %in% colnames(loci_obs$obs_losses)))
})

test_that("obs_losses and predictions fields survive reset and combine operations", {
  skip_if_not_installed("mlr3learners")

  set.seed(42)
  task = mlr3::tgen("friedman1")$generate(n = 50)
  learner = mlr3::lrn("regr.lm")

  # Create LOCO with obs_loss
  loco = LOCO$new(
    task = task,
    learner = learner,
    measure = mlr3::msr("regr.mae"),
    obs_loss = TRUE,
    features = c("important1", "important2")
  )
  loco$compute()

  # Should have obs_losses
  expect_false(is.null(loco$obs_losses))

  # Test reset clears obs_losses
  loco$reset()
  expect_true(is.null(loco$obs_losses))
  expect_true(is.null(loco$importance()))
  expect_true(is.null(loco$scores))
})

test_that("LOCO micro-averaged and macro-averaged with mean produce equivalent results", {
  skip_if_not_installed("mlr3learners")

  set.seed(1)
  n <- 400
  x1 <- runif(n, max = 10)
  x2 <- runif(n, max = 10)
  y <- x1 + x2 + rnorm(n)

  task_full = mlr3::as_task_regr(data.frame(x1, x2, y), target = "y")
  resampling = mlr3::rsmp("cv", folds = 3)
  resampling$instantiate(task_full)
  learner = mlr3::lrn("regr.ranger")
  measure = mlr3::msr("regr.mse")

  # Micro-averaged approach (default)
  set.seed(1)
  loco_micro <- LOCO$new(
    task = task_full,
    learner = learner,
    resampling = resampling,
    measure = measure
  )
  loco_micro$compute()

  # Macro-averaged approach with mean aggregation (measure's default)
  set.seed(1)
  loco_macro <- LOCO$new(
    task = task_full,
    learner = learner,
    resampling = resampling,
    measure = measure,
    obs_loss = TRUE
  )
  loco_macro$compute()

  # Results should be equivalent when using mean aggregation
  expect_equal(
    loco_micro$importance()$importance,
    loco_macro$importance()$importance,
    tolerance = 1e-10,
    info = "Micro-averaged and macro-averaged with mean should produce identical results"
  )

  # Both should have proper structure
  expect_importance_dt(loco_micro$importance(), features = c("x1", "x2"))
  expect_importance_dt(loco_macro$importance(), features = c("x1", "x2"))

  # Macro-averaged should have obs_losses field
  expect_false(is.null(loco_macro$obs_losses))
  expect_true(is.null(loco_micro$obs_losses))
})
