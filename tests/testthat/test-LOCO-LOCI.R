test_that("LeaveOutIn base class works correctly", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  task = mlr3::tgen("simplex", d = 5)$generate(n = 150)

  # LeaveOutIn should work but is intended to be used via LOCO/LOCI
  loi = LeaveOutIn$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    direction = "leave-out",
    label = "test"
  )

  checkmate::expect_r6(loi, c("FeatureImportanceMeasure", "LeaveOutIn"))
  expect_equal(loi$direction, "leave-out")

  # Should be able to compute
  expect_importance_dt(loi$compute(), features = loi$features)
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

  checkmate::expect_r6(loco, c("FeatureImportanceMeasure", "LeaveOutIn", "LOCO"))
  expect_equal(loco$direction, "leave-out")
  expect_equal(loco$label, "Leave-One-Covariate-Out (LOCO)")

  expect_importance_dt(loco$compute(), features = loco$features)
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

  checkmate::expect_r6(loci, c("FeatureImportanceMeasure", "LeaveOutIn", "LOCI"))
  expect_equal(loci$direction, "leave-in")
  expect_equal(loci$label, "Leave-One-Covariate-In (LOCI)")

  expect_importance_dt(loci$compute(), features = loci$features)
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

  loco_results = loco$compute()
  loci_results = loci$compute()

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

  expect_identical(loco$importance, expected)
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
  expect_equal(length(unique(loci$importance$importance)), 1)
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

  result = loco$compute()
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

  result = loci$compute()
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
  task = mlr3::tgen("simplex", d = 5)$generate(n = 150)

  loco = LOCO$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  # Default behavior should be sane
  res_1 = loco$compute()
  expect_importance_dt(res_1, loco$features)

  res_2 = loco$compute()
  expect_identical(res_1, res_2)

  res_3 = loco$compute("difference")
  expect_identical(res_1, res_3)

  res_4 = loco$compute("ratio")
  res_5 = loco$compute("difference")

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

  res_loco = loco$compute()
  res_loci = loci$compute()

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

  expect_importance_dt(loco$importance, features = "important1")
  expect_importance_dt(loci$importance, features = "important1")

  expect_equal(nrow(loco$importance), 1L)
  expect_equal(nrow(loci$importance), 1L)
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

  expect_importance_dt(loco$importance, features = features_subset)
  expect_importance_dt(loci$importance, features = features_subset)

  expect_equal(nrow(loco$importance), length(features_subset))
  expect_equal(nrow(loci$importance), length(features_subset))
  expect_setequal(loco$importance$feature, features_subset)
  expect_setequal(loci$importance$feature, features_subset)
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
  expect_equal(nrow(loco$importance), 2L)
  expect_equal(nrow(loci$importance), 2L)
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
  expect_gt(loci$importance$importance, 15)
  
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
  expect_gt(loci2$importance$importance, -15)
  expect_lt(loci2$importance$importance, 2)
})
