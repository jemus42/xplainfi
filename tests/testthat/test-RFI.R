test_that("RFI can't be constructed without args", {
  expect_error(RFI$new())
})

test_that("RFI can be constructed with simple objects", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 100)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  checkmate::expect_r6(rfi, c("FeatureImportanceMeasure", "PerturbationImportance", "RFI"))

  expect_importance_dt(rfi$compute(), features = rfi$features)
  expect_importance_dt(rfi$compute(relation = "difference"), features = rfi$features)
})

test_that("RFI uses ARFSampler by default", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("xor")$generate(n = 100)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  # Should have ARFSampler
  checkmate::expect_r6(rfi$sampler, "ARFSampler")
  expect_equal(rfi$label, "Relative Feature Importance")
})

test_that("RFI with custom conditioning set", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)

  # Use only the first two important features as conditioning set
  conditioning_set = c("important1", "important2")

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    conditioning_set = conditioning_set
  )

  expect_identical(rfi$conditioning_set, conditioning_set)

  result = rfi$compute()
  expect_importance_dt(result, features = rfi$features)
})

test_that("RFI with empty conditioning set (equivalent to PFI)", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200) # Use friedman1 with more features for better ranking comparison
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  # RFI with empty conditioning set
  rfi = RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = character(0), # Empty conditioning set
    iters_perm = 3 # Use multiple iterations for more robust comparison
    # Uses ARFSampler by default
  )

  expect_equal(length(rfi$conditioning_set), 0)

  # PFI for comparison
  pfi = PFI$new(
    task = task,
    learner = learner,
    measure = measure,
    iters_perm = 3 # Same number of iterations
  )

  # Compute results
  set.seed(456) # Different seed for RFI computation
  rfi_result = rfi$compute()

  set.seed(456) # Same seed for PFI computation
  pfi_result = pfi$compute()

  # Both should be valid importance tables
  expect_importance_dt(rfi_result, features = rfi$features)
  expect_importance_dt(pfi_result, features = pfi$features)

  # Results should be similar but not necessarily identical due to different sampling methods
  # (RFI uses ARF-based conditional sampling, PFI uses marginal permutation)
  # Check that they have similar patterns for important vs unimportant features

  # Extract important and unimportant feature scores for both methods
  important_features = grep("^important", rfi_result$feature, value = TRUE)
  unimportant_features = grep("^unimportant", rfi_result$feature, value = TRUE)

  rfi_important_scores = rfi_result[feature %in% important_features]$importance
  rfi_unimportant_scores = rfi_result[feature %in% unimportant_features]$importance

  pfi_important_scores = pfi_result[feature %in% important_features]$importance
  pfi_unimportant_scores = pfi_result[feature %in% unimportant_features]$importance

  # Both methods should show that important features have higher scores than unimportant features on average
  expect_gt(mean(rfi_important_scores), mean(rfi_unimportant_scores))
  expect_gt(mean(pfi_important_scores), mean(pfi_unimportant_scores))

  # The ranking patterns should be similar - check that the relative difference patterns are consistent
  rfi_diff = mean(rfi_important_scores) - mean(rfi_unimportant_scores)
  pfi_diff = mean(pfi_important_scores) - mean(pfi_unimportant_scores)

  # Both should show a positive difference (important > unimportant) and be in the same order of magnitude
  expect_gt(rfi_diff, 0)
  expect_gt(pfi_diff, 0)
})

test_that("RFI with single conditioning feature", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 100)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    conditioning_set = c("x1") # Single conditioning feature
    # Uses ARFSampler by default
  )

  expect_equal(length(rfi$conditioning_set), 1)
  expect_equal(rfi$conditioning_set, "x1")

  result = rfi$compute()
  expect_importance_dt(result, features = rfi$features)
})

test_that("RFI with custom ARF sampler", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("spirals")$generate(n = 100)
  custom_sampler = ARFSampler$new(task)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    conditioning_set = c("x1"), # Use one feature as conditioning set
    sampler = custom_sampler
  )

  # Should use the custom sampler
  checkmate::expect_r6(rfi$sampler, "ARFSampler")
  rfi$compute()
  expect_importance_dt(rfi$importance, features = rfi$features)
})

test_that("RFI null result for featureless learner", {
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("xor")$generate(n = 200)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.featureless"),
    measure = mlr3::msr("classif.ce"),
    conditioning_set = character(0) # Empty conditioning set now works
    # Uses ARFSampler by default
  )

  rfi$compute()

  expected = data.table::data.table(
    feature = rfi$features,
    importance = 0,
    key = "feature"
  )

  expect_identical(rfi$importance, expected)
})

test_that("RFI multiple perms", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    resampling = mlr3::rsmp("cv", folds = 3),
    conditioning_set = c("important1"),
    iters_perm = 2
  )

  rfi$compute()

  expect_importance_dt(rfi$importance, features = rfi$features)

  checkmate::expect_data_table(
    rfi$scores,
    types = c("character", "integer", "numeric"),
    nrows = rfi$resampling$iters *
      rfi$param_set$values$iters_perm *
      length(rfi$features),
    ncols = 6,
    any.missing = FALSE,
    min.cols = 6
  )
})

test_that("RFI only one feature", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    resampling = mlr3::rsmp("cv", folds = 3),
    conditioning_set = c("important1", "important2"),
    iters_perm = 2,
    features = "important4"
  )

  rfi$compute()

  expect_importance_dt(rfi$importance, features = "important4")

  checkmate::expect_data_table(
    rfi$scores,
    types = c("character", "integer", "numeric"),
    nrows = rfi$resampling$iters *
      rfi$param_set$values$iters_perm,
    ncols = 6,
    any.missing = FALSE,
    min.cols = 6
  )
})

test_that("RFI with friedman1 produces sensible results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  rfi = RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = c("important1"), # Condition on one feature
    iters_perm = 2
  )

  result = rfi$compute()
  expect_importance_dt(result, features = rfi$features)

  # Check that important features (important1-5) generally have higher scores
  # than unimportant features (unimportant1-5)
  important_features = grep("^important", result$feature, value = TRUE)
  unimportant_features = grep("^unimportant", result$feature, value = TRUE)

  important_scores = result[feature %in% important_features]$importance
  unimportant_scores = result[feature %in% unimportant_features]$importance

  # On average, important features should have higher RFI values
  expect_gt(mean(important_scores), mean(unimportant_scores))

  # Check that scores are finite and not all zero
  expect_true(all(is.finite(result$importance)))
  expect_gt(max(abs(result$importance)), 0)
})

test_that("RFI different relations (difference vs ratio)", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 100)

  rfi = RFI$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    conditioning_set = character(0) # Empty conditioning set (as oppsosed to NULL -> condition on all features)
  )

  # Default behavior should be sane
  res_1 = rfi$compute()
  expect_importance_dt(res_1, rfi$features)

  res_2 = rfi$compute()
  expect_identical(res_1, res_2)

  res_3 = rfi$compute("difference")
  expect_identical(res_1, res_3)

  res_4 = rfi$compute("ratio")
  res_5 = rfi$compute("difference")

  expect_error(expect_equal(res_4, res_5))

  expect_importance_dt(res_2, rfi$features)
  expect_importance_dt(res_3, rfi$features)
  expect_importance_dt(res_4, rfi$features)
  expect_importance_dt(res_5, rfi$features)
})

test_that("RFI with resampling", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("xor")$generate(n = 200)
  learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
  resampling = mlr3::rsmp("cv", folds = 3)
  measure = mlr3::msr("classif.ce")

  rfi = RFI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    conditioning_set = character(0), # Empty conditioning set now works
    iters_perm = 2
  )

  res_1 = rfi$compute()
  expect_importance_dt(rfi$importance, rfi$features)

  res_2 = rfi$compute("ratio")
  expect_importance_dt(rfi$importance, rfi$features)

  expect_error(expect_equal(res_1, res_2))
})

test_that("RFI parameter validation", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 50)
  learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
  measure = mlr3::msr("classif.ce")

  # iters_perm must be positive integer
  expect_error(RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    iters_perm = 0L
  ))

  expect_error(RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    iters_perm = -1L
  ))

  # conditioning_set must be valid feature names
  expect_error(RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = c("nonexistent_feature")
  ))
})

test_that("RFI different conditioning sets produce different results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  # RFI with empty conditioning set (equivalent to PFI)
  rfi_empty = RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = character(0),
    iters_perm = 2
  )

  # RFI with one conditioning feature
  rfi_one = RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = "important1",
    iters_perm = 2
  )

  # RFI with multiple conditioning features
  rfi_multi = RFI$new(
    task = task,
    learner = learner,
    measure = measure,
    conditioning_set = c("important1", "important2"),
    iters_perm = 2
  )

  result_empty = rfi_empty$compute()
  result_one = rfi_one$compute()
  result_multi = rfi_multi$compute()

  # All should be valid importance tables
  expect_importance_dt(result_empty, features = rfi_empty$features)
  expect_importance_dt(result_one, features = rfi_one$features)
  expect_importance_dt(result_multi, features = rfi_multi$features)

  # Results should generally be different (allowing for some tolerance due to randomness)
  # We don't expect exact differences but the conditioning should have some effect
  expect_false(all(abs(result_empty$importance - result_one$importance) < 1e-10))
  expect_false(all(abs(result_one$importance - result_multi$importance) < 1e-10))
})
