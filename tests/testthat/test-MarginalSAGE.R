test_that("MarginalSAGE can't be constructed without args", {
  expect_error(MarginalSAGE$new())
})

test_that("MarginalSAGE can be constructed with simple objects", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  # Test with binary classification
  set.seed(123)
  task_binary = mlr3::tgen("2dnormals")$generate(n = 100)
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
  expect_importance_dt(sage_binary$compute(), features = sage_binary$features)

  # Test with multiclass classification
  set.seed(123)
  task_multi = mlr3::tgen("cassini")$generate(n = 100)
  sage_multi = MarginalSAGE$new(
    task = task_multi,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_multi, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
  expect_importance_dt(sage_multi$compute(), features = sage_multi$features)

  # Test with regression
  set.seed(123)
  task_regr = mlr3::tgen("friedman1")$generate(n = 100)
  sage_regr = MarginalSAGE$new(
    task = task_regr,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "MarginalSAGE"))
  expect_importance_dt(sage_regr$compute(), features = sage_regr$features)
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
    importance = 0
  )
  expect_identical(sage_binary$importance, expected_binary)

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
    importance = 0
  )
  expect_identical(sage_multi$importance, expected_multi)

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
    importance = 0
  )
  expect_identical(sage_regr$importance, expected_regr)
})

test_that("MarginalSAGE with friedman1 produces sensible results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 3L, # Keep small for fast testing
    max_reference_size = 50L
  )

  result = sage$compute()
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
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)

  # Test with binary classification
  task_binary = mlr3::tgen("xor")$generate(n = 200)
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    resampling = mlr3::rsmp("cv", folds = 3),
    n_permutations = 2L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance, features = sage_binary$features)
  checkmate::expect_data_table(
    sage_binary$scores,
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
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    resampling = mlr3::rsmp("cv", folds = 3),
    n_permutations = 2L
  )
  sage_regr$compute()
  expect_importance_dt(sage_regr$importance, features = sage_regr$features)
  checkmate::expect_data_table(
    sage_regr$scores,
    types = c("integer", "character", "numeric"),
    nrows = sage_regr$resampling$iters * length(sage_regr$features),
    ncols = 3,
    any.missing = FALSE,
    min.cols = 3
  )
})

test_that("MarginalSAGE only one feature", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 100)

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("regr.ranger", num.trees = 50),
    measure = mlr3::msr("regr.mse"),
    features = "important4",
    n_permutations = 2L
  )

  sage$compute()
  expect_importance_dt(sage$importance, features = "important4")

  # Should only have one feature
  expect_equal(nrow(sage$importance), 1L)
  expect_equal(sage$importance$feature, "important4")
})

test_that("MarginalSAGE with custom reference data", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)

  # Test with binary classification
  task_binary = mlr3::tgen("2dnormals")$generate(n = 200)
  reference_data_binary = task_binary$data(cols = task_binary$feature_names)[1:20, ]
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    reference_data = reference_data_binary,
    n_permutations = 2L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance, features = sage_binary$features)
  expect_equal(nrow(sage_binary$reference_data), 20L)

  # Test with regression
  task_regr = mlr3::tgen("friedman1")$generate(n = 200)
  reference_data_regr = task_regr$data(cols = task_regr$feature_names)[1:20, ]
  sage_regr = MarginalSAGE$new(
    task = task_regr,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    reference_data = reference_data_regr,
    n_permutations = 2L
  )
  sage_regr$compute()
  expect_importance_dt(sage_regr$importance, features = sage_regr$features)
  expect_equal(nrow(sage_regr$reference_data), 20L)
})

test_that("MarginalSAGE with max_reference_size parameter", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)

  # Test with binary classification
  task_binary = mlr3::tgen("2dnormals")$generate(n = 200)
  sage_binary = MarginalSAGE$new(
    task = task_binary,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    max_reference_size = 30L,
    n_permutations = 2L
  )
  sage_binary$compute()
  expect_importance_dt(sage_binary$importance, features = sage_binary$features)
  expect_lte(nrow(sage_binary$reference_data), 30L)

  # Test with regression
  task_regr = mlr3::tgen("friedman1")$generate(n = 200)
  sage_regr = MarginalSAGE$new(
    task = task_regr,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    max_reference_size = 30L,
    n_permutations = 2L
  )
  sage_regr$compute()
  expect_importance_dt(sage_regr$importance, features = sage_regr$features)
  expect_lte(nrow(sage_regr$reference_data), 30L)

  # Test with multiclass classification
  task_multi = mlr3::tgen("cassini")$generate(n = 200)
  sage_multi = MarginalSAGE$new(
    task = task_multi,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    max_reference_size = 30L,
    n_permutations = 2L
  )
  sage_multi$compute()
  expect_importance_dt(sage_multi$importance, features = sage_multi$features)
  expect_lte(nrow(sage_multi$reference_data), 30L)
})

test_that("MarginalSAGE reproducibility with same seed", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 100)
  learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
  measure = mlr3::msr("classif.ce")

  set.seed(42)
  sage1 = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 3L
  )
  result1 = sage1$compute()

  set.seed(42)
  sage2 = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 3L
  )
  result2 = sage2$compute()

  # Results should be identical with same seed
  expect_equal(result1$importance, result2$importance, tolerance = 1e-10)
})

test_that("MarginalSAGE parameter validation", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 50)
  learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
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
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 50)
  learner = mlr3::lrn("classif.ranger", num.trees = 10) # Default is response
  measure = mlr3::msr("classif.ce")

  # Should error for classification without predict_type = "prob"
  expect_error(
    MarginalSAGE$new(
      task = task,
      learner = learner,
      measure = measure
    ),
    "Classification learners must use predict_type = 'prob' for SAGE"
  )

  # Should work fine for regression
  task_regr = mlr3::tgen("friedman1")$generate(n = 50)
  learner_regr = mlr3::lrn("regr.ranger", num.trees = 10)

  expect_silent(
    MarginalSAGE$new(
      task = task_regr,
      learner = learner_regr,
      measure = mlr3::msr("regr.mse")
    )
  )
})

test_that("MarginalSAGE works with multiclass classification", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("cassini")$generate(n = 200)
  learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
  measure = mlr3::msr("classif.ce")

  sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 3L,
    max_reference_size = 50L
  )

  result = sage$compute()
  expect_importance_dt(result, features = sage$features)

  # Check that scores are finite and not all zero
  expect_true(all(is.finite(result$importance)))
  expect_gt(max(abs(result$importance)), 0)

  # Verify task has 3 classes
  expect_equal(length(task$class_names), 3L)
})
