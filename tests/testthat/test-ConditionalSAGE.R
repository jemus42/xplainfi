test_that("ConditionalSAGE can't be constructed without args", {
  expect_error(ConditionalSAGE$new())
})

test_that("ConditionalSAGE can be constructed with simple objects", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  # Test with binary classification
  set.seed(123)
  task_binary = mlr3::tgen("2dnormals")$generate(n = 100)
  sage_binary = ConditionalSAGE$new(
    task = task_binary,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_binary, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
  expect_importance_dt(sage_binary$compute(), features = sage_binary$features)

  # Test with multiclass classification
  set.seed(123)
  task_multi = mlr3::tgen("cassini")$generate(n = 100)
  sage_multi = ConditionalSAGE$new(
    task = task_multi,
    learner = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_multi, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
  expect_importance_dt(sage_multi$compute(), features = sage_multi$features)

  # Test with regression
  set.seed(123)
  task_regr = mlr3::tgen("friedman1")$generate(n = 100)
  sage_regr = ConditionalSAGE$new(
    task = task_regr,
    learner = mlr3::lrn("regr.ranger", num.trees = 10),
    measure = mlr3::msr("regr.mse"),
    n_permutations = 2L
  )
  checkmate::expect_r6(sage_regr, c("FeatureImportanceMethod", "SAGE", "ConditionalSAGE"))
  expect_importance_dt(sage_regr$compute(), features = sage_regr$features)
})

test_that("ConditionalSAGE null result for featureless learner", {
  skip_if_not_installed("arf")

  set.seed(123)

  # Test with binary classification
  task_binary = mlr3::tgen("xor")$generate(n = 200)
  sage_binary = ConditionalSAGE$new(
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
  sage_multi = ConditionalSAGE$new(
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
  sage_regr = ConditionalSAGE$new(
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

test_that("ConditionalSAGE with friedman1 produces sensible results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 200)
  learner = mlr3::lrn("regr.ranger", num.trees = 50)
  measure = mlr3::msr("regr.mse")

  sage = ConditionalSAGE$new(
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

test_that("ConditionalSAGE uses ARFSampler by default", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("xor")$generate(n = 100)

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )

  # Should have ARFSampler
  checkmate::expect_r6(sage$sampler, "ARFSampler")
  expect_equal(sage$label, "Conditional SAGE")
})

test_that("ConditionalSAGE with custom sampler", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("spirals")$generate(n = 200)
  custom_sampler = MarginalSampler$new(task)

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    sampler = custom_sampler,
    n_permutations = 2L
  )

  # Should use the custom sampler
  checkmate::expect_r6(sage$sampler, "MarginalSampler")
  sage$compute()
  expect_importance_dt(sage$importance, features = sage$features)
})

test_that("ConditionalSAGE requires predict_type='prob' for classification", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)
  task = mlr3::tgen("2dnormals")$generate(n = 50)
  learner = mlr3::lrn("classif.ranger", num.trees = 10) # Default is response
  measure = mlr3::msr("classif.ce")

  # Should error for ConditionalSAGE
  expect_error(
    ConditionalSAGE$new(
      task = task,
      learner = learner,
      measure = measure
    ),
    "Classification learners must use predict_type = 'prob' for SAGE"
  )
})

test_that("ConditionalSAGE works with multiclass classification", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")

  set.seed(123)
  task = mlr3::tgen("simplex")$generate(n = 150)
  learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob")
  measure = mlr3::msr("classif.ce")

  sage = ConditionalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 2L, # Keep small for fast testing
    max_reference_size = 30L
  )

  result = sage$compute()
  expect_importance_dt(result, features = sage$features)

  # Check that scores are finite and not all zero
  expect_true(all(is.finite(result$importance)))
  expect_gt(max(abs(result$importance)), 0)

  # Verify task has 4 classes
  expect_equal(length(task$class_names), 4L)
})
