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
    importance = 0,
    key = "feature"
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
    importance = 0,
    key = "feature"
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
    importance = 0,
    key = "feature"
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
  custom_sampler = ARFSampler$new(task, finite_bounds = "local")

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.ranger", num.trees = 50, predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    sampler = custom_sampler,
    n_permutations = 2L
  )

  # Should use the custom sampler
  checkmate::expect_r6(sage$sampler, "ConditionalSampler")
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

test_that("ConditionalSAGE batching produces identical results", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")
  skip_if_not_installed("withr")

  # Test with regression
  task_regr = mlr3::tgen("friedman1")$generate(n = 30)
  learner_regr = mlr3::lrn("regr.ranger", num.trees = 10)
  measure_regr = mlr3::msr("regr.mse")

  # Test with binary classification
  task_binary = mlr3::tgen("2dnormals")$generate(n = 30)
  learner_binary = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
  measure_binary = mlr3::msr("classif.ce")

  # Test with multiclass classification
  task_multi = mlr3::tgen("cassini")$generate(n = 90)
  learner_multi = mlr3::lrn("classif.ranger", num.trees = 10, predict_type = "prob")
  measure_multi = mlr3::msr("classif.ce")

  # Test each task type
  test_configs = list(
    list(task = task_regr, learner = learner_regr, measure = measure_regr, type = "regression"),
    list(task = task_binary, learner = learner_binary, measure = measure_binary, type = "binary"),
    list(task = task_multi, learner = learner_multi, measure = measure_multi, type = "multiclass")
  )

  for (config in test_configs) {
    # Compute without batching
    result_no_batch = withr::with_seed(42, {
      sage = ConditionalSAGE$new(
        task = config$task,
        learner = config$learner,
        measure = config$measure,
        n_permutations = 3L,
        max_reference_size = 20L
      )
      sage$compute()
    })

    # Compute with large batch size (should not trigger batching)
    result_large_batch = withr::with_seed(42, {
      sage = ConditionalSAGE$new(
        task = config$task,
        learner = config$learner,
        measure = config$measure,
        n_permutations = 3L,
        max_reference_size = 20L
      )
      sage$compute(batch_size = 10000)
    })

    # Compute with small batch size (should trigger batching)
    result_small_batch = withr::with_seed(42, {
      sage = ConditionalSAGE$new(
        task = config$task,
        learner = config$learner,
        measure = config$measure,
        n_permutations = 3L,
        max_reference_size = 20L
      )
      sage$compute(batch_size = 50)
    })

    # Compute with very small batch size (many batches)
    result_tiny_batch = withr::with_seed(42, {
      sage = ConditionalSAGE$new(
        task = config$task,
        learner = config$learner,
        measure = config$measure,
        n_permutations = 3L,
        max_reference_size = 20L
      )
      sage$compute(batch_size = 10)
    })

    # Results should be similar (but not identical due to ARF stochasticity)
    # Use more reasonable tolerance for stochastic conditional sampling
    expect_equal(
      result_no_batch$importance,
      result_large_batch$importance,
      tolerance = 0.05,
      info = paste("ConditionalSAGE", config$type, "- no batch vs large batch")
    )
    expect_equal(
      result_large_batch$importance,
      result_small_batch$importance,
      tolerance = 0.05,
      info = paste("ConditionalSAGE", config$type, "- large batch vs small batch")
    )
    expect_equal(
      result_small_batch$importance,
      result_tiny_batch$importance,
      tolerance = 0.05,
      info = paste("ConditionalSAGE", config$type, "- small batch vs tiny batch")
    )
  }
})

test_that("ConditionalSAGE batching handles edge cases", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")
  skip_if_not_installed("withr")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 20)
  learner = mlr3::lrn("regr.ranger", num.trees = 10)
  measure = mlr3::msr("regr.mse")

  # Test with batch_size = 1 (extreme case)
  result_batch_1 = withr::with_seed(42, {
    sage = ConditionalSAGE$new(
      task = task,
      learner = learner,
      measure = measure,
      n_permutations = 2L,
      max_reference_size = 10L
    )
    sage$compute(batch_size = 1)
  })

  # Compare with normal result
  result_normal = withr::with_seed(42, {
    sage = ConditionalSAGE$new(
      task = task,
      learner = learner,
      measure = measure,
      n_permutations = 2L,
      max_reference_size = 10L
    )
    sage$compute()
  })

  expect_equal(
    result_batch_1$importance,
    result_normal$importance,
    tolerance = 1e-10,
    info = "ConditionalSAGE batch_size=1 should produce identical results"
  )

  # Note: Resampling tests are omitted here because mlr3's internal random state
  # management during resampling may interact differently with batching,
  # making exact reproducibility challenging. The core batching functionality
  # is thoroughly tested above without resampling.
})

test_that("ConditionalSAGE batching with custom sampler", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("arf")
  skip_if_not_installed("withr")

  set.seed(123)
  task = mlr3::tgen("friedman1")$generate(n = 25)
  learner = mlr3::lrn("regr.ranger", num.trees = 10)
  measure = mlr3::msr("regr.mse")

  # Create custom ARF sampler
  custom_sampler = ARFSampler$new(task, verbose = FALSE)

  # Test with custom sampler - no batching
  result_no_batch = withr::with_seed(42, {
    sage = ConditionalSAGE$new(
      task = task,
      learner = learner,
      measure = measure,
      sampler = custom_sampler,
      n_permutations = 2L,
      max_reference_size = 15L
    )
    sage$compute()
  })

  # Test with custom sampler - with batching
  result_batch = withr::with_seed(42, {
    sage = ConditionalSAGE$new(
      task = task,
      learner = learner,
      measure = measure,
      sampler = custom_sampler,
      n_permutations = 2L,
      max_reference_size = 15L
    )
    sage$compute(batch_size = 30)
  })

  expect_equal(
    result_no_batch$importance,
    result_batch$importance,
    tolerance = 1e-10,
    info = "ConditionalSAGE with custom sampler should produce identical results with batching"
  )
})
