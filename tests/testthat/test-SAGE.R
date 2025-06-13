test_that("MarginalSAGE can't be constructed without args", {
  expect_error(MarginalSAGE$new())
})

test_that("ConditionalSAGE can't be constructed without args", {
  expect_error(ConditionalSAGE$new())
})

test_that("MarginalSAGE can be constructed with simple objects", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce")
  )

  checkmate::expect_r6(sage, c("FeatureImportanceMeasure", "SAGE", "MarginalSAGE"))
  expect_importance_dt(sage$compute(), features = sage$features)
})

test_that("ConditionalSAGE can be constructed with simple objects", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("arf")

  task = mlr3::tsk("pima")

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L # Keep small for fast testing
  )

  checkmate::expect_r6(sage, c("FeatureImportanceMeasure", "SAGE", "ConditionalSAGE"))
  expect_importance_dt(sage$compute(), features = sage$features)
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
    max_background_size = 50L
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
    max_background_size = 50L
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

test_that("MarginalSAGE with multiple permutations", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    resampling = mlr3::rsmp("cv", folds = 3),
    n_permutations = 2L
  )

  sage$compute()
  expect_importance_dt(sage$importance, features = sage$features)

  # Check scores data.table structure
  checkmate::expect_data_table(
    sage$scores,
    types = c("integer", "character", "numeric"),
    nrows = sage$resampling$iters * length(sage$features),
    ncols = 3,
    any.missing = FALSE,
    min.cols = 3
  )
})

test_that("MarginalSAGE only one feature", {
  task = mlr3::tsk("pima")

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    features = "glucose",
    n_permutations = 2L
  )

  sage$compute()
  expect_importance_dt(sage$importance, features = "glucose")

  # Should only have one feature
  expect_equal(nrow(sage$importance), 1L)
  expect_equal(sage$importance$feature, "glucose")
})

test_that("SAGE with custom background data", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")
  background_data = task$data(cols = task$feature_names)[1:20, ]

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    background_data = background_data,
    n_permutations = 2L
  )

  sage$compute()
  expect_importance_dt(sage$importance, features = sage$features)

  # Check that background data was used
  expect_equal(nrow(sage$background_data), 20L)
})

test_that("SAGE with max_background_size parameter", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")

  sage = MarginalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    max_background_size = 30L,
    n_permutations = 2L
  )

  sage$compute()
  expect_importance_dt(sage$importance, features = sage$features)

  # Background should be subsampled to max_background_size
  expect_lte(nrow(sage$background_data), 30L)
})

test_that("SAGE reproducibility with same seed", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")
  learner = mlr3::lrn("classif.rpart", predict_type = "prob")
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

test_that("ConditionalSAGE uses ARFSampler by default", {
  skip_if_not_installed("rpart")
  skip_if_not_installed("arf")

  task = mlr3::tsk("pima")

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    n_permutations = 2L
  )

  # Should have ARFSampler
  checkmate::expect_r6(sage$sampler, "ARFSampler")
  expect_equal(sage$label, "Conditional SAGE")
})

test_that("ConditionalSAGE with custom sampler", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("pima")
  custom_sampler = MarginalSampler$new(task)

  sage = ConditionalSAGE$new(
    task = task,
    learner = mlr3::lrn("classif.rpart", predict_type = "prob"),
    measure = mlr3::msr("classif.ce"),
    sampler = custom_sampler,
    n_permutations = 2L
  )

  # Should use the custom sampler
  checkmate::expect_r6(sage$sampler, "MarginalSampler")
  sage$compute()
  expect_importance_dt(sage$importance, features = sage$features)
})

test_that("SAGE parameter validation", {
  task = mlr3::tsk("pima")
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
