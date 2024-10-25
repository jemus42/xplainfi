test_that("can't be constructed without args", {
  expect_error(PFI$new())
})

test_that("can be constructed with simple objects", {
  skip_if_not_installed("rpart")

  task = mlr3::tsk("zoo")

  pfi = PFI$new(
    task = task,
    learner = mlr3::lrn("classif.rpart"),
    measure = mlr3::msr("classif.ce")
  )

  checkmate::expect_r6(pfi, c("FeatureImportanceLearner", "PFI"))

  expect_importance_dt(pfi$compute(), features = pfi$features)
  expect_importance_dt(pfi$compute(relation = "difference"), features = pfi$features)
})

test_that("null result for featureless learner", {

  task = mlr3::tsk("zoo")

  pfi = PFI$new(
    task = task,
    learner = mlr3::lrn("classif.featureless"),
    measure = mlr3::msr("classif.ce")
  )

  pfi$compute()

  expected = data.table::data.table(
    feature = pfi$features,
    importance = 0,
    key = "feature"
  )

  expect_identical(pfi$importance, expected)
})

test_that("multiple perms", {

  task = mlr3::tsk("zoo")

  pfi = PFI$new(
    task = task,
    learner = mlr3::lrn("classif.rpart"),
    measure = mlr3::msr("classif.ce"),
    resampling = mlr3::rsmp("cv", folds = 3),
    iters_perm = 2
  )

  pfi$compute()

  expect_importance_dt(pfi$importance, features = pfi$features)

  checkmate::expect_data_table(
    pfi$scores, types = c("character", "integer", "numeric"),
    nrows = pfi$resampling$iters *
      pfi$param_set$values$iters_perm *
      length(pfi$features),
    ncols = 6,
    any.missing = FALSE, min.cols = 6
  )
})

test_that("only one feature", {

  task = mlr3::tsk("zoo")

  pfi = PFI$new(
    task = task,
    learner = mlr3::lrn("classif.rpart"),
    measure = mlr3::msr("classif.ce"),
    resampling = mlr3::rsmp("cv", folds = 3),
    iters_perm = 2,
    features = "legs"
  )

  pfi$compute()

  expect_importance_dt(pfi$importance, features = "legs")

  checkmate::expect_data_table(
    pfi$scores, types = c("character", "integer", "numeric"),
    nrows = pfi$resampling$iters *
      pfi$param_set$values$iters_perm,
    ncols = 6,
    any.missing = FALSE, min.cols = 6
  )
})



test_that("snapshot results", {

  skip_if_not_installed("ranger")
  skip_if_not_installed("mlr3learners")

  set.seed(123)

  task = mlr3::tsk("german_credit")
  learner = mlr3::lrn("classif.ranger", num.trees = 500)
  measure = mlr3::msr("classif.ce")

  pfi = PFI$new(
    task = task,
    learner = learner,
    measure = measure
  )

  # Default behavior should be sane
  res_1 = pfi$compute()
  # Expect named, non-missing/finite numeric vector corresponding to feature names
  expect_importance_dt(res_1, pfi$features)

  res_2 = pfi$compute()

  expect_identical(res_1, res_2)

  res_3 = pfi$compute("difference")
  expect_identical(res_1, res_3)

  res_4 = pfi$compute("ratio")
  res_5 = pfi$compute("difference")

  expect_error(expect_equal(res_4, res_5))

  expect_importance_dt(res_2, pfi$features)
  expect_importance_dt(res_3, pfi$features)
  expect_importance_dt(res_4, pfi$features)
  expect_importance_dt(res_5, pfi$features)

  # With resampling
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")

  pfi = PFI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure
  )

  res_1 = pfi$compute()
  expect_importance_dt(pfi$importance, pfi$features)

  res_2 = pfi$compute("ratio")
  expect_importance_dt(pfi$importance, pfi$features)

  expect_error(expect_equal(res_1, res_2))

})
