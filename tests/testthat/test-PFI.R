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

  expect_importance_vec(pfi$compute(), features = task$feature_names)
  expect_importance_vec(pfi$compute(relation = "difference"), features = task$feature_names)
})

test_that("null result for featureless learner", {

  task = mlr3::tsk("zoo")

  pfi = PFI$new(
    task = task,
    learner = mlr3::lrn("classif.featureless"),
    measure = mlr3::msr("classif.ce")
  )

  expected = rep(0, length(task$feature_names))
  names(expected) = task$feature_names

  expect_identical(pfi$compute(), expected)
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
  expect_importance_vec(res_1, task$feature_names)

  expect_snapshot(pfi$importance, variant = Sys.info()[["sysname"]])
  res_2 = pfi$compute()

  expect_identical(res_1, res_2)

  res_3 = pfi$compute("difference")
  expect_identical(res_1, res_3)

  res_4 = pfi$compute("ratio")
  expect_snapshot(pfi$importance, variant = Sys.info()[["sysname"]])
  res_5 = pfi$compute("difference")

  expect_error(expect_equal(res_4, res_5))

  expect_importance_vec(res_2, task$feature_names)
  expect_importance_vec(res_3, task$feature_names)
  expect_importance_vec(res_4, task$feature_names)
  expect_importance_vec(res_5, task$feature_names)

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
  expect_importance_vec(pfi$importance, task$feature_names)

  expect_snapshot(pfi$importance, variant = Sys.info()[["sysname"]])

  res_2 = pfi$compute("ratio")
  expect_importance_vec(pfi$importance, task$feature_names)
  expect_snapshot(pfi$importance, variant = Sys.info()[["sysname"]])

  expect_error(expect_equal(res_1, res_2))

})
