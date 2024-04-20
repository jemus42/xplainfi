test_that("can be constructed", {

  expect_error(PFI$new())
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

  expect_snapshot(pfi$importance)
  res_2 = pfi$compute()

  expect_identical(res_1, res_2)

  res_3 = pfi$compute("difference")
  expect_identical(res_1, res_3)

  res_4 = pfi$compute("ratio")
  expect_snapshot(pfi$importance)
  res_5 = pfi$compute("difference")

  expect_error(expect_equal(res_4, res_5))

  # With resampling

  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")


  pfi = PFI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure
  )

  pfi$compute()
  expect_snapshot(pfi$importance)

  pfi$compute("ratio")
  expect_snapshot(pfi$importance)

})
