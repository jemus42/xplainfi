test_that("as.data.table(PFI)", {

  skip_if_not_installed("mlr3learners")

  pfi = PFI$new(
    task = mlr3::tsk("zoo"),
    measure = mlr3::msr("classif.ce"),
    learner = mlr3::lrn("classif.ranger")
  )

  expect_warning({ tmp <- as.data.table(pfi) })
  checkmate::expect_data_table(tmp, nrows = 0)
  checkmate::expect_subset(names(tmp), choices = c("feature", "importance"))
})
