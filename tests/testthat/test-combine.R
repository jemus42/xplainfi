library(mlr3)
library(mlr3learners)

test_that("combine two PFIs with same setup", {
  learner = lrn("classif.rpart")
  resampling = rsmp("cv", folds = 3)
  measure = msr("classif.ce")
  task = tsk("breast_cancer")

  pfis = lapply(c(1, 5), \(iters) {
    x = PFI$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = msr("classif.ce"),
      iters_perm = iters
    )
    x$compute()
    x
  })

  res = c(pfis[[1]], pfis[[2]])

  expect_importance_dt(res$importance, task$feature_names)
  expect_score_dt(res$scores, task$feature_names)
})
