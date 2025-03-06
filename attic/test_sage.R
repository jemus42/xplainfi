library(mlr3verse)
library(data.table)


set.seed(10)
task = tgen("friedman1")$generate(n = 200)
splits = partition(task)
learner = lrn("regr.ranger")
learner$train(task, row_ids = splits$train)


if (FALSE) {
  average_prediction = marginal_imputer(
    learner = learner,
    x = task$data(rows = splits$test, cols = task$feature_names),
    S = S,
    ins = current_batch_dt,
    target = task$target_names
  )
}


set.seed(1)
tictoc::tic("New implementation")
res = sage(
  learner = learner,
  task = task,
  test_ids = splits$test,
  # target = task$target_names,
  loss = mlr3measures::se,
  batch_count = 5
)
res
tictoc::toc()

source(here::here("attic", "sage_ref_kb.R"))
tictoc::tic("Reference implementation")
set.seed(1)
res_ref = sage_ref(
  model = learner,
  data = task$data(rows = splits$test),
  target = "y",
  loss = Metrics::se, # use se or ll
  batch_size = 5
)
res_ref
tictoc::toc()

testthat::expect_equal(res, res_ref)


# xplain version ---------------------------------------------------------
library(xplainfi)
library(mlr3verse)

sage = SAGE$new(
  task = tsk("zoo"),
  learner = lrn("classif.rpart"),
  measure = msr("classif.ce")
)

sage$compute()
sage$importance
