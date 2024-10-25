library(mlr3)
library(mlr3learners)
library(data.table)


set.seed(10)
task = tgen("friedman1")$generate(n = 200)
splits = partition(task)
learner = lrn("regr.ranger")
learner$train(task, row_ids = splits$train)


set.seed(1)
res <- sage(
  learner = learner,
  data = task$data(rows = splits$test),
  target = task$target_names,
  loss = mlr3measures::se,
  batch_count = 5
)
res
tictoc::toc()

