library(mlr3)
library(mlr3learners)
library(data.table)

#' Marginal Imputer
#'
#' @source Based on code from Blesch et al. 2023, https://github.com/bips-hb/CFI_mixedData/blob/main/3.2_sim_FI_comparison/SAGE_R_efficient.R
#' @note
#' Marginal imputation only.
#' Minimum coalition is empty set, maximum coalition is the set of all features
#' No early stopping in case of convergence of SAGE values phi -- using all instances instead of approx. sampling
#' @param learner `(mlr3::Learner)` A trained `Learner` to use for prediction.
#' @param x `(data.table)` A binary matrix indicating the feature setup.
#' @param S `(data.table)` A binary matrix indicating the coalition setup.
#' @param target `(character(1))` Name of target column.
#'
marginal_imputer <- function(learner, x, S, ins, target) {
  S_rep <- S[rep(seq_len(nrow(S)), each = nrow(x)), ]
  ins_rep <- ins[rep(seq_len(nrow(ins)), each = nrow(x)), ]
  ins_rep_x <- ins_rep[, (target) := NULL]
  x_rep <- do.call("rbind", replicate(nrow(ins), x, simplify = FALSE))

  for (x_col_index in 1:ncol(x_rep)) {
    set(
      x = x_rep,
      i = which(S_rep[[x_col_index]] == TRUE),
      j = x_col_index,
      value = ins_rep[[x_col_index]][S_rep[[x_col_index]] == TRUE]
    )
  }
  # if regression task - average predictions
  # if classification task - average probs
   if (is.factor(ins[[target]][1])) {
     pred_raw = learner$predict_newdata(newdata = x_rep)$prob[, "1"]
  } else {
    pred_raw = learner$predict_newdata(newdata = x_rep)$response
  }
  pred_dt <- data.table("pred" = pred_raw, "ins_id" = rep(1:nrow(ins), each = nrow(x)))
  mean_preds <- pred_dt[, .(mean_pred = mean(pred)), by = ins_id]
  mean_preds$mean_pred

}

#' (Marginal) SAGE
#'
#'
#' @param learner `(mlr3::Learner)` trained learner to calculate SAGE on
#' @param data `(data.table`) test data to calculate SAGE on
#' @param loss `(function(truth, response))`  instance wise loss function of form function(truth, response),
#'   defaults to `mlr3measures::se` for regression and `Metrics::ll` for binary classification
#' @param batch_count `(integer(1): 5)` Batch size for parallel execution (NYI)
#' @return Numeric vector of SAGE values with named according to the features
#' @source Based on code from Blesch et al. 2023, https://github.com/bips-hb/CFI_mixedData/blob/main/3.2_sim_FI_comparison/SAGE_R_efficient.R
# TODO: Save value function (perf_empty_model - perf_current_coalition)
sage <- function(learner, data, target = "y", loss = mlr3measures::se, batch_count = 5) {
  mlr3::assert_predictable(task, learner)
  data <- checkmate::assert_data_table(data)
  data_x <- data[, names(data) != target, with = FALSE]
  batch_start_index <- round(seq(1, nrow(data), length.out = (batch_count + 1)))

  # Placeholder table to hold results
  sage_values <- data.table(matrix(data = 0, nrow = batch_count, ncol = ncol(data_x)))
  setnames(sage_values, new = colnames(data_x))

  # sage_values <- foreach(i = 1:batch_count, .export = c("marginal_imputer"), .combine = rbind) %dopar% { # in case we want to use foreach
  #  cli::cli_inform("Iterating over {batch_count} batches")
  for (i in 2:(batch_count + 1)) {
    # select an instance
    # cli::cli_inform("Getting instance with index {batch_start_index[i - 1]} to {batch_start_index[i]}")
    current_batch_dt <- data[batch_start_index[i - 1]:batch_start_index[i], ]

    # initialize SAGE values phi
    phi <- data.table(matrix(
      data = 0,
      nrow = nrow(current_batch_dt),
      ncol = ncol(data_x),
      dimnames = list(NULL, colnames(data_x))
    ))

    # sample coalition setup D; subset S of D builds the actual coalition
    # perm: nrow(current_batch_dt) x ncol(data_x) matrix of random integers
    perm <- t(replicate(
      n = nrow(current_batch_dt),
      expr = sample.int(ncol(data_x)),
      simplify = TRUE))
    # colnames(perm) <- colnames(data_x)

    # calculate initial loss - S = empty set
    S <- data.table(matrix(
      data = FALSE,
      nrow = nrow(current_batch_dt),
      ncol = ncol(data_x),
      dimnames = list(NULL, colnames(data_x))
    ))

    loss_prev <- loss(
      current_batch_dt[[target]],
      marginal_imputer(
        learner = learner,
        x = data_x,
        S = S,
        ins = current_batch_dt,
        target = target
      )
    )

    for (col_index in 1:ncol(perm)) {
      # add feature d to coalition
      for (row_index in 1:nrow(S)) {
        set(S, row_index, perm[row_index, col_index], TRUE)
      }
      # impute values of variables not in S
      y_hat_mean <- marginal_imputer(
        learner = learner,
        x = data_x,
        S = S,
        ins = current_batch_dt,
        target = target
      )

      loss_S <- loss(current_batch_dt[[target]], y_hat_mean)
      delta <- loss_prev - loss_S
      loss_prev <- loss_S
      # save importance values phi
      for (p in 1:nrow(phi)) {
        set(x = phi, i = p, j = perm[p, col_index], value = delta[p])
      }
    }
    means <- colMeans(phi)
    which_i <- as.integer(i - 1)

    for (ss in 1:ncol(sage_values)) {
      set(sage_values, i = as.integer(i - 1), j = ss, means[ss])
    }
  }

  colMeans(sage_values)
}


set.seed(10)
task = tgen("friedman1")$generate(n = 200)
splits = partition(task)
learner = lrn("regr.ranger")
learner$train(task, row_ids = splits$train)

tictoc::tic("New version")
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

# profvis::profvis(sage(model = learner, data = task$data(), target = task$target_names, loss = mlr3measures::se))

source(here::here("attic", "sage_ref_kb.R"))
tictoc::tic("Reference implementation")
set.seed(1)
res_ref <- sage_ref(
  model = learner,
  data = task$data(rows = splits$test),
  target = "y",
  loss = Metrics::se, # use se or ll
  batch_size = 5
)
res_ref
tictoc::toc()

testthat::expect_equal(res, res_ref)

# profvis::profvis(sage_ref(model = learner, data = task$data(rows = splits$test), target = "y", loss = Metrics::se))

# Binary classif
# learner <- lrn("classif.ranger", predict_type = "prob")
# task <- tsk("breast_cancer")
# # learner = lrn("classif.ranger", predict_type = "prob")
# # task = as_task_classif(x = train, target = "y")
# learner$train(task)
# learner$predict(task)
#
# tictoc::tic()
# res <- sage(model = learner, data = task$data(), target = task$target_names, loss = Metrics::ll)
# res
# tictoc::toc()
