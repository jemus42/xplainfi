library(data.table)

# -------------------------
# Implement SAGE in R
# -------------------------

# DISCLAIMER
# marginal imputation only
# hard coded: minimum coalition = empty set, maximum coalition = all features
# no early stopping in case of convergence of SAGE values phi -- using all instances instead of approx. sampling

#--------------------------
# helper function : Marginal imputer
#--------------------------
marginal_imputer_ref <- function(model, x, S, ins, target = "y") {
  S_rep <- S[rep(seq_len(nrow(S)), each = nrow(x)), ]
  ins_rep <- ins[rep(seq_len(nrow(ins)), each = nrow(x)), ]
  ins_rep_x <- ins_rep[, (target) := NULL]
  x_rep <- do.call("rbind", replicate(nrow(ins), x, simplify = FALSE))
  for (c in 1:ncol(x_rep)) set(x_rep, i = which(S_rep[[c]] == TRUE), j = c, ins_rep[[c]][S_rep[[c]] == TRUE])
  # if regression task - average predictions
  # if classification task - average probs
  pred_raw <- if (is.factor(ins[[target]][1])) {
    model$predict_newdata(newdata = x_rep)$prob[, "1"]
  } else {
    model$predict_newdata(newdata = x_rep)$response
  }
  pred_dt <- data.table("pred" = pred_raw, "ins_id" = rep(1:nrow(ins), each = nrow(x)))
  mean_preds <- setDT(pred_dt)[, .(mean_pred = mean(pred)), by = ins_id]
  return(mean_preds$mean_pred)
}

#--------------------------
# SAGE
#--------------------------
# @param model: fitted mlr3 model
# @param data: data.frame() of test data to calculate SAGE on
# @param loss: instance wise loss function of form function(actual, predicted), e.g. Metrics::se, Metrics::ll

sage_ref <- function(model, data, target = "y", loss = Metrics::se, batch_size = 5) {
  data <- checkmate::assert_data_table(data)
  data_x <- data[, !names(data) %in% target, with = FALSE]
  batch <- round(seq(1, nrow(data), length.out = (batch_size + 1)))
  sage_values <- data.table(matrix(
    data = rep(0, batch_size * ncol(data_x)),
    nrow = batch_size, ncol = ncol(data_x)
  ))
  # sage_values <- foreach(i = 1:batch_size, .export = c("marginal_imputer_ref"), .combine = rbind) %dopar% { # in case we want to use foreach
  for (i in 2:(batch_size + 1)) {
    # select an instance
    ins <- data.table(data[batch[i - 1]:batch[i], ])
    # initialize SAGE values phi
    phi <- data.table(matrix(
      data = rep(0, nrow(ins) * ncol(data_x)),
      nrow = nrow(ins), ncol = ncol(data_x)
    ))
    # sample coalition setup D; subset S of D builds the actual coalition
    perm <- t(apply(matrix(nrow = nrow(ins), ncol = ncol(data_x)), 1, function(x) {
      x <- sample(1:ncol(data_x), ncol(data_x))
    }))
    # calculate initial loss - S = empty set
    S <- data.table(matrix(logical(length(perm)), nrow = nrow(ins)))
    loss_prev <- loss(
      actual = as.numeric(as.character(ins$y)),
      predicted = marginal_imputer_ref(model = model, x = data_x, S = S, ins = ins)
    )

    for (d in 1:ncol(perm)) {
      # add feature d to coalition
      for (s in 1:nrow(S)) set(S, s, perm[, d][s], TRUE)
      # impute values of variables not in S
      y_hat_mean <- marginal_imputer_ref(model = model, x = data_x, S = S, ins = ins)
      loss_S <- loss(actual = as.numeric(as.character(ins$y)), predicted = y_hat_mean)
      delta <- loss_prev - loss_S
      loss_prev <- loss_S
      # save importance values phi
      for (p in 1:nrow(phi)) set(phi, p, perm[, d][p], delta[p])
    }
    means <- colMeans(phi)
    which_i <- as.integer(i - 1)
    for (ss in 1:ncol(sage_values)) set(sage_values, i = as.integer(i - 1), j = ss, means[ss])
  }
  setnames(sage_values, old = colnames(sage_values), new = colnames(data_x))
  return(colMeans(sage_values))
}
