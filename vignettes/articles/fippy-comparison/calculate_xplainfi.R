#!/usr/bin/env Rscript

# Calculate feature importance using xplainfi methods
# Save results to JSON for comparison with fippy
# This version is designed for pkgdown articles

library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(jsonlite)
library(cli)
lgr::get_logger("mlr3")$set_threshold("warn")

# Set seed for reproducibility
set.seed(123)

# Generate Ewald simulation data
cli_progress_step("Generating Ewald simulation dataset")
task <- sim_dgp_ewald(n = 5000)
data <- task$data()

# Create holdout resampling for consistent train/test split
resampling <- rsmp("holdout", ratio = 0.7)
resampling$instantiate(task)

# Save separate train and test datasets for consistent comparison
train_data <- data[resampling$train_set(1), ]
test_data <- data[resampling$test_set(1), ]

base_dir <- here::here("vignettes", "articles", "fippy-comparison")

fwrite(train_data, file.path(base_dir, "ewald_train.csv"))
fwrite(test_data, file.path(base_dir, "ewald_test.csv"))

cli_alert_info("Generated {nrow(train_data)} training and {nrow(test_data)} test observations")
cli_alert_info("Features: {paste(task$feature_names, collapse = ', ')}")

# Setup for feature importance methods
cli_progress_step("Setting up feature importance methods")

# Train model
cli_progress_step("Training model")
learner <- lrn("regr.ranger", num.trees = 500, num.threads = 4)
learner$train(task, row_ids = resampling$train_set(1))

# Evaluate model
pred <- learner$predict(task, row_ids = resampling$test_set(1))
perf <- pred$score(msr("regr.rsq"))
cli_alert_info("Model R² score: {.val {round(perf, 3)}}")

# Fit arf for conditional sampling
sampler <- ARFSampler$new(task$clone()$filter(rows = resampling$train_set(1)))

# Initialize results list
results <- list(
  model_performance = list(r_squared = perf),
  train_indices = resampling$train_set(1),
  test_indices = resampling$test_set(1)
)

# 1. PFI
cli_progress_step("Computing PFI")
pfi_r <- PFI$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  iters_perm = 5
)

pfi_results <- pfi_r$compute()
results$PFI <- list(
  feature = pfi_results$feature,
  importance = pfi_results$importance
)

# 2. CFI
cli_progress_step("Computing CFI")
cfi_r <- CFI$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  iters_perm = 5,
  sampler = sampler
)

cfi_results <- cfi_r$compute()
results$CFI <- list(
  feature = cfi_results$feature,
  importance = cfi_results$importance
)
cli_alert_success("CFI completed")


# 3. RFI
cli_progress_step("Computing RFI")
rfi_r <- RFI$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  conditioning_set = c("x1", "x2"),
  iters_perm = 5,
  sampler = sampler
)

rfi_results <- rfi_r$compute()
results$RFI <- list(
  feature = rfi_results$feature,
  importance = rfi_results$importance,
  conditioning_set = c("x1", "x2")
)
cli_alert_success("RFI completed")

# 4. Marginal SAGE
n_perm_sage = 30L
max_ref_size_sage = 200L
batch_size_sage = 5000L

cli_progress_step("Computing Marginal SAGE")
sage_marginal_r <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  n_permutations = n_perm_sage,
  max_reference_size = max_ref_size_sage
)

sage_marginal_results <- sage_marginal_r$compute(batch_size = batch_size_sage)
results$SAGE_Marginal <- list(
  feature = sage_marginal_results$feature,
  importance = sage_marginal_results$importance
)

# 5. Conditional SAGE
cli_progress_step("Computing Conditional SAGE")
sage_conditional_r <- ConditionalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  sampler = sampler,
  n_permutations = n_perm_sage,
  max_reference_size = max_ref_size_sage
)

sage_conditional_results <- sage_conditional_r$compute(batch_size = batch_size_sage)
results$SAGE_Conditional <- list(
  feature = sage_conditional_results$feature,
  importance = sage_conditional_results$importance
)
cli_alert_success("Conditional SAGE completed")

# Save results
cli_progress_step("Saving results to {.file {file.path(base_dir, 'xplainfi_results.json')}}")
write_json(results, file.path(base_dir, "xplainfi_results.json"), pretty = TRUE, auto_unbox = TRUE)

cli_alert_success("xplainfi calculations completed successfully!")
