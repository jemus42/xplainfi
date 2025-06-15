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

# Set seed for reproducibility
set.seed(123)

# Generate Friedman1 data
cli_progress_step("Generating Friedman1 dataset")
task <- tgen("friedman1")$generate(n = 1000)
data <- task$data()

# Create holdout resampling for consistent train/test split
# Reset seed to ensure consistent train/test split
set.seed(123)
resampling <- rsmp("holdout", ratio = 0.7)
resampling$instantiate(task)

# Save separate train and test datasets for consistent comparison
train_data <- data[resampling$train_set(1), ]
test_data <- data[resampling$test_set(1), ]

base_dir <- here::here("vignettes", "articles", "fippy-comparison")

fwrite(train_data, file.path(base_dir, "friedman1_train.csv"))
fwrite(test_data, file.path(base_dir, "friedman1_test.csv"))

cli_alert_info("Generated {nrow(train_data)} training and {nrow(test_data)} test observations")

# Setup for feature importance methods
cli_progress_step("Setting up feature importance methods")

# Train model
cli_progress_step("Training model")
set.seed(123)
learner <- lrn("regr.ranger", num.trees = 100, seed = 123)
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

# 2. CFI (if arf available)
cli_progress_step("Computing CFI")
if (requireNamespace("arf", quietly = TRUE)) {
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
} else {
  cli_alert_warning("arf package not available - skipping CFI")
  results$CFI <- NULL
}

# 3. RFI (if arf available)
cli_progress_step("Computing RFI")
rfi_r <- RFI$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  conditioning_set = c("important1", "important2"),
  iters_perm = 5,
  sampler = sampler
)

rfi_results <- rfi_r$compute()
results$RFI <- list(
  feature = rfi_results$feature,
  importance = rfi_results$importance,
  conditioning_set = c("important1", "important2")
)
cli_alert_success("RFI completed")


# 4. Marginal SAGE
cli_progress_step("Computing Marginal SAGE")
sage_marginal_r <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  n_permutations = 50L
)

sage_marginal_results <- sage_marginal_r$compute()
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
  n_permutations = 50L,
  sampler = sampler
)

sage_conditional_results <- sage_conditional_r$compute()
results$SAGE_Conditional <- list(
  feature = sage_conditional_results$feature,
  importance = sage_conditional_results$importance
)
cli_alert_success("Conditional SAGE completed")

# Save results
cli_progress_step("Saving results to {.file {file.path(base_dir, 'xplainfi_results.json')}}")
write_json(results, file.path(base_dir, "xplainfi_results.json"), pretty = TRUE, auto_unbox = TRUE)

cli_alert_success("xplainfi calculations completed successfully!")
