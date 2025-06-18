#!/usr/bin/env Rscript

# Calculate SAGE values using xplainfi MarginalSAGE
# Save results to JSON for comparison with reference implementation

library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(jsonlite)
library(cli)
lgr::get_logger("mlr3")$set_threshold("warn")

# Set seed for reproducibility
set.seed(42)

cli_h1("SAGE Comparison: xplainfi Implementation")

# Generate Ewald simulation data
cli_progress_step("Generating Ewald simulation dataset")
n_obs <- 1000
task <- sim_dgp_ewald(n = n_obs)
data_sim <- task$data()

# Create holdout resampling for consistent train/test split
resampling <- rsmp("holdout", ratio = 0.7)
resampling$instantiate(task)

# Save separate train and test datasets for consistent comparison
train_data <- data_sim[resampling$train_set(1), ]
test_data <- data_sim[resampling$test_set(1), ]

base_dir <- here::here("vignettes", "articles", "sage-ref-comparison")

fwrite(train_data, file.path(base_dir, "ewald_train.csv"))
fwrite(test_data, file.path(base_dir, "ewald_test.csv"))

cli_alert_info("Generated {nrow(train_data)} training and {nrow(test_data)} test observations")
cli_alert_info("Features: {paste(task$feature_names, collapse = ', ')}")

# Create and train learner on training data only
cli_progress_step("Training model on training set")
learner <- lrn("regr.ranger", num.trees = 100)
learner$train(task, row_ids = resampling$train_set(1))

# Evaluate model performance on test set
pred <- learner$predict(task, row_ids = resampling$test_set(1))
r2_score <- pred$score(msr("regr.rsq"))
cli_alert_info("Model R² score on test set: {.val {round(r2_score, 3)}}")

# Test different parameter combinations
cli_h2("Testing different parameter combinations")

results <- list(
  data_info = list(
    n_obs = n_obs,
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    n_features = length(task$feature_names),
    feature_names = task$feature_names
  ),
  model_performance = list(r_squared = r2_score),
  train_indices = resampling$train_set(1),
  test_indices = resampling$test_set(1)
)

# 1. Standard parameters
cli_progress_step("Computing SAGE with standard parameters")
sage_standard <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  n_permutations = 20L,
  max_reference_size = 200L
)

set.seed(123)
result_standard <- sage_standard$compute()
results$standard <- list(
  feature = result_standard$feature,
  importance = result_standard$importance,
  parameters = list(
    n_permutations = 20L,
    max_reference_size = 200L
  )
)
cli_alert_success("Standard parameters completed")

# 2. High precision (more permutations)
cli_progress_step("Computing SAGE with high precision")
sage_high_precision <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  n_permutations = 50L,
  max_reference_size = 300L
)

set.seed(123)
result_high_precision <- sage_high_precision$compute()
results$high_precision <- list(
  feature = result_high_precision$feature,
  importance = result_high_precision$importance,
  parameters = list(
    n_permutations = 50L,
    max_reference_size = 300L
  )
)
cli_alert_success("High precision completed")

# 3. With cross-validation for robustness
cli_progress_step("Computing SAGE with cross-validation")
cv_resampling <- rsmp("cv", folds = 3)
sage_cv <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = cv_resampling,
  n_permutations = 15L,
  max_reference_size = 150L
)

set.seed(123)
result_cv <- sage_cv$compute()
results$cross_validation <- list(
  feature = result_cv$feature,
  importance = result_cv$importance,
  parameters = list(
    n_permutations = 15L,
    max_reference_size = 150L,
    resampling = "3-fold CV"
  )
)
cli_alert_success("Cross-validation completed")

# 4. Test batching
cli_progress_step("Computing SAGE with batching")
sage_batched <- MarginalSAGE$new(
  task = task,
  learner = learner,
  measure = msr("regr.mse"),
  resampling = resampling,
  n_permutations = 20L,
  max_reference_size = 200L
)

set.seed(123)
result_batched <- sage_batched$compute(batch_size = 1000)
results$batched <- list(
  feature = result_batched$feature,
  importance = result_batched$importance,
  parameters = list(
    n_permutations = 20L,
    max_reference_size = 200L,
    batch_size = 1000L
  )
)
cli_alert_success("Batching completed")

# Save results
cli_progress_step("Saving xplainfi results")
results_file <- file.path(base_dir, "xplainfi_results.json")
write_json(results, results_file, pretty = TRUE, auto_unbox = TRUE)

cli_alert_success("xplainfi SAGE calculations completed!")
cli_alert_info("Results saved to {.file {results_file}}")

# Print summary
cli_h2("Summary of Results")
for (config_name in c("standard", "high_precision", "cross_validation", "batched")) {
  config_results <- results[[config_name]]
  if (!is.null(config_results)) {
    cli_h3(stringr::str_to_title(gsub("_", " ", config_name)))

    # Sort by importance
    importance_dt <- data.table(
      feature = config_results$feature,
      importance = config_results$importance
    )
    setorder(importance_dt, -importance)

    for (i in 1:nrow(importance_dt)) {
      cli_alert(glue::glue("  {importance_dt$feature[i]}: {round(importance_dt$importance[i], 4)}"))
    }
  }
}
