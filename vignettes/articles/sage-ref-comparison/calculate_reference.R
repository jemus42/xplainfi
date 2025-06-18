#!/usr/bin/env Rscript

# Calculate SAGE values using reference implementation
# Save results to JSON for comparison with xplainfi

library(data.table)
library(jsonlite)
library(cli)
library(mlr3)
library(mlr3learners)
lgr::get_logger("mlr3")$set_threshold("warn")

# Load reference implementation
source(here::here("vignettes", "articles", "sage-ref-comparison", "sage_ref_kb.R"))

# Set seed for reproducibility
set.seed(42)

cli_h1("SAGE Comparison: Reference Implementation")

base_dir <- here::here("vignettes", "articles", "sage-ref-comparison")

# Check if train/test data files exist from xplainfi calculation
train_file <- file.path(base_dir, "ewald_train.csv")
test_file <- file.path(base_dir, "ewald_test.csv")

if (!file.exists(train_file) || !file.exists(test_file)) {
  cli_alert_danger("Train/test data files not found. Please run calculate_xplainfi.R first.")
  quit(status = 1)
}

# Load train and test data
cli_progress_step("Loading train/test data from calculate_xplainfi.R")
train_data <- fread(train_file)
test_data <- fread(test_file)
cli_alert_info("Loaded {nrow(train_data)} training and {nrow(test_data)} test observations")

# Recreate task and train model identically to xplainfi script
# Combine data for task creation but train only on training portion
data_sim <- rbind(train_data, test_data)
task <- as_task_regr(data_sim, target = "y")

# Create the same resampling as xplainfi (train rows first, test rows after)
train_indices <- 1:nrow(train_data)
test_indices <- (nrow(train_data) + 1):(nrow(train_data) + nrow(test_data))

learner <- lrn("regr.ranger", num.trees = 100)

# Use same seed as xplainfi script for model training
set.seed(42)
learner$train(task, row_ids = train_indices)

# Evaluate model performance on test set (should match xplainfi)
pred <- learner$predict(task, row_ids = test_indices)
r2_score <- pred$score(msr("regr.rsq"))
cli_alert_info("Model R² score on test set: {.val {round(r2_score, 3)}}")

cli_h2("Computing reference SAGE values")

results <- list(
  data_info = list(
    n_obs = nrow(data_sim),
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    n_features = length(task$feature_names),
    feature_names = task$feature_names
  ),
  model_performance = list(r_squared = r2_score),
  train_indices = train_indices,
  test_indices = test_indices
)

# Test different batch sizes to match xplainfi parameter variations
batch_configs <- list(
  small = list(batch_size = 5, description = "Small batch (fast)"),
  medium = list(batch_size = 10, description = "Medium batch (standard)"),
  large = list(batch_size = 20, description = "Large batch (high precision)")
)

for (config_name in names(batch_configs)) {
  config <- batch_configs[[config_name]]
  
  cli_progress_step("Computing reference SAGE with {config$description}")
  
  # Use same seed as xplainfi for fair comparison
  set.seed(123)
  
  result_ref <- sage_ref(
    model = learner,
    data = test_data,  # Use test data for SAGE calculation
    target = "y",
    loss = Metrics::se,
    batch_size = config$batch_size
  )
  
  # Convert to data.table format
  result_ref_dt <- data.table(
    feature = names(result_ref),
    importance = unname(result_ref)
  )
  
  results[[config_name]] <- list(
    feature = result_ref_dt$feature,
    importance = result_ref_dt$importance,
    parameters = list(
      batch_size = config$batch_size,
      loss_function = "squared_error"
    )
  )
  
  cli_alert_success("{config$description} completed")
}

# Add a reproducibility test - same parameters, different seed
cli_progress_step("Testing reproducibility with different seed")
set.seed(456)  # Different seed
result_diff_seed <- sage_ref(
  model = learner,
  data = test_data,  # Use test data for SAGE calculation
  target = "y", 
  loss = Metrics::se,
  batch_size = 10
)

result_diff_seed_dt <- data.table(
  feature = names(result_diff_seed),
  importance = unname(result_diff_seed)
)

results$different_seed <- list(
  feature = result_diff_seed_dt$feature,
  importance = result_diff_seed_dt$importance,
  parameters = list(
    batch_size = 10,
    seed = 456,
    loss_function = "squared_error"
  )
)
cli_alert_success("Different seed test completed")

# Save results
cli_progress_step("Saving reference implementation results")
results_file <- file.path(base_dir, "reference_results.json")
write_json(results, results_file, pretty = TRUE, auto_unbox = TRUE)

cli_alert_success("Reference SAGE calculations completed!")
cli_alert_info("Results saved to {.file {results_file}}")

# Print summary
cli_h2("Summary of Results")
for (config_name in c("small", "medium", "large", "different_seed")) {
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

# Compare medium batch with different seed for stability
medium_results <- results$medium
diff_seed_results <- results$different_seed

if (!is.null(medium_results) && !is.null(diff_seed_results)) {
  comparison_ref <- data.table(
    feature = medium_results$feature,
    medium_batch = medium_results$importance,
    different_seed = diff_seed_results$importance
  )
  comparison_ref[, difference := abs(medium_batch - different_seed)]
  
  cli_h3("Stability Across Seeds")
  correlation <- cor(comparison_ref$medium_batch, comparison_ref$different_seed)
  cli_alert_info("Correlation between seeds: {round(correlation, 4)}")
  
  max_diff <- max(comparison_ref$difference)
  cli_alert_info("Maximum absolute difference: {round(max_diff, 4)}")
}