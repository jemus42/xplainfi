test_that("MarginalSAGE and ConditionalSAGE produce different results on correlated data", {
  skip_if_not_installed("arf")
  skip_if_not_installed("mlr3learners")
  library(mlr3learners)

  set.seed(42)

  # Use the sim_dgp_correlated which is designed to show differences
  task = sim_dgp_correlated(n = 800)
  learner = lrn("regr.ranger", num.trees = 100)
  measure = msr("regr.mse")

  # Create both SAGE methods
  marginal_sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 20L,
    max_reference_size = 100L
  )

  conditional_sage = ConditionalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 20L,
    max_reference_size = 100L
  )

  # Compute results with consistent seeds
  set.seed(123)
  marginal_sage$compute()
  marginal_results = marginal_sage$importance()
  
  set.seed(123)
  conditional_sage$compute()
  conditional_results = conditional_sage$importance()

  # Both should produce valid importance data.tables
  expect_importance_dt(marginal_results, features = task$feature_names)
  expect_importance_dt(conditional_results, features = task$feature_names)

  # Check that the results are meaningfully different
  # For highly correlated x1 and x2, the methods should differ
  x1_marginal = marginal_results[feature == "x1"]$importance
  x2_marginal = marginal_results[feature == "x2"]$importance
  x1_conditional = conditional_results[feature == "x1"]$importance
  x2_conditional = conditional_results[feature == "x2"]$importance
  
  # Calculate the absolute differences
  x1_diff = abs(x1_marginal - x1_conditional)
  x2_diff = abs(x2_marginal - x2_conditional)
  
  # At least one correlated feature should show substantial difference
  max_diff = max(x1_diff, x2_diff)
  max_importance = max(abs(c(x1_marginal, x1_conditional, x2_marginal, x2_conditional)))
  
  # The difference should be at least 15% of the maximum importance
  expect_gt(max_diff / (max_importance + 1e-6), 0.15)
})

test_that("MarginalSAGE and ConditionalSAGE are similar on independent data", {
  skip_if_not_installed("arf")
  skip_if_not_installed("mlr3learners")
  library(mlr3learners)

  set.seed(789)

  # Use independent features DGP where methods should be more similar
  task = sim_dgp_independent(n = 1000)
  learner = lrn("regr.lm") # Linear model for consistency
  measure = msr("regr.mse")

  # Create both SAGE methods
  marginal_sage = MarginalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 20L
  )

  conditional_sage = ConditionalSAGE$new(
    task = task,
    learner = learner,
    measure = measure,
    n_permutations = 20L
  )

  # Compute results
  marginal_sage$compute()
  marginal_results = marginal_sage$importance()
  conditional_sage$compute()
  conditional_results = conditional_sage$importance()

  # Both should produce valid importance data.tables
  expect_importance_dt(marginal_results, features = task$feature_names)
  expect_importance_dt(conditional_results, features = task$feature_names)

  # With independent features, the methods should be more correlated
  merged_results = merge(
    marginal_results[, .(feature, marginal_imp = importance)],
    conditional_results[, .(feature, conditional_imp = importance)],
    by = "feature"
  )

  correlation = cor(merged_results$marginal_imp, merged_results$conditional_imp)

  # Should have high correlation for independent features (though not perfect due to randomness)
  expect_gt(correlation, 0.8)
})
