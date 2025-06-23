test_that("MarginalSAGE and ConditionalSAGE produce different results on correlated data", {
  skip_if_not_installed("arf")
  skip_if_not_installed("mlr3learners")
  library(mlr3learners)

  set.seed(456)

  # Use correlated features DGP with larger sample
  task = sim_dgp_correlated(n = 1000)
  learner = lrn("regr.lm") # Linear model for clarity
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
  marginal_results = marginal_sage$compute()
  conditional_results = conditional_sage$compute()

  # Both should produce valid importance data.tables
  expect_importance_dt(marginal_results, features = task$feature_names)
  expect_importance_dt(conditional_results, features = task$feature_names)

  # The methods should produce meaningfully different results
  # Calculate correlation between the two sets of importance scores
  merged_results = merge(
    marginal_results[, .(feature, marginal_imp = importance)],
    conditional_results[, .(feature, conditional_imp = importance)],
    by = "feature"
  )

  correlation = cor(merged_results$marginal_imp, merged_results$conditional_imp)

  # With correlated features, methods should show meaningful differences
  # (correlation should be less than perfect)
  expect_lt(correlation, 0.9)

  # Check that correlated features (x1, x2) show different patterns
  x1_marginal = marginal_results[feature == "x1"]$importance
  x1_conditional = conditional_results[feature == "x1"]$importance
  x2_marginal = marginal_results[feature == "x2"]$importance
  x2_conditional = conditional_results[feature == "x2"]$importance

  # At least one of the correlated features should show a meaningful difference
  # between marginal and conditional SAGE
  x1_diff = abs(x1_marginal - x1_conditional)
  x2_diff = abs(x2_marginal - x2_conditional)
  max_diff = max(x1_diff, x2_diff)

  # The maximum difference should be substantial (more than 10% of the larger importance)
  max_importance = max(abs(c(x1_marginal, x1_conditional, x2_marginal, x2_conditional)))
  expect_gt(max_diff, 0.1 * max_importance)
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
  marginal_results = marginal_sage$compute()
  conditional_results = conditional_sage$compute()

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
