test_that("sim_dgp_independent generates correct structure", {
  task <- sim_dgp_independent(n = 100)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 100)
  expect_equal(task$target_names, "y")
  expect_setequal(
    task$feature_names,
    c("important1", "important2", "important3", "unimportant1", "unimportant2")
  )
  expect_true(grepl("^independent_", task$id))

  # Check data types
  expect_true(all(sapply(data, is.numeric)))

  # Check approximate independence (correlations should be low)
  cor_matrix <- cor(data[, .(important1, important2, important3, unimportant1, unimportant2)])
  off_diagonal <- cor_matrix[upper.tri(cor_matrix)]
  expect_true(mean(abs(off_diagonal)) < 0.2) # Low correlations

  # Check that y has reasonable correlation with predictors
  cor_y <- cor(data$y, data[, .(important1, important2, important3)])
  expect_true(all(abs(cor_y) > 0.2)) # Should have some correlation
  expect_true(abs(cor(data$y, data$unimportant1)) < 0.2) # Noise should have low correlation
  expect_true(abs(cor(data$y, data$unimportant2)) < 0.2) # Noise should have low correlation
})

test_that("sim_dgp_correlated generates correlated features", {
  task <- sim_dgp_correlated(n = 200)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 200)
  expect_setequal(task$feature_names, c("x1", "x2", "x3", "x4"))
  expect_true(grepl("^correlated_", task$id))

  # Check high correlation between x1 and x2
  cor_x1_x2 <- cor(data$x1, data$x2)
  expect_true(cor_x1_x2 > 0.9) # Should be highly correlated

  # Check that x3 and x4 are less correlated with x1, x2
  expect_true(abs(cor(data$x1, data$x3)) < 0.3)
  expect_true(abs(cor(data$x1, data$x4)) < 0.3)

  # Check that all features contribute to y
  lm_fit <- lm(y ~ x1 + x2 + x3 + x4, data = data)
  expect_true(summary(lm_fit)$r.squared > 0.7) # Should explain most variance
})

test_that("sim_dgp_mediated generates mediation structure", {
  task <- sim_dgp_mediated(n = 150)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 150)
  expect_setequal(task$feature_names, c("exposure", "mediator", "direct", "noise"))
  expect_true(grepl("^mediated_", task$id))

  # Check mediation relationships
  # Exposure should correlate with mediator
  expect_true(cor(data$exposure, data$mediator) > 0.3)

  # Direct should correlate with both mediator and y
  expect_true(cor(data$direct, data$mediator) > 0.3)
  expect_true(cor(data$direct, data$y) > 0.3)

  # Mediator should strongly correlate with y
  expect_true(cor(data$mediator, data$y) > 0.5)

  # Noise should have low correlation with everything
  expect_true(abs(cor(data$noise, data$y)) < 0.2)
})

test_that("sim_dgp_confounded with hidden=TRUE generates correct structure", {
  task <- sim_dgp_confounded(n = 200, hidden = TRUE)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 200)
  expect_setequal(task$feature_names, c("x1", "x2", "proxy", "independent"))
  expect_true(grepl("^confounded_hidden_", task$id))
  expect_false("confounder" %in% task$feature_names) # Should be hidden

  # Check confounding structure through correlations
  # x1, x2, and proxy should be correlated (due to hidden confounder)
  expect_true(cor(data$x1, data$x2) > 0.4)
  expect_true(cor(data$x1, data$proxy) > 0.4)
  expect_true(cor(data$x2, data$proxy) > 0.4)

  # Independent should have low correlation with confounded variables
  expect_true(abs(cor(data$independent, data$x1)) < 0.3)
  expect_true(abs(cor(data$independent, data$x2)) < 0.3)
  expect_true(abs(cor(data$independent, data$proxy)) < 0.3)

  # All should correlate with y
  cor_y <- cor(data$y, data[, .(x1, x2, proxy, independent)])
  expect_true(all(abs(cor_y) > 0.2))
})

test_that("sim_dgp_confounded with hidden=FALSE includes confounder", {
  task <- sim_dgp_confounded(n = 100, hidden = FALSE)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 100)
  expect_setequal(task$feature_names, c("x1", "x2", "confounder", "proxy", "independent"))
  expect_true(grepl("^confounded_observed_", task$id))
  expect_true("confounder" %in% task$feature_names) # Should be observable

  # Check that confounder is the source of correlations
  # Confounder should correlate highly with x1, x2, proxy
  expect_true(cor(data$confounder, data$x1) > 0.6)
  expect_true(cor(data$confounder, data$x2) > 0.6)
  expect_true(cor(data$confounder, data$proxy) > 0.6)
  expect_true(cor(data$confounder, data$y) > 0.6)

  # Independent should still be independent of confounder
  expect_true(abs(cor(data$independent, data$confounder)) < 0.3)
})

test_that("sim_dgp_interactions generates pure interaction effects", {
  task <- sim_dgp_interactions(n = 300)
  data <- task$data()

  # Basic structure tests
  expect_s3_class(task, "TaskRegr")
  expect_equal(nrow(data), 300)
  expect_setequal(task$feature_names, c("x1", "x2", "x3", "noise1", "noise2"))
  expect_true(grepl("^interactions_", task$id))

  # Check that x1 and x2 have low marginal correlations with y
  expect_true(abs(cor(data$x1, data$y)) < 0.2) # Should have low marginal correlation
  expect_true(abs(cor(data$x2, data$y)) < 0.2) # Should have low marginal correlation

  # But x3 should have strong correlation (main effect)
  expect_true(abs(cor(data$x3, data$y)) > 0.3)

  # Check interaction effect by fitting model
  lm_fit <- lm(y ~ x1 + x2 + x1:x2 + x3 + noise1 + noise2, data = data)
  coefs <- coef(lm_fit)

  # Main effects of x1, x2 should be near zero
  expect_true(abs(coefs["x1"]) < 0.5)
  expect_true(abs(coefs["x2"]) < 0.5)

  # Interaction effect should be significant and close to 2
  expect_true(abs(coefs["x1:x2"]) > 1.5)
  expect_true(abs(coefs["x1:x2"]) < 2.5)

  # x3 effect should be close to 1
  expect_true(abs(coefs["x3"]) > 0.5)
  expect_true(abs(coefs["x3"]) < 1.5)

  # Noise effects should be small
  expect_true(abs(coefs["noise1"]) < 0.5)
  expect_true(abs(coefs["noise2"]) < 0.5)
})

test_that("all simulation functions handle different sample sizes", {
  sample_sizes <- c(50, 500)

  for (n in sample_sizes) {
    # Test each function with different sample sizes
    task_ind <- sim_dgp_independent(n = n)
    expect_equal(nrow(task_ind$data()), n)

    task_cor <- sim_dgp_correlated(n = n)
    expect_equal(nrow(task_cor$data()), n)

    task_med <- sim_dgp_mediated(n = n)
    expect_equal(nrow(task_med$data()), n)

    task_conf_h <- sim_dgp_confounded(n = n, hidden = TRUE)
    expect_equal(nrow(task_conf_h$data()), n)

    task_conf_o <- sim_dgp_confounded(n = n, hidden = FALSE)
    expect_equal(nrow(task_conf_o$data()), n)

    task_int <- sim_dgp_interactions(n = n)
    expect_equal(nrow(task_int$data()), n)
  }
})

test_that("simulation functions produce finite values", {
  # Test that all functions produce finite (no NaN, Inf, NA) values
  functions_to_test <- list(
    function() sim_dgp_independent(n = 50),
    function() sim_dgp_correlated(n = 50),
    function() sim_dgp_mediated(n = 50),
    function() sim_dgp_confounded(n = 50, hidden = TRUE),
    function() sim_dgp_confounded(n = 50, hidden = FALSE),
    function() sim_dgp_interactions(n = 50)
  )

  for (sim_fn in functions_to_test) {
    task <- sim_fn()
    data <- task$data()

    # Check for finite values
    expect_true(all(is.finite(as.matrix(data))))

    # Check for no missing values
    expect_true(all(!is.na(as.matrix(data))))
  }
})

test_that("simulation functions are reproducible with set.seed", {
  # Test reproducibility
  set.seed(123)
  task1 <- sim_dgp_independent(n = 100)
  data1 <- task1$data()

  set.seed(123)
  task2 <- sim_dgp_independent(n = 100)
  data2 <- task2$data()

  expect_equal(data1, data2)

  # Test with different function
  set.seed(456)
  task3 <- sim_dgp_interactions(n = 50)
  data3 <- task3$data()

  set.seed(456)
  task4 <- sim_dgp_interactions(n = 50)
  data4 <- task4$data()

  expect_equal(data3, data4)
})

test_that("sim_dgp_confounded default parameters work correctly", {
  # Test default parameters
  task_default <- sim_dgp_confounded()
  expect_equal(nrow(task_default$data()), 500) # Default n
  expect_false("confounder" %in% task_default$feature_names) # Default hidden=TRUE

  # Test explicit parameters match defaults
  task_explicit <- sim_dgp_confounded(n = 500L, hidden = TRUE)
  expect_equal(task_default$feature_names, task_explicit$feature_names)
  expect_equal(nrow(task_default$data()), nrow(task_explicit$data()))
})
