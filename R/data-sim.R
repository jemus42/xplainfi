#' Simulate data as in Ewald et al. (2024)
#'
#'
#' – X1, X3 and X5 are independent and standard normal: Xj ∼ N (0, 1),
#' – X2 is a noisy copy of X1: X2 := X1 + eps_2, eps_2 ∼ N (0, 0.001),
#' – X4 is a (more) noisy copy of X3: X4 := X3 + eps_4, eps_4 ∼ N (0, 0.1),
#' – Y depends on X4 and X5 via linear effects and a bivariate interaction: Y := X4 + X5 + X4 ∗ X5 + eps_Y , eps_Y ∼ N (0, 0.1).
#'
#' @param n (`integer(1)`) Number of samples to create.
#'
#' @return A regression task ([mlr3::TaskRegr]) with [data.table][data.table::data.table] backend.
#' @export
#' @references `r print_bib("ewald_2024")`
#' @examples
#' sim_dgp_ewald(100)
#'
sim_dgp_ewald <- function(n = 500) {
  x1 <- runif(n)
  x3 <- runif(n)
  x5 <- runif(n)

  x2 <- x1 + rnorm(n, 0, 0.001)
  x4 <- x3 + rnorm(n, 0, 0.1)

  y <- x4 + x5 + x4 * x5 + rnorm(n, 0, 0.1)

  xdf <- data.table::data.table(
    y,
    x1,
    x2,
    x3,
    x4,
    x5
  )

  mlr3::TaskRegr$new(backend = xdf, target = "y", id = paste0("Ewald_", n))
}

#' @title Simulation DGPs for Feature Importance Method Comparison
#' @name sim_dgp_scenarios
#' @description
#' These data generating processes (DGPs) are designed to illustrate specific
#' strengths and weaknesses of different feature importance methods like PFI, CFI, and RFI.
#' Each DGP focuses on one primary challenge to make the differences between methods clear.
#'
#' @references `r print_bib("ewald_2024")`
NULL

#' @describeIn sim_dgp_scenarios Correlated features demonstrating PFI's limitations
#'
#' @details
#' **Correlated Features DGP:**
#' This DGP creates highly correlated predictors where PFI will show artificially low
#' importance due to redundancy, while CFI will correctly identify each feature's
#' conditional contribution.
#'
#' - `x1`: Standard normal, direct effect on y
#' - `x2`: Nearly perfect copy of x1 (x1 + small noise)
#' - `x3`: Independent standard normal, direct effect on y
#' - `x4`: Independent standard normal, no effect on y
#'
#' Expected behavior:
#' - **PFI**: Will show low importance for x1 and x2 due to redundancy
#' - **CFI**: Will show high importance for both x1 and x2 when conditioned properly
#' - **Ground truth**: Both x1 and x2 have causal effects, x3 has effect, x4 has none
#'
#' @param n (`integer(1)`) Number of samples to generate.
#' @return A regression task ([mlr3::TaskRegr]) with [data.table][data.table::data.table] backend.
#' @export
#' @examples
#' task = sim_dgp_correlated(200)
#' task$data()
sim_dgp_correlated <- function(n = 500L) {
  # Two highly correlated features with causal effects
  x1 <- rnorm(n)
  x2 <- x1 + rnorm(n, 0, 0.05) # Nearly perfect copy with small noise

  # Independent features
  x3 <- rnorm(n) # Has causal effect
  x4 <- rnorm(n) # No causal effect (noise)

  # Outcome depends on correlated features x1, x2 and independent x3
  y <- 2 * x1 + 1.5 * x2 + x3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("correlated_", n))
}

#' @describeIn sim_dgp_scenarios Mediated effects showing direct vs total importance
#'
#' @details
#' **Mediated Effects DGP:**
#' This DGP demonstrates the difference between total and direct causal effects.
#' Some features affect the outcome only through mediators.
#'
#' - `exposure`: Has no direct effect on y, only through mediator
#' - `mediator`: Mediates the effect of exposure on y
#' - `direct`: Has both direct effect on y and effect on mediator
#' - `noise`: No causal relationship to y
#'
#' Causal structure: exposure → mediator → y ← direct → mediator
#'
#' Expected behavior:
#' - **PFI**: Shows total effects (exposure appears important)
#' - **CFI**: Shows direct effects (exposure appears less important when conditioning on mediator)
#' - **RFI with mediator**: Should show direct effects similar to CFI
#'
#' @export
#' @examples
#' task = sim_dgp_mediated(200)
#' task$data()
sim_dgp_mediated <- function(n = 500L) {
  # Initial exposure variable
  exposure <- rnorm(n)

  # Direct predictor that affects both mediator and outcome
  direct <- rnorm(n)

  # Mediator affected by both exposure and direct
  mediator <- 0.8 * exposure + 0.6 * direct + rnorm(n, 0, 0.3)

  # Noise variable
  noise <- rnorm(n)

  # Outcome depends on mediator and direct effect, but NOT directly on exposure
  y <- 1.5 * mediator + 0.5 * direct + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    exposure = exposure,
    mediator = mediator,
    direct = direct,
    noise = noise
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("mediated_", n))
}

#' @describeIn sim_dgp_scenarios Confounding scenario for conditional sampling
#'
#' @details
#' **Confounding DGP:**
#' This DGP includes a confounder that affects both features and the outcome.
#' Uses simple coefficients for easy interpretation.
#'
#' Model structure:
#' - Hidden confounder H ~ N(0,1)
#' - x1 = H + noise, x2 = H + noise (both affected by confounder)
#' - proxy = H + noise (noisy measurement of confounder)
#' - independent ~ N(0,1) (truly independent)
#' - y = H + 0.5*x1 + 0.5*x2 + independent + noise
#'
#' Expected behavior:
#' - **PFI**: Will show inflated importance for x1 and x2 due to confounding
#' - **CFI**: Should partially account for confounding through conditional sampling
#' - **RFI conditioning on proxy**: Should reduce confounding bias by controlling for H
#'
#' @export
#' @examples
#' task = sim_dgp_confounded(200)
#' task$data()
sim_dgp_confounded <- function(n = 500L) {
  # Hidden confounder
  confounder <- rnorm(n)

  # Features affected by confounder
  x1 <- confounder + rnorm(n, 0, 0.5)
  x2 <- confounder + rnorm(n, 0, 0.5)

  # Proxy measurement of confounder (observable but noisy)
  proxy <- confounder + rnorm(n, 0, 0.5)

  # Independent feature unaffected by confounder
  independent <- rnorm(n)

  # Outcome affected by confounder and all features
  y <- confounder + 0.5 * x1 + 0.5 * x2 + independent + rnorm(n, 0, 0.5)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    proxy = proxy,
    independent = independent
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("confounded_", n))
}

#' @describeIn sim_dgp_scenarios Interaction effects between features
#'
#' @details
#' **Interaction Effects DGP:**
#' This DGP demonstrates a pure interaction effect where features have no main effects.
#'
#' Model: y = 2 * x1 * x2 + x3 + noise
#'
#' - `x1`, `x2`: Independent features with ONLY interaction effect (no main effects)
#' - `x3`: Independent feature with main effect only
#' - `noise1`, `noise2`: No causal effects
#'
#' Expected behavior:
#' - **PFI**: Should assign near-zero importance to x1 and x2 (no marginal effect)
#' - **CFI**: Should capture the interaction and assign high importance to x1 and x2
#' - **Ground truth**: x1 and x2 are important ONLY through their interaction
#'
#' @export
#' @examples
#' task = sim_dgp_interactions(200)
#' task$data()
sim_dgp_interactions <- function(n = 500L) {
  # Independent features for interaction
  x1 <- rnorm(n)
  x2 <- rnorm(n)

  # Independent feature with main effect
  x3 <- rnorm(n)

  # Noise features
  noise1 <- rnorm(n)
  noise2 <- rnorm(n)

  # Outcome with ONLY interaction between x1 and x2 (no main effects), plus main effect of x3
  y <- 2 * x1 * x2 + x3 + rnorm(n, 0, 0.5)

  data.table::data.table(
    y = y,
    x1 = x1,
    x2 = x2,
    x3 = x3,
    noise1 = noise1,
    noise2 = noise2
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("interactions_", n))
}

#' @describeIn sim_dgp_scenarios Independent features baseline scenario
#'
#' @details
#' **Independent Features DGP:**
#' This is a baseline scenario where all features are independent and their
#' effects are additive. All importance methods should give similar results.
#'
#' - `important1-3`: Independent features with different effect sizes
#' - `unimportant1-2`: Independent noise features with no effect
#'
#' Expected behavior:
#' - **All methods**: Should rank features consistently by their true effect sizes
#' - **Ground truth**: important1 > important2 > important3 > unimportant1,2 ≈ 0
#'
#' @export
#' @examples
#' task = sim_dgp_independent(200)
#' task$data()
sim_dgp_independent <- function(n = 500L) {
  # Independent important features with different effect sizes
  important1 <- rnorm(n)
  important2 <- rnorm(n)
  important3 <- rnorm(n)

  # Independent unimportant features
  unimportant1 <- rnorm(n)
  unimportant2 <- rnorm(n)

  # Additive linear outcome
  y <- 2.0 * important1 + 1.0 * important2 + 0.5 * important3 + rnorm(n, 0, 0.2)

  data.table::data.table(
    y = y,
    important1 = important1,
    important2 = important2,
    important3 = important3,
    unimportant1 = unimportant1,
    unimportant2 = unimportant2
  ) |>
    mlr3::TaskRegr$new(target = "y", id = paste0("independent_", n))
}
