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
#'
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
