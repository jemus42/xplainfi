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

#' Example DGP for illustration
#'
#'
#' - x1: random uniform, direct effect on y
#' - xm, xme: random uniform, xme is the mediated exposure, xm is the mediator, no direct effect of xme on y
#' - xi1,2: random uniform, interaction effect only
#' - z: random uniform, xz is noisy version of z, only z has effect on y
#' - xc1,2,3: correlated multivariate normal, additive effects on y
#' - xbin: random bernoulli with prob 1/3
#' - znoise: random uniform and random normal noise terms, no effect on y
#'
#' \eqn{y = x1 + x2 + xi1 * xi2 + z + xc1 + xc2 + xc3 + xbin + \epsilon}
#'
#' @param n (`integer(1)`: 100L) Number of samples to create.
#'
#' @return A regression task ([mlr3::TaskRegr]) with [data.table][data.table::data.table] backend.
#' @export
#' @importFrom stats runif rnorm rbinom toeplitz
#' @examples
#' sim_dgp_example(100)
sim_dgp_example <- function(n = 100L) {
  # Don't want to add mvtnorm to Suggests: for now
  require_package("mvtnorm")

  # x1 is independent uniform predictr
  x1 <- runif(n)
  # xm is mediator for "exposure" xme: xme -> xm -> y
  xme <- runif(n)
  xm <- xme + rnorm(n, 0, 0.1)

  # xi1,2 are independent interaction effects
  xi1 <- runif(n)
  xi2 <- runif(n)

  # z is a confounder affecting xz and y
  # z -> y and z -> xz but xz !-> y
  z <- runif(n)
  xz <- z + rnorm(n, 0, 0.1)

  # 3 correlated features for good measure
  xc <- mvtnorm::rmvnorm(n = n, sigma = stats::toeplitz(0.5^(0:2)))
  colnames(xc) <- c("xc1", "xc2", "xc3")

  # a binary
  xbin <- rbinom(100, 1, 1 / 3)

  znoise <- runif(n)

  y <- x1 + xme + xi1 * xi2 + z + xc[, 1] + xc[, 2] + xc[, 3] + xbin + rnorm(n, 0, 0.1)

  xdf <- data.table::data.table(
    y,
    x1,
    xme,
    xm,
    xi1,
    xi2,
    z,
    xz,
    xc,
    xbin,
    znoise
  )

  mlr3::TaskRegr$new(backend = xdf, target = "y", id = paste0("Example_", n))
}
