#' Package dependency helper
#'
#' Error if the requried package is not installed and show an informative
#' message to the user about how to install it, defaulting to installation
#' from CRAN via `install.packages()` or some other source, e.g. a GitHub URL.
#'
#' @param pkg (`character(1)`) Package name to check.
#' @param from (`character(1)`: `"cran"`) Package source. If not `"cran"`,
#'   should be the URL to the package remote, e.g. a GitHub URL.
#'
#' @return If package is available: `TRUE` (invisibly). Error otherwise.
#' @noRd
#' @keywords internal
#' @examples
#'
#' require_package("arf")
#' require_package("seqknockoff", from = "https://github.com/kormama1/seqknockoff")
#'
require_package <- function(pkg, from = "cran") {
  has_pkg <- length(find.package(pkg, quiet = TRUE)) > 0

  if (has_pkg) {
    return(invisible(TRUE))
  }

  msg <- "Package {.pkg {pkg}} required but not found!"

  if (from == "cran") {
    instruct <- "Install it from CRAN with {.code install.packages(\"{pkg}\")}"
  } else {
    instruct <- "Install it from {.url {from}}"
  }

  cli::cli_abort(c(x = msg, i = instruct))
}
