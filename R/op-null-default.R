#' Default value for `NULL`
#'
#' A backport of `%||%` available in R versions from 4.4.0.
#'
#' @param x,y If `x` is NULL or length 0, will return `y`; otherwise returns `x`.
#' @rawNamespace if (getRversion() < "4.4.0") export(`%||%`)
#' @name op-null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
