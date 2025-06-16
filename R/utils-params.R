#' Hierarchical parameter resolution helper
#'
#' Used to retrieve the highest-priority parameter value.
#'
#' Priority is `arg` > `stored` > `default`.
#'
#' @param arg,stored,default Scalar parameter values evlauted in that order
#'
#' @return First non-`NULL` parameter
#' @keywords internal
#' @examples
#' resolve_param(arg = "foo", stored = "bar", default = "baz")
#' resolve_param(arg = NULL, stored = "bar", default = "baz")
#' resolve_param(arg = NULL, stored = NULL, default = "baz")
resolve_param = function(arg, stored, default) (arg %||% stored) %||% default
