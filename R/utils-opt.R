#' xplain options
#'
#' Options are always initialized as `NA` and can be enabled as either an
#' environment variable `XPLAIN_OPTION` or R option `xplain.option`.
#' The R option has higher priority.
#' @keywords internal
#' @noRd
#' @param x `character(1)` Name of option to retrieve, e.g. `"debug"`.
#'   Will be translated to `XPLAIN_DEBUG` and `xplain.debug` respectively.
#'
#' @examples
#' xplain_opt("debug")
#' xplain_opt("progress")
#'
xplain_opt <- function(x) {
	opt <- getOption(paste0("xplain.", tolower(x)), default = NA)
	envvar <- Sys.getenv(toupper(paste0("xplain_", x)), unset = NA)

	# cli::cli_inform("opt: {.val {opt}}, env: {.val {envvar}}")
	opt <- as.logical(opt)
	if (is.na(opt)) {
		opt <- NULL
	}

	envvar <- as.logical(envvar)
	if (is.na(envvar)) {
		envvar <- NULL
	}

	# option has higher priority than env, if both NULL then FALSE
	# cli::cli_inform("opt: {.val {opt}}, env: {.val {envvar}}")
	opt %||% envvar %||% FALSE
}
