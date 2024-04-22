.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE) && "xplainvi" %in% strsplit(Sys.getenv("DEBUGME"), ",", fixed = TRUE)[[1L]]) {
    debugme::debugme()
  }
}
