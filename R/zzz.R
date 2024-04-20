.onLoad <- function(libname, pkgname) {
  if (requireNamespace("debugme", quietly = TRUE) && "explainvi" %in% strsplit(Sys.getenv("DEBUGME"), ",", fixed = TRUE)[[1L]]) {
    debugme::debugme()
  }
}
