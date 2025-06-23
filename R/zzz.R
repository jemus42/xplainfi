.onLoad = function(libname, pkgname) {
  options("xplainfi.debug" = as.logical(Sys.getenv("XPLAINFI_DEBUG", unset = FALSE)))
}
