.onLoad = function(libname, pkgname) {
  # Register sequential backend to suppress foreach warnings when no parallel backend is set
  if (requireNamespace("foreach", quietly = TRUE)) {
    if (!foreach::getDoParRegistered()) {
      foreach::registerDoSEQ()
    }
  }
}
