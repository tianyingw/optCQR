#' @useDynLib optCQR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("optCQR: Optimal Conformalized Quantile Regression")
  packageStartupMessage("For help, type: ?optCQR")
}
