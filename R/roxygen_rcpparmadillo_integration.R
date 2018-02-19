#' @useDynLib YetAnotherRStudioApplication, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
NULL

#' @title a test function
#' @export
sumxy <- function(x, y) {
  x + y
}