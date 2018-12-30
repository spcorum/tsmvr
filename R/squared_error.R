#' Squared error
#'
#' This function calculates the normalized squared error between two
#' matrices \eqn{U} and \eqn{V} according to the formula
#' \deqn{Trace [ (U-V)' (U-V) ]/(mn),}
#' where \eqn{m}, \eqn{n} are the dimensions of \eqn{U} (\eqn{V}).
#'
#' @param U matrix (m-by-n)
#' @param V matrix (m-by-n)
#'
#' @references
#' \insertRef{MRCE}{tsmvr}
#'
#' @useDynLib tsmvr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#'
#' @export
squared_error <- function(U, V) {
  stopifnot(is.matrix(U), is.matrix(V), all(dim(U) == dim(V)))
  A <- U - V
  return(matrixcalc::matrix.trace(tcrossprod(A)) / dim(A)[1] / dim(A)[2])
}
