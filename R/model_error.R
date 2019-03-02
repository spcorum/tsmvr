#' Model error
#'
#' Given a positive definite covariance matrix \eqn{\Sigma}, this
#' function calculates the model error between two matrices \eqn{U}
#' and \eqn{V} according to the formula
#' \deqn{Trace [ (U-V)' \Sigma' \Sigma (U-V) ]/(mn),}
#' where \eqn{m}, \eqn{n} are the dimensions of \eqn{U} (\eqn{V}).
#' If no covariance matrix is specified, the function calculates the
#' model error using the identity covariance matrix, which is
#' equivalent the normalized squared matrix error).
#'
#' @param U matrix (m-by-n)
#' @param V matrix (m-by-n)
#' @param Sigma positive definite covariance matrix (m-by-m)
#' @return The model error between \code{U} and \code{V} given
#' \code{Sigma}.
#'
# #' @note
# #' See also \code{\link[tsmvr]{squared_error}}.
#'
#' @references
#' \insertRef{MRCE}{tsmvr}
#'
#' @export
model_error <- function(U, V, Sigma = NULL) {
  stopifnot(
    is.matrix(U), is.matrix(V),
    is.matrix(Sigma) || is.null(Sigma),
    all(dim(U) == dim(V)), dim(V)[2] == dim(Sigma)[1],
    is.null(Sigma) ||
      matrixcalc::is.positive.definite(Sigma)
  )

  A <- U - V
  if (is.null(Sigma)) Sigma <- diag(dim(A)[2])
  return(matrixcalc::matrix.trace(A %*% Sigma %*% t(A)) /
    dim(A)[1] / dim(A)[2])
}
