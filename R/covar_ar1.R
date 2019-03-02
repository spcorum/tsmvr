#' Autoregression covariance matrix
#'
#' Constructs a first order autoregression [AR(1)] covariance matrix.
#'
#' @param q dimension of covariance matrix (positive integer)
#' @param rho autoregression parameter (0 <= \code{rho} <= 1)
#' @references
#' \insertRef{MRCE}{tsmvr}
#' @return Returns an AR(1) covariance matrix of parameter \eqn{\rho} and
#' dimension \eqn{q x q}.
#' @export
covar_ar1 <- function(q, rho = 0.7) {
  stopifnot(q %% 1 == 0, q > 0, rho >= 0, rho < 1)
  X <- diag(q)
  for (i in 1:q - 1) {
    for (j in ifelse(i < q, i + 1, i):q) {
      X[i, j] <- rho^abs(i - j)
      X[j, i] <- X[i, j]
    }
  }
  return(X)
}
