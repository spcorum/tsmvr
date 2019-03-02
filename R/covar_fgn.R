#' Fractional gaussian noise covariance matrix
#'
#' Constructs a fractional gaussian noise (FGN) covariance matrix.
#'
#' @param q dimension of covariance matrix (positive integer)
#' @param h Hurst parameter (0 < \code{h} < 1)
#' @references
#' \insertRef{MRCE}{tsmvr}
#' @return Returns an FGN covariance matrix with Hurst parameter
#' \code{h}.
#' @export
covar_fgn <- function(q, h = 0.9) {
  stopifnot(q %% 1 == 0, q > 0, h >= 0, h < 1)
  Sigma <- function(i, j, h) 0.5 * ((abs(i - j) + 1)^(2 * h) - 2 * (abs(i - j))^(2 * h) +
      # (abs(i - j) - 1)^(2 * h))
      ((abs(i - j) - 1)^2)^h)
  X <- diag(q)
  for (i in 1:q - 1) {
    for (j in ifelse(i < q, i + 1, i):q) {
      X[i, j] <- Sigma(i, j, h)
      X[j, i] <- X[i, j]
    }
  }
  return(X)
}
