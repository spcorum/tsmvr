#' Construct covariance and precision matrices
#'
#' Constructs a covariance matrix and its associated precision matrix
#' of the following types: first-order autoregressive [AR(1)],
#' fractional gaussian noise [FGN], or scale free network [SFN].
#'
#' @param q dimension of covariance matrix (positive integer)
#' @param type type of covariance matrix (string: 'AR1', 'FGN', or 'SFN')
#' @param rho autoregression parameter for AR(1) covariance matrix (0 < \code{rho_err} < 1)
#' @param h Hurst parameter for FGN covariance matrix (0 < \code{H} < 1)
#' @param power scaling power for SFN covariance matrix (positive numeric)
#' @param zero_appeal Barabasi algorithm baseline attractiveness for SFN covariance matrix (positive numeric)
#' @param n_edge Barabasi algorithm number of edges per step for SFN covariance matrix (positive integer)
#' @param min_ev minimum eigenvalue of SFN covariance matrix (\code{min_ev} > 0) (ensures matrix is PSD) )
#' @references
#' \insertRef{MRCE}{tsmvr}
#'
#' \insertRef{chen2016high}{tsmvr}
#' @return A list of two matrices, the \code{covariance} matrix and the
#' \code{precision} matrix.
#' @export
covariance_matrix <- function(q, type = "AR1", rho = 0.7, h = 0.9,
                              power = 1, n_edge = 1, zero_appeal = 1,
                              min_ev = 0.18) {
  stopifnot(
    q %% 1 == 0, q > 0, rho >= 0, rho < 1, h >= 0, h < 1,
    power > 0, zero_appeal > 0, n_edge > 0, min_ev > 0
  )

  if (type == "AR1") {
    covariance <- covar_ar1(q, rho)
    precision <- zapsmall(chol2inv(chol(covariance)))
  }
  else if (type == "FGN") {
    covariance <- covar_fgn(q, h)
    precision <- zapsmall(chol2inv(chol(covariance)))
  }
  else if (type == "SFN") {
    precision <- precision_sfn(q,
      power = power, zero_appeal = zero_appeal,
      n_edge = n_edge, min_ev = min_ev
    )
    covariance <- zapsmall(chol2inv(chol(precision)))
  }
  return(list(covariance = covariance, precision = precision))
}
