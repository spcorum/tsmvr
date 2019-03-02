#' Multivariate normal random vector generator
#'
#' Generates vectors iid. from the multivariate normal distribution.
#'
#' @param n number of realizations (positive integer-valued numeric)
#' @param mu mean (\eqn{\mu}) (numeric)
#' @param Sigma positive definite covariance matrix (\eqn{\Sigma})
#' @param reps number of random matrix realizations to return (positive integer-valued numeric)
#' @param seed seed for pseudo-random number generator (numeric)
#'
#' @return Returns a \code{rep} length list of matrices of \code{n}
#' stacked vectors, each an iid. realization of \eqn{N(\mu,\Sigma)}.
#' If \code{reps}=1, then a single matrix returned not in a list. Otherwise,
#' \code{reps} matrices are returned in a list.
#'
#' @export
mvrnorm <- function(n, mu = 0, Sigma, reps = 1, seed = NULL) {
  p <- length(mu)
  Sigma.dim <- dim(Sigma)
  stopifnot(
    matrixcalc::is.positive.definite(Sigma), p == dim(Sigma)[1], reps >= 1,
    reps %% 1 == 0
  )

  # Calculate the eigen-decomposition of Sigma and Sigma^(1/2) --------
  eigen.object <- eigen(Sigma, symmetric = T)
  Sigma.sqrt <- t(eigen.object$vectors) %*% diag(eigen.object$values^0.5) %*%
    eigen.object$vectors

  # Generate an n-by-p random standard normal matrix(es) and
  # n-by-p matrix that is row-wise iid N_p(mu,Sigma) ------------------
  N.list <- as.list(rep(0, reps))
  set.seed(seed)
  for (i in 1:reps) {
    Z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    N.list[[i]] <- rep(1, n) %*% t(mu) + Z %*% Sigma.sqrt
  }
  return(N.list)
  # if (reps == 1) {
  #   return(N.list[[1]])
  # } else {
  #   return(N.list)
  # }
}
