#' Set parameters for tsmvr_solve
#'
#' Sets paramters for tsmvr_solve and functions that call tsmvr_solve
#' (tsmvr_cv, tsmvr_replicate, and tsmvr_gridsearch).
#'
#' @param B_type type of descent for regression steps (string: 'gd' or 'ls')
#' @param Omega_type (string: 'gd', 'ls' or 'min')
#' @param eta1 B-step learning rate (positive numeric)
#' @param eta2 Omega-step learning rate (positive numeric)
#' @param lambda1 B-step learning rate (positive numeric)
#' @param lambda2 Omega-step learning rate (positive numeric)
#' @param delta1 B initialization matrix inversion fudge factor (positive numeric)
#' @param delta2 Omega initialization matrix inversion fudge factor (positive numeric)
#' @param delta_Om Omega-step matrix inversion fudge factor (positive numeric)
#' @param rho1 B-step linesearch convergence parameter (positive numeric)
#' @param rho2 Omega-step linesearch convergence parameter (positive numeric)
#' @param beta1 B-step linesearch stepsize shrinking paramter (positive numeric)
#' @param beta2 Omega-step linesearch stepsize shrinking paramter (positive numeric)
#' @param qmax1 B-step linesearch maximum number of iterations (positive integer valued numeric)
#' @param qmax2 Omega-step linesearch maximum number of iterations (positive integer valued numeric)
#' @param eps1 B-step convergence parameter (positive numeric)
#' @param eps1 Omega-step convergence parameter (positive numeric)
#' @param max_iter maximum number of iterations (positive integer)
#' @param skip iteration skip frequency for output to screen (positive integer)
#' @param quiet whether or not to operate   (bool)
#' @param suppress whether or not to suppress warnings (bool)
#'
#' @return Returns a named list of input parameters.
#'
#' See also \code{\link{tsmvr_solve}},
#' \code{\link{tsmvr_cv}},
#' \code{\link{tsmvr_replicate}}, and
#' \code{\link{tsmvr_gridsearch}}.
#'
#' @references
#' \insertRef{chen2016high}{tsmvr}
#' \insertRef{chen2018covariate}{tsmvr}
#'
#' @export
set_parameters = function(type_B = 'ls', type_Om = 'ls',
                          eta1 = 0.01, eta2 = 0.01,
                          lam1 = 0.1, lam2 = 0.1,
                          del1 = 1e-6, del2 = 1e-6, del_Om = 0,
                          rho1 = 1, rho2 = 1,
                          beta1 = 0.5, beta2 = 0.5,
                          qmax1 = 48, qmax2 = 48,
                          eps1 = 1e-4, eps2 = 1e-4,
                          max_iter = 2000, skip = 10,
                          quiet = F, suppress = F) {
  return(
    list(
      type_B = type_B, type_Om = type_Om, eta1 = eta1, eta2 = eta2,
      lam1 = lam1, lam2 = lam2, del1 = del1, del2 = del2, del_Om = del_Om,
      rho1 = rho1, rho2 = rho2, beta1 = beta1, beta2 = beta2,
      qmax1 = qmax1, qmax2 = qmax2, eps1 = eps1, eps2 = eps2,
      max_iter = max_iter, skip = skip, quiet = quiet, suppress = suppress
    )
  )
}
