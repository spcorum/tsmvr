#' Truly Spare MultiVariate Regression
#'
#' Calculates sparse solutions to the multivariate regression problem with
#' correlated errors. Let \eqn{X} be the \eqn{n-by-p} predictor matrix,
#' \eqn{Y} be the \eqn{n-by-q} response matrix, \eqn{B} be the \eqn{p-by-q}
#' regression matrix, and \eqn{\Omega} be the \eqn{q-by-q} precision matrix
#' (inverse of the covariance matrix \eqn{\Sigma}). Let \eqn{s1} be a sparsity
#' parameter that only allows the top \eqn{s1} values for \eqn{B} in aboslute
#' value be nonzero, and lets \eqn{s2} be defined similarly for \eqn{\Omega}. Then
#' this functions calculates the solution to
#' \deqn{(B_hat,\Omega_hat) = argmin_(B,\Omega)
#' (1/n*Tr[(Y-XB)^T*\Omega*(Y-XB)] - log|\Omega}|)
#' such that \eqn{||B||_(1,1) \le s1} and \eqn{||\Omega||_(1,1) \le s2}. Here
#' \eqn{|| * ||_(1,1)} indicates the 1-1 "norm" that counts the number of nonzero
#' matrix entries.
#'
#' @param X n-by-p predictor matrix
#' @param Y n-by-q response matrix
#' @param s1 sparsity for p-by-q regressor matrix \code{B} (positive integer or NULL)
#' @param s2 sparsity for q-by-q precision matrix \code{Omega} (positive integer or NULL)
#' @param s1_vec \code{s1} values for gridsearch (vector of integer valued numerics or NULL)
#' @param s2_vec \code{s2} values for gridsearch (vector of integer valued numerics or NULL)
#' @param pars list of algorithm parameters as constructed by the \code{set_parameters} function
#' @param seed set random seed (integer or NULL)
#'
#' @return A list containing the \code{mean} and \code{sd} of the
#' error over the replicates as well as the means and standard
#' deviations of the errors across each fold.
#'
#' @note See also \code{\link{set_parameters}}.
#'
#' @useDynLib tsmvr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @export
tsmvr = function(X, Y, s1 = NULL, s2 = NULL, s1_vec = NULL,
                 s2_vec = NULL, method = c("single", "gs"),
                 pars, quiet = FALSE, seed = NULL) {

  stopifnot(
    is.numeric(X), is.matrix(X),
    is.numeric(Y), is.matrix(Y),
    dim(X)[1] == dim(Y)[1],
    is.null(s1) || (is.numeric(s1) && s1%%1 == 0),
    s1 <= dim(X)[2] * dim(Y)[2],
    is.null(s2) || (is.numeric(s2) && s2%%1 == 0),
    s2 <= dim(Y)[2]^2,
    is.null(s1_vec) || (is.numeric(s1_vec) && s1_vec%%1 == 0),
    s1_vec <= dim(X)[2] * dim(Y)[2],
    is.null(s2_vec) || (is.numeric(s2_vec) && s2_vec%%1 == 0),
    s2 <= dim(Y)[2]^2,
    is.list(pars),
    is.null(seed) || is.numeric(seed),
    method == "single" && !is.null(s1) && !is.null(s2) ||
      method == "gs" && !is.null(s1_vec) && !is.null(s2_vec)
  )

  if (method == "single") {

    output2 = list()
    output2$error_min = NULL
    output2$error_min_sd = NULL
    output2$s1_min = NULL
    output2$s2_min = NULL
    output2$s1_vec = NULL
    output2$s2_vec = NULL
    output2$folds = NULL
    output2$reps = NULL
    output2$gs_time = NULL

    tic <- Sys.time()
    output1 = tsmvr_solve(X = X, Y = Y, s1 = s1, s2 = s2, pars = pars)
    output2$model_time = (Sys.time() - tic)[[1]]

    output = c(output1, output2)
  }


  else if (method == "gs") {
    output2 = tsmvr_gridsearch(
      X = X, Y = Y, s1_vec = s1_vec, s2_vec = s2_vec, pars = pars,
      quiet = pars$quiet, seed = seed
    )
    pars$quiet = T
    tic <- Sys.time()
    output1 = tsmvr_solve(
      X = X, Y = Y, s1 = output2$s1_min, s2 = output2$s2_min, pars = pars
    )
    output2$model_time = (Sys.time() - tic)[[1]]
    output = c(output1,output2)
  }

  return(output)
}
