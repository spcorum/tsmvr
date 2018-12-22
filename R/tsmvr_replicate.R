#' Replicated k-fold cross-validation for tsmvr
#'
#' Calculates the mean and standard deviation of fold-avereged errors
#' derived from replcated tsmvr k-fold cross-validation experiments.
#' The error for each base model is the normalized squared
#' error between the true response and the predicted response
#' on a given cross-validation set.
#'
#' @param X design matrix (n-by-p)
#' @param Y response matrix (n-by-q)
#' @param s1 regressor matrix sparsity (positive integer)
#' @param s2 covariance matrix sparsity (positive integer)
#' @param K number of folds (integer greater than 1)
#' @param reps number of replications (positive integer)
#' @param b_type B-step descent type (string: 'gd')
#' @param omega_type Omega-step descent type (string: 'gd' or 'min')
#' @param eta1 B-step learning rate (positive numeric)
#' @param eta2 Omega-step learning rate (positive numeric)
#' @param epsilon convergence parameter (positive numeric)
#' @param maxiter maximum number of allowed iterations (positive integer)
#' @param quiet quiet mode (bool)
#' @param seed set random seed (integer)
#'
#' @return A list containing the \code{mean} and \code{sd} of the
#' error over the replicates as well as the means and standard
#' deviations of the errors across each fold.
#' See also \code{\link{squared_error}}, \code{\link{kfolds}},
#' \code{\link{tsmvr_cv}}, and \code{\link{tsmvr_solve}}.
#'
#'
#' @export
tsmvr_replicate <- function(X, Y, s1, s2, K = 10, reps = 1, b_type = "gd",
                            omega_type = "min", eta1 = 0.01, eta2 = 0.01,
                            epsilon = 1e-5, maxiter = 1000, quiet = FALSE,
                            seed = NULL) {

  # Initialize objects.
  n <- nrow(X)
  fold_error <- rep(0, reps)
  fold_sd <- rep(0, reps)

  # Header
  if (!quiet) {
    cat("Truly Sparse multivariate regression with cross validation: reps = ", reps, ", folds = ", K, "\n", sep = "")
    cat("rep\terror\t\ttime (s)\n")
  }

  # Iterate over reps.
  set.seed(seed)
  tic <- Sys.time()
  for (r in 1:reps) {

    # For each rep, iterate over folds.
    fold_result <- tsmvr_cv(
      X = X, Y = Y, s1 = s1, s2 = s2,
      K = K, b_type = b_type,
      omega_type = omega_type,
      eta1 = eta1, eta2 = eta2,
      epsilon = epsilon, maxiter = maxiter,
      quiet = T
    )

    # Record results.
    fold_error[r] <- fold_result$error
    fold_sd[r] <- fold_result$sd

    # Print to screen.
    toc <- (Sys.time() - tic)
    cat(r, "\t", fold_error[r], "\t", round(toc, 3), "\n", sep = "")
  }

  # Return results.
  return(list(
    rep_error = mean(fold_error), rep_sd = sd(fold_error),
    fold_errors = fold_error, fold_sds = fold_sd
  ))
}