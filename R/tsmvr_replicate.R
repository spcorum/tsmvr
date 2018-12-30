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
#' @param k number of k-folds (integer greater than 1)
#' @param reps number of replications (positive integer)
#' @param B_type B-step descent type (string: 'gd')
#' @param Omega_type Omega-step descent type (string: 'gd' or 'min')
#' @param eta1 B-step learning rate (positive numeric)
#' @param eta2 Omega-step learning rate (positive numeric)
#' @param epsilon convergence parameter (positive numeric)
#' @param max_iter maximum number of allowed iterations (positive integer)
#' @param quiet quiet mode (bool)
#' @param seed set random seed (integer)
#'
#' @return A list containing the \code{mean} and \code{sd} of the
#' error over the replicates as well as the means and standard
#' deviations of the errors across each fold.
#' See also \code{\link{squared_error}}, \code{\link{k_folds}},
#' \code{\link{tsmvr_cv}}, and \code{\link{tsmvr_solve}}.
#'
#'
#' @export
tsmvr_replicate <- function(X, Y, s1, s2, k = 10, reps = 1, B_type = "gd",
                            Omega_type = "min", eta1 = 0.01, eta2 = 0.01,
                            epsilon = 1e-5, max_iter = 1000, quiet = FALSE,
                            seed = NULL) {

  stopifnot(
    is.numeric(X), is.matrix(Y), is.numeric(Y), is.matrix(Y),
    is.numeric(s1), s1 >= 0, is.numeric(s2), s2 >= dim(Y)[2],
    s1 <= dim(X)[2] * dim(Y)[2], s2 <= (dim(Y)[2])^2,
    s1 %% 1 == 0, s2 %% 1 == 0,
    k %% 1 == 0, k > 1, reps %% 1 == 0, reps > 0,
    is.character(B_type), is.character(Omega_type),
    B_type %in% c('gd'), Omega_type %in% c('gd', 'min'),
    is.numeric(epsilon), epsilon > 0,
    is.numeric(max_iter), max_iter > 0, max_iter %% 1 == 0,
    is.null(seed) || is.numeric(seed)
  )

  # Initialize objects.
  n <- nrow(X)
  fold_error <- rep(0, reps)
  fold_sd <- rep(0, reps)

  # Header
  if (!quiet) {
    cat("Truly Sparse multivariate regression with cross validation: reps = ", reps, ", folds = ", k, "\n", sep = "")
    cat("rep\terror\t\ttime (s)\n")
  }

  # Iterate over reps.
  set.seed(seed)
  tic <- Sys.time()
  for (r in 1:reps) {

    # For each rep, iterate over folds.
    fold_result <- tsmvr_cv(
      X = X, Y = Y, s1 = s1, s2 = s2,
      k = k, B_type = B_type,
      Omega_type = Omega_type,
      eta1 = eta1, eta2 = eta2,
      epsilon = epsilon, max_iter = max_iter,
      quiet = T
    )

    # Record results.
    fold_error[r] <- fold_result$error_mean
    fold_sd[r] <- fold_result$error_sd
    toc <- (Sys.time() - tic)

    # Print to screen.
    if (!quiet) {
      cat(r, "\t", fold_error[r], "\t", round(toc, 3), "\n",
        sep = "")
    }

  }

  # Return results.
  return(list(
    rep_error_mean = mean(fold_error), rep_error_sd = sd(fold_error),
    fold_error_means = fold_error, fold_error_sds = fold_sd,
    folds = k, reps = reps
  ))
}
