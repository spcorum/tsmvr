#' Replicated k-fold cross-validation for tsmvr
#'
#' Calculates the mean and standard deviation of fold-averaged errors
#' derived from replicated tsmvr k-fold cross-validation experiments.
#' The error for each base model is the normalized squared
#' error between the true response and the predicted response
#' on a given cross-validation set.
#'
#' @param X design matrix (n-by-p)
#' @param Y response matrix (n-by-q)
#' @param s1 regressor matrix sparsity (positive integer)
#' @param s2 covariance matrix sparsity (positive integer)
#' @param pars list of algorithm parameters; output of \code{set_parameters}
#' @param quiet whether or not to print replicate statuses to the screen (bool)
#' @param seed set random seed (integer)
#'
#' @return A list containing the \code{mean} and \code{sd} of the
#' error over the replicates as well as the means and standard
#' deviations of the errors across each fold.
#'
#' @note See also \code{\link{k_folds}},
#' \code{\link{tsmvr}},
#' \code{\link{tsmvr_cv}}, and
#' \code{\link{set_parameters}}.
#'
#'
# #' @export
tsmvr_replicate <- function(X, Y, s1, s2, pars, quiet = F, seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(X),
    is.numeric(Y), is.matrix(Y),
    dim(X)[1] == dim(Y)[1],
    is.numeric(s1), s1 %% 1 == 0, s1 > 0, s1 <= dim(X)[2] * dim(Y)[2],
    is.numeric(s2), s2 %% 1 == 0, s2 > 0, s2 <= (dim(Y)[2])^2,
    is.list(pars),
    is.null(seed) || is.numeric(seed)
  )

  # Initialize objects.
  n <- nrow(X)
  fold_error <- rep(0, pars$reps)
  fold_sd <- rep(0, pars$reps)

  # Header
  if (!quiet) {
    cat("Solver mode ", pars$B_type, "-", pars$Omega_type, " with eta1 = ", pars$eta1, sep = "")
    if (pars$Omega_type == "min") {
      cat(".\n", sep = "")
    } else {
      cat(" and eta2 = ", pars$eta2, ".\n", sep = "")
    }
    cat("rep\terror\t\ttime (s)\n")
  }

  # Iterate over reps.
  set.seed(seed)
  tic <- Sys.time()
  for (r in 1:pars$reps) {

    # For each rep, iterate over folds.
    if (is.null(seed)) {
      temp_seed <- NULL
    } else {
      temp_seed <- seed + 10 * (r - 1)
    }
    # print(temp_seed)
    fold_result <- tsmvr_cv(
      X = X, Y = Y, s1 = s1, s2 = s2,
      pars = pars, quiet = T,
      seed = temp_seed
    )

    # Record results.
    fold_error[r] <- fold_result$error_mean
    fold_sd[r] <- fold_result$error_sd
    toc <- (Sys.time() - tic)

    # Print to screen.
    if (!quiet) {
      cat(r, "\t", fold_error[r], "\t", round(toc, 3), "\n",
        sep = ""
      )
    }
  }

  toc <- (Sys.time() - tic)
  rep_error_mean <- mean(fold_error)
  rep_error_sd <- sd(fold_error)

  # Print final result to screen.
  if (!quiet) {
    cat("Replicate mean = ", rep_error_mean, "\n", sep = "")
    cat("Replicate sd = ", rep_error_sd, "\n", sep = "")
  }

  # Return results.
  return(list(
    rep_error_mean = rep_error_mean, rep_error_sd = rep_error_sd,
    fold_error_means = fold_error, fold_error_sds = fold_sd,
    folds = pars$k, reps = pars$reps, time = toc
  ))
}
