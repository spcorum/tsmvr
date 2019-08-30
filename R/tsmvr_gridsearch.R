#' Gridsearch for tsmvr
#'
#' Calculates the mean and standard deviation of fold-averaged errors
#' derived from replicated tsmvr k-fold cross-validation experiments.
#' The error for each base model is the normalized squared
#' error between the true response and the predicted response
#' on a given cross-validation set.
#'
#' @param X design matrix (n-by-p)
#' @param Y response matrix (n-by-q)
#' @param s1_vec values for gridsearch (vector of integer valued numerics)
#' @param s2_vec values for gridsearch (vector of integer valued numerics)
#' @param k number of k-folds (integer valued numerics greater than 1)
#' @param reps number of replications (positive integer valued numeric)
#' @param pars list of algorithm parameters; output of \code{set_parameters}
#' @param seed set random seed (integer)
#'
#' @return A list containing the \code{mean} and \code{sd} of the
#' error over the replicates as well as the means and standard
#' deviations of the errors across each fold.
#'
#' @note See also \code{\link{squared_error}},
#' \code{\link{k_folds}},
#' \code{\link{tsmvr_solve}},
#' \code{\link{tsmvr_cv}},
#' \code{\link{tsmvr_replicate}}, and
#' \code{\link{set_parameters}}.
#'
# #' @export
tsmvr_gridsearch <- function(X, Y, s1_vec, s2_vec, pars, quiet = F, seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(X),
    is.numeric(Y), is.matrix(Y),
    dim(X)[1] == dim(Y)[1],
    is.numeric(s1_vec), s1_vec%%1 == 0,
    s1_vec >= 0, s1_vec <= dim(X)[2] * dim(Y)[2],
    is.numeric(s2_vec), s2_vec%%1 == 0,
    s2_vec >= dim(Y)[2], s2_vec <=(dim(Y)[2])^2,
    is.list(pars),
    is.null(seed) || is.numeric(seed)
  )

  # Initialize objects.
  m <- length(s1_vec)
  n <- length(s2_vec)
  error <- matrix(rep(0, m * n), m, n)
  error_sd <- matrix(rep(0, m * n), m, n)

  # Header
  if (!quiet) {
    cat("Solver mode ", pars$B_type, "-", pars$Omega_type, " with eta1 = ", pars$eta1, sep = "")
    if (pars$Omega_type == 'min') cat(".\n", sep = '')
    else cat(" and eta2 = ", pars$eta2,  ".\n", sep = '')
    cat("s1\ts2\terror\t\tsd\ttime\n")
  }

  # Iterate over reps.
  pars$quiet = T
  set.seed(seed)
  tic <- Sys.time()
  for (i in 1:m) {
    for (j in 1:n) {

      # For each grid point, perform replicated, cross-validated
      # tsmvr_solve.
      replicate_result <- tsmvr_replicate(
        X = X, Y = Y, s1 = s1_vec[i], s2 = s2_vec[j],
        pars = pars, quiet = T, seed = seed
      )

      # Record this result.
      error[i, j] <- replicate_result$rep_error_mean
      error_sd[i, j] <- replicate_result$rep_error_sd
      toc <- (Sys.time() - tic)

      # Print this result to screen.
      if (!quiet) {
        cat(s1_vec[i], "\t", s2_vec[j], "\t", error[i, j], "\t",
            error_sd[i, j], "\t", round(toc, 3), "\n", sep = ""
        )
      }
    }
  }

  # Compute final result.
  toc <- (Sys.time() - tic)[[1]]
  error_min <- min(error)
  error_min_idx <- which(error == min(error), arr.ind = T)
  error_min_sd <- error_sd[error_min_idx]
  s1_min <- s1_vec[error_min_idx[1]]
  s2_min <- s2_vec[error_min_idx[2]]
  solution <- tsmvr_solve(
    X = X, Y = Y, s1 = s1_min, s2 = s2_min, pars = pars
  )

  # Print final result to screen.
  if (!quiet) {
    cat("Minimum mean error = ", error_min, "\n", sep = "")
    cat("Sd at minimum error = ", error_min_sd, "\n", sep = "")
    cat("s1 = ", s1_min, ", s2 = ", s2_min, "\n", sep = "")
  }

  # Return result.
  return(list(
    solution = solution,
    error_min = error_min, error_min_sd = error_min_sd,
    s1_min = s1_min, s2_min = s2_min,
    s1_vec = s1_vec, s2_vec = s2_vec,
    folds = pars$k, reps = pars$reps,
    gs_time = toc
  ))
}
