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
#' @param s1_grid values for gridsearch (vector of integer valued numerics)
#' @param s2_grid values for gridsearch (vector of integer valued numerics)
#' @param k number of k-folds (integer greater than 1)
#' @param reps number of replications (positive integer)
#' @param B_type B-step descent type (string: 'gd')
#' @param Omega_type Omega-step descent type (string: 'gd' or 'min')
#' @param eta1 B-step learning rate (positive numeric)
#' @param eta2 Omega-step learning rate (positive numeric)
#' @param rho1 B-step lineserach convergence parameter (positive numeric)
#' @param rho2 Omega-step lineserach convergence parameter  (positive numeric)
#' @param beta1 B-step linesearch shrinkage parameter (positive numeric)
#' @param beta2 Omega-step linesearch shrinkage parameter (positive numeric)
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
tsmvr_gridsearch <- function(X, Y, s1_grid, s2_grid,
                             k = 10, reps = 10,
                             B_type = "ls", Omega_type = "ls",
                             eta1 = 0.05, eta2 = 0.2,
                             rho1 = 1e2, rho2=1,
                             beta1 = 0.5, beta2 = 0.5,
                             epsilon = 1e-5, max_iter = 40000,
                             quiet = FALSE, seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(Y), is.numeric(Y), is.matrix(Y),
    is.numeric(s1_grid), s1_grid >= 0,
    is.numeric(s2_grid), s2_grid >= dim(Y)[2],
    s1_grid <= dim(X)[2] * dim(Y)[2], s2_grid <= (dim(Y)[2])^2,
    s1_grid %% 1 == 0, s2_grid %% 1 == 0,
    k %% 1 == 0, k > 1, reps %% 1 == 0, reps > 0,
    is.character(B_type), is.character(Omega_type),
    B_type %in% c("gd", "ls"), Omega_type %in% c("gd", "min", "ls"),
    is.numeric(epsilon), epsilon > 0,
    is.numeric(max_iter), max_iter > 0, max_iter %% 1 == 0,
    is.null(seed) || is.numeric(seed)
  )

  # Initialize objects.
  m = length(s1_grid)
  n = length(s2_grid)
  error = matrix(rep(0,m*n),m,n)
  error_sd = matrix(rep(0,m*n),m,n)

  # Header
  if (!quiet) {
    if (Omega_type == "gd")
      cat("Solver mode 'gd-gd' with eta1 = ", eta1, " and eta2 = ", eta2, ".\n", sep ='')
    else if (Omega_type == "min")
      cat("Solver mode 'gd-min' with eta1 = ", eta1, ".\n", sep = '')
    cat("s1\ts2\terror\t\ttime (s)\n")
  }

  # Iterate over reps.
  set.seed(seed)
  tic <- Sys.time()
  for (i in 1:m) {
    for (j in 1:n) {

      # For each grid point, perform replicated, cross-validated
      # tsmvr_solve.
      replicate_result <- tsmvr_replicate(
        X = X, Y = Y, s1 = s1_grid[i], s2 = s2_grid[j],
        k = k, reps = reps,
        B_type = B_type, Omega_type = Omega_type,
        eta1 = eta1, eta2 = eta2,
        epsilon = epsilon, max_iter = max_iter,
        quiet = T, seed = seed
      )

      # Record this result.
      error[i,j] <- replicate_result$rep_error_mean
      error_sd[i,j] <- replicate_result$rep_error_sd
      toc <- (Sys.time() - tic)

      # Print this result to screen.
      if (!quiet) {
        cat(s1_grid[i], "\t", s2_grid[j], "\t", error[i,j], "\t",
            round(toc, 3), "\n", sep = ""
        )
      }
    }
  }

  # Compute final result.
  toc <- (Sys.time() - tic)
  error_min = min(error)
  error_min_idx = which(error == min(error), arr.ind = T)
  error_min_sd = sd(error_min_idx)
  s1_min = s1_grid[error_min_idx[1]]
  s2_min = s2_grid[error_min_idx[2]]

  # Print final result to screen.
  if (!quiet) {
    cat('Minimum mean error = ', error_min, '\n', sep = '')
    cat('Sd at minimum error = ', error_min_sd, '\n', sep = '')
    cat('s1 = ', s1_min, ', s2 = ', s2_min, '\n', sep = '')
    # cat('s1 = ', s1_min, '\n', sep = '')
    # cat('s2 = ', s2_min, '\n', sep = '')
  }

  # Return result.
  return(list(
    error_min = error_min, error_min_sd = error_min_sd,
    s1_min = s1_min, s2_min = s2_min,
    error = error, error_sd = error_sd, time = toc
  ))
}
