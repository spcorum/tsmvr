#' k-fold cross-validation for tsmvr
#'
#' Calculates the mean and standard deviation of errors
#' over a tsmvr k-fold cross-validation experiment.
#' The error for each base model is the normalized squared
#' error between the true response and the predicted response
#' on a given cross-validation set.
#'
#' @param X design matrix (n-by-p)
#' @param Y response matrix (n-by-q)
#' @param s1 regressor matrix sparsity (positive integer)
#' @param s2 covariance matrix sparsity (positive integer)
#' @param k number of k-folds (integer greater than 1)
#' @param pars list of algorithm parameters; output of \code{set_parameters}
#' @param quiet (logical)
#' @param seed set random seed (integer)
#'
#' @return A list of the mean and standard deviation of the errors
#' across the \code{K} folds.
#'
#' @note See also \code{\link{squared_error}},
#' \code{\link{k_folds}},
#' \code{\link{tsmvr_solve}}, and
#' \code{\link{set_parameters}}.
#'
#' @importFrom stats sd
#'
# #' @export
tsmvr_cv <- function(X, Y, s1, s2, pars, k = 10, quiet = F, seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(X),
    is.numeric(Y), is.matrix(Y),
    dim(X)[1] == dim(Y)[1],
    is.numeric(s1), s1%%1 == 0, s1 > 0, s1 <= dim(X)[2] * dim(Y)[2],
    is.numeric(s2), s2%%1 == 0, s2 > 0, s2 <= (dim(Y)[2])^2,
    is.list(pars),
    is.numeric(k), k%%1 == 0, k > 1, k <= dim(X)[1],
    is.logical(quiet),
    is.null(seed) || is.numeric(seed)
  )

  error <- rep(0, k)

  # Print header.
  if (!quiet) {
    cat("Solver mode ", pars$B_type, "-", pars$Omega_type, " with eta1 = ", pars$eta1, sep = "")
    if (pars$Omega_type == 'min') cat(".\n", sep = '')
    else cat(" and eta2 = ", pars$eta2,  ".\n", sep = '')
    cat("Fold\terror/p/q\t\ttime (s)\n")
  }

  # Compute folds.
  fold_list <- k_folds(nrow(X), k, seed)

  # For each fold, solve and compute result.
  tic <- Sys.time()
  pars$quiet = T
  for (i in 1:k) {
    X_train <- X[fold_list$train[[i]], ]
    Y_train <- Y[fold_list$train[[i]], ]
    X_val <- X[fold_list$val[[i]], ]
    Y_val <- Y[fold_list$val[[i]], ]

    B_hat <- tsmvr_solve(
      X = X_train, Y = Y_train, s1 = s1, s2 = s2, pars = pars
    )$B_hat

    Y_pred <- X_val %*% B_hat
    error[i] <- squared_error(Y_val, Y_pred)
    toc <- (Sys.time() - tic)

    # Print fold result to screen.
    if (!quiet) {
      cat(i, "\t", error[i], "\t", round(toc, 3), "\n", sep = "")
    }
  }
  toc <- (Sys.time() - tic)

  error_mean <- mean(error)
  error_sd <- sd(error)

  # Print final result to screen.
  if (!quiet) {
    cat("Fold mean = ", error_mean, "\n", sep = "")
    cat("Fold sd = ", error_sd, "\n", sep = "")
  }

  # Compute results.
  return(list(
    error_mean = error_mean, error_sd = error_sd,
    num_folds = k, time = toc
  ))
}
