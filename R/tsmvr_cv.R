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
#' @param suppress suppress maximum iteration warning (bool)
#' @param seed set random seed (integer)
#' @return A list of the mean and standard deviation of the errors
#' across the \code{K} folds.
#'
#' @note See also \code{\link{squared_error}}, \code{\link{k_folds}},
#' \code{\link{tsmvr_solve}}.
#'
#' @importFrom stats sd
#'
#' @export
tsmvr_cv <- function(X, Y, s1, s2, k = 10,
                     B_type = "ls", Omega_type = "ls",
                     eta1 = 0.05, eta2 = 0.2,
                     rho1 = 1e2, rho2 = 1,
                     beta1 = 0.5, beta2 = 0.5,
                     epsilon = 1e-3, max_iter = 2000,
                     quiet = FALSE, suppress = FALSE,
                     seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(Y), is.numeric(Y), is.matrix(Y),
    is.numeric(s1), s1 >= 0, is.numeric(s2), s2 >= dim(Y)[2],
    s1 <= dim(X)[2] * dim(Y)[2], s2 <= (dim(Y)[2])^2,
    s1 %% 1 == 0, s2 %% 1 == 0,
    k %% 1 == 0, k > 1,
    is.character(B_type), is.character(Omega_type),
    B_type %in% c("gd", "ls"), Omega_type %in% c("gd", "min", "ls"),
    is.numeric(epsilon), epsilon > 0,
    is.numeric(max_iter), max_iter > 0, max_iter %% 1 == 0,
    is.null(seed) || is.numeric(seed)
  )

  error <- rep(0, k)

  # Print header.
  if (!quiet) {
    if (Omega_type == "gd")
      cat("Solver mode 'gd-gd' with eta1 = ", eta1, " and eta2 = ", eta2, ".\n", sep ='')
    else if (Omega_type == "min")
      cat("Solver mode 'gd-min' with eta1 = ", eta1, ".\n", sep = '')
    cat('Fold\tError\t\ttime (s)\n')
  }

  # Compute folds.
  set.seed(seed)
  fold_list <- k_folds(nrow(X), k)

  # For each fold, solve and compute result.
  tic <- Sys.time()
  for (i in 1:k) {
    X_train <- X[fold_list$train[[i]], ]
    Y_train <- Y[fold_list$train[[i]], ]
    X_val <- X[fold_list$val[[i]], ]
    Y_val <- Y[fold_list$val[[i]], ]

    B_hat <- tsmvr_solve(
      X = X_train, Y = Y_train, s1 = s1, s2 = s2, B_type = B_type,
      Omega_type = Omega_type, eta1 = eta1, eta2 = eta2,
      rho1 = rho1, rho2 = rho2, beta1 = beta1, beta2 = beta2,
      epsilon = epsilon, max_iter = max_iter, quiet = T,
      suppress = suppress, skip = 1
    )$B_hat

    Y_pred <- X_val %*% B_hat
    error[i] <- squared_error(Y_val, Y_pred)
    toc <- (Sys.time() - tic)

    # Print fold result to screen.
    if (!quiet) {
      cat(i, '\t', error[i], '\t', round(toc,3), '\n', sep = '')
    }

  }
  toc <- (Sys.time() - tic)

  error_mean = mean(error)
  error_sd = sd(error)

  # Print final result to screen.
  if (!quiet) {
    cat('Fold mean = ', error_mean, '\n', sep = '')
    cat('Fold sd = ', error_sd, '\n', sep = '')
  }

  # Compute results.
  return(list(error_mean = error_mean, error_sd = error_sd,
              num_folds = k, time = toc))
}
