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
#' @param epsilon convergence parameter (positive numeric)
#' @param max_iter maximum number of allowed iterations (positive integer)
#' @param quiet quiet mode (bool)
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
tsmvr_cv <- function(X, Y, s1, s2, k = 10, B_type = "gd",
                     Omega_type = "gd", eta1 = 0.01, eta2 = 0.01,
                     epsilon = 1e-4, max_iter = 2000, quiet = FALSE,
                     seed = NULL) {
  stopifnot(
    is.numeric(X), is.matrix(Y), is.numeric(Y), is.matrix(Y),
    is.numeric(s1), s1 >= 0, is.numeric(s2), s2 >= dim(Y)[2],
    s1 <= dim(X)[2] * dim(Y)[2], s2 <= (dim(Y)[2])^2,
    s1 %% 1 == 0, s2 %% 1 == 0,
    k %% 1 == 0, k > 1,
    is.character(B_type), is.character(Omega_type),
    B_type %in% c("gd"), Omega_type %in% c("gd", "min"),
    is.numeric(epsilon), epsilon > 0,
    is.numeric(max_iter), max_iter > 0, max_iter %% 1 == 0,
    is.null(seed) || is.numeric(seed)
  )

  error <- rep(0, k)

  # Compute folds.
  set.seed(seed)
  fold_list <- k_folds(nrow(X), k)

  # Compute fold results.
  for (i in 1:k) {
    X_train <- X[fold_list$train[[i]], ]
    Y_train <- Y[fold_list$train[[i]], ]
    X_val <- X[fold_list$val[[i]], ]
    Y_val <- Y[fold_list$val[[i]], ]

    B_hat <- tsmvr_solve(
      X = X_train, Y = Y_train, s1 = s1, s2 = s2, B_type = B_type,
      Omega_type = Omega_type, eta1 = eta1, eta2 = eta2,
      epsilon = epsilon, max_iter = max_iter, quiet = quiet
    )$B_hat

    Y_pred <- X_val %*% B_hat
    error[i] <- squared_error(Y_val, Y_pred)
  }

  # Compute results.
  return(list(error_mean = mean(error), error_sd = sd(error), num_folds = k))
}
