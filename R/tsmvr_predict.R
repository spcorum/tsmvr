#' predicts a response from a tsmvr model
#'
#' Given a design matrix \code{X}, predicts a response using the
#' the model found by \code{tsmvr_solve}.
#'
#' @param tsmvr_list output list from tsmvr_solve
#' @param X the design matrix the response is predicted from
#'
#' @note \code{\link{tsmvr_solve}}.
#'
#' @export

tsmvr_predict = function(tsmvr_list, X) {
    stopifnot (
      is.list(tsmvr_list),
      length(tsmvr_list) == 11,
      names(tsmvr_list) == c('B_hat', 'Omega_hat', 'objective',
        'iterations', 'time', 'Y_hat', 'residuals',
        'sum_of_squares', 'B_history', 'Omega_history',
        'objective_history'),
      is.numeric(tsmvr_list$B_hat),
      is.matrix(tsmvr_list$B_hat),
      is.numeric(X),
      is.matrix(X),
      dim(X)[2] == dim(tsmvr_list$B_hat)[1]
    )
  return (X %*% tsmvr_list$B_hat)
}
