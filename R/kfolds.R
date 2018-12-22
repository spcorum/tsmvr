#' k-fold cross-validation indices
#'
#' Produces k-fold partitions of training and validation indices for
#' a dataset.
#'
#' @param n number of data points (positive integer)
#' @param K number of folds (integer K >= 2)
#' @param seed sets random seed for reproducibility (positive integer)
#' @return A list of length two, each entry itself being a list of
#' length \code{K} containing the training (validation) indices.
#'
#' @note See also \code{\link{tsmvr_cv}}.
#'
#' @export
kfolds <- function(n, K, seed = NULL) {

  # Create random permutation of the data indices.
  set.seed(seed)
  full_list <- sample.int(n)

  # Calculate the cv-fold indeces by making K evenly sized splits of
  # the permuted indices.
  cv_list <- split(full_list, ceiling(seq_along(full_list) / (n / K)))
  names(cv_list) <- NULL

  # Use set difference to calculate the training indices for each
  # fold: set(train) = set(full) - set(cv).
  train_list <- NULL
  for (k in 1:K) {
    train_list <- c(train_list, list(setdiff(full_list, cv_list[[k]])))
  }

  # Return the lists of training and cv indices as a named list.
  return(list(train = train_list, val = cv_list))
}
