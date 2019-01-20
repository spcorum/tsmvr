#' k-fold cross-validation indices
#'
#' Produces k-fold partitions of training and validation indices for
#' a dataset.
#'
#' @param n number of data points (integer valued numeric > 1)
#' @param k number of k-folds (integer valued numeric > 1)
#' @param seed sets random seed for reproducibility (numeric)
#' @return A list of length two, each entry itself being a list of
#' length \code{K} containing the training (validation) indices.
#'
#' @note See also \code{\link{tsmvr_cv}}.
#'
#' @export
k_folds <- function(n, k, seed = NULL) {
  stopifnot(
    is.numeric(n), is.numeric(k), is.numeric(seed) || is.null(seed),
    n %% 1 == 0, n > 1, k %% 1 == 0, k > 1
  )

  # Create random permutation of the data indices.
  set.seed(seed)
  full_list <- sample.int(n)

  # Calculate the cv-fold indices by making K evenly sized splits of
  # the permuted indices.
  cv_list <- split(full_list, ceiling(seq_along(full_list) / (n / k)))
  names(cv_list) <- NULL

  # Use set difference to calculate the training indices for each
  # fold: set(train) = set(full) - set(cv).
  train_list <- NULL
  for (i in 1:k) {
    train_list <- c(train_list, list(setdiff(full_list, cv_list[[i]])))
  }

  # Return the lists of training and cv indices as a named list.
  return(list(train = train_list, val = cv_list))
}
