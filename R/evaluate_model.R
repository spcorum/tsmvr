#' Evaluate a sparse model
#'
#' Calculates evaluation metrics between estimated and ground truth
#' sparse models (i.e., matrices)
#'
#' @param Star matrix of true values (m-by-n)
#' @param Hat matrix of fitted values (m-by-n)
#' @param Sigma positive definite covariance matrix (m-by-m)
#' @return A list of evaluation metrics, including: \cr \cr
#' \code{se}, squared error \cr
#' \code{me}, model error \cr
#' \code{tp}, number of true positives \cr
#' \code{tn}, number of true negatives \cr
#' \code{fp}, number of false positives \cr
#' \code{fn}, number of false negatives \cr
#' \code{tpr}, true positive rate \cr
#' \code{tnr}, true negative rate \cr
#' \code{acc}, accuracy \cr
#' \code{auc}, Area Under Curve \cr
#' \code{f1}, F1 score \cr
#'
#' @note
#' Also see \code{\link{model_error}}.
#'
#' @export
evaluate_model <- function(Star, Hat, Sigma = NULL) {
  stopifnot(
    is.matrix(Star), is.matrix(Hat), is.null(Sigma) ||
      is.matrix(Sigma), dim(Star) == dim(Hat),
    is.null(Sigma) || (dim(Star)[2] == dim(Sigma)[1])
  )
  se <- model_error(Star, Hat)
  me <- model_error(Star, Hat, Sigma)
  p <- sum(Star != 0)
  n <- sum(Star == 0)
  tp <- sum(Star != 0 & Hat != 0)
  tn <- sum(Star == 0 & Hat == 0)
  fp <- sum(Star == 0 & Hat != 0)
  fn <- sum(Star != 0 & Hat == 0)
  tpr <- tp / p
  tnr <- tn / n
  fpr <- fp / p
  fnr <- fn / n
  f1 <- 2 * tp / (2 * tp + fp + fn)
  acc <- (tp + tn) / (tp + tn + fp + fn)
  auc <- (pROC::roc(
    response = as.vector((Hat != 0) * 1),
    predictor = as.vector((Star != 0) * 1)
  )$auc) * 1
  return(list(
    squared_error = se, model_error = me, p = p, n = n, tp = tp, tn = tn,
    fp = fp, fn = fn, tpr = tpr, tnr = tnr, fpr = fpr, fnr = fnr, acc = acc,
    auc = auc, f1 = f1
  ))
}
