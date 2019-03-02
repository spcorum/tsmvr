#' Precision matrix of scale free network
#'
#' Constructs a scale free network (SFN) precision matrix according
#' to the Barabasi algorithm.
#'
#' @param q square dimension of covariance matrix (positive integer)
#' @param power power of the preferential attachment, (\code{a} > 0, default 1 or linear preferential attachment)
#' @param zero_appeal the ‘attractiveness’ of the vertices with no adjacent edges (\code{b} >0 0, default 1)
#' @param n_edge number of edges to add to the network per growth iteration (\code{n_edge} > 0, default 1)
#' @param min_ev minimum eigenvalue of resulting precision matrix (\code{min_ev} > 0, default 0.18)
#' @references
#' \insertRef{chen2016high}{tsmvr}
#'
#' \insertRef{barabasi}{tsmvr}
#'
#' @note
#' See also \code{\link[igraph]{get.adjacency}},
#' \code{\link[igraph]{barabasi.game}}, and
#' \code{\link[igraph]{sample_pa}}.
#'
#' @return Returns an FGN covariance matrix.
#' @export
precision_sfn <- function(q, power = 1, n_edge = 1, zero_appeal = 1,
                          min_ev = 0.18) {
  stopifnot(
    q %% 1 == 0, q > 0, power > 0, zero_appeal > 0, n_edge > 0,
    min_ev > 0
  )
  Sigma <- as.matrix(igraph::get.adjacency(igraph::barabasi.game(
    n = q, m = n_edge,
    power = power,
    zero.appeal = zero_appeal,
    directed = F,
    algorithm = "psumtree"
  )))
  min.lambda <- min(eigen(Sigma)$values)
  if (min.lambda <= 0) {
    Sigma <- Sigma +
      (abs(min.lambda) + min_ev) * diag(q)
  }
  return(Sigma)
}
