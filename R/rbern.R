#' Bernoulli random number generator.
#'
#' Random number generation from the Bernoulli distribution.
#'
#' @param n number of draws
#' @param p Bernoulli parameter
#' @return Returns a vector of length \code{n} with each entry drawn
#' iid. from Bern(\eqn{p}).
#' @export
rbern <- function(n, p = 0.5) {
  stopifnot(n > 0, n %% 1 == 0, p >= 0, p <= 1)
  return(1 * (stats::runif(n) < p))
}
