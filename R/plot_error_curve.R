#' Plot error curves
#'
#' Plots the estimated and true error curves for iterates of an
#' optimization algorithm using ggplot2.
#'
#'
#' @param est numeric vector representing the errors between iterates
#' and the final iterate
#' @param tru numeric vector representing the errors between iterates
#' and the ground truth
#' @param up upper limit of vertical axis
#' @param low lower limit of vertical axis
#' @param left left limit of horizontal axis
#' @param right right limit of horizontal axis
#' @param auto calculate limit automatically (default FALSE; if TRUE axes limits are ignored)
#'
#' @return A ggplot object.
#'
#' @note
#' See also \code{\link[tsmvr]{tsmvr_solve}} and
#' \code{\link{error_curve}}.
#'
#' @export
plot_error_curve <- function(est, tru, up = 1,
                             low = -5, left = 0,
                             right = 100, auto = T) {
  stopifnot(
    length(est) == length(tru),
    is.numeric(est), is.numeric(tru),
    up %% 1 == 0, low %% 1 == 0,
    left %% 1 == 0, right %% 1 == 0,
    up > low, left < right
  )

  k <- length(est)
  logest <- log2(est)
  logtru <- log2(tru)

  # # If auto == T, calculate automatic axes limits.
  # if (auto == T) {
  #   left <- 0
  #
  #   # Tricky one is the left horizontal axis limit. First find
  #   # where the tru error curve flattens within a tolerance "x",
  #   # and then plot some extra to the right of test (5 extra
  #   # iterations if x < 25 or 20% more if x > 25).
  #   tol <- 1e-3
  #   r.logical <- which((pracma::gradient(tru) < tol) == T)
  #   if (length(r.logical) == 0) {
  #     right <- length(tru)
  #   } else {
  #     x <- min(r.logical)
  #     if (x < 25) {
  #       right <- x + 5
  #     } else {
  #       right <- round(1.2 * x)
  #     }
  #   }
  #
  #   # Find general place of left-right axis limits, and then
  #   # scale so there is some scaled white space below and above
  #   # the two curves.
  #   slace <- 0.15
  #   y <- max(c(est[0], tru[0]))
  #   z <- min(c(est[right], tru[right]))
  #   if (y <= z) { # (enzure y > z, and if not flip)
  #     t <- y
  #     y <- z
  #     z <- t
  #   }
  #   if (y >= 0) {
  #     up <- (1 + scale) * y
  #   } else {
  #     up <- (1 - scale) * y
  #   }
  #   if (z <= 0) {
  #     low <- (1 + scale) * z
  #   } else {
  #     low <- (1 - scale) * z
  #   }
  # }

  # Coerce error curve vectors into dataframe for plotting.
  df <- as.data.frame(tru)
  df$est <- est
  names(df) <- c("True", "Estimated")
  invisible(capture.output(df <- reshape::melt(df)))
  df$iteration <- 1:k

  # Plot the error curves.
  plt <- ggplot2::ggplot(df, ggplot2::aes(
    x = iteration,
    y = value,
    colour = variable
  )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::xlab("t") +
    ggplot2::xlim(left, right) +
    ggplot2::ylab("log2 Frobenius norm") +
    ggplot2::ylim(low, up) +
    ggplot2::scale_color_manual(
      labels =
        c(
          expression(group("|", B^t - B^"*", "|")[F]),
          expression(group("|", B^t - est(B), "|")[F])
        ),
      values = c("blue", "red")
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = c(0.22, 0.25),
      legend.background = ggplot2::element_rect(
        color = "black",
        linetype = "solid", size = 0.3
      ),
      axis.text = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_text(size = 24, face = "bold"),
      legend.text = ggplot2::element_text(size = 20, face = "bold")
    )

  # Return the plot handle.
  return(plt)
}
