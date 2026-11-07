#' Gauss-Legendre quadrature
#'
#' Approximates an integral using Gauss-Legendre nodes and weights. The
#' implementation supports 2- and 3-point rules.
#'
#' @param f Vectorized function to integrate.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param points Number of Gauss points: 2 or 3.
#' @return A `numetron_result` object.
#' @examples
#' integrate_gauss_legendre(function(x) x^2, 0, 1, points = 2)$estimate
#' @export
integrate_gauss_legendre <- function(f, lower, upper, points = 2L) {
  z <- .prepare_interval(f, lower, upper)
  points <- .validate_n(points, min = 2L)
  if (!points %in% c(2L, 3L))
    stop("`points` must be either 2 or 3.", call. = FALSE)
  if (points == 2L) {
    nodes <- c(-1, 1) / sqrt(3)
    weights <- c(1, 1)
  } else {
    nodes <- c(-sqrt(3/5), 0, sqrt(3/5))
    weights <- c(5/9, 8/9, 5/9)
  }
  x <- (z$b - z$a) / 2 * nodes + (z$a + z$b) / 2
  y <- .eval_f(f, x)
  value <- (z$b - z$a) / 2 * sum(weights * y)
  .make_result(z$sign * value, paste0("Gauss-Legendre (", points, " points)"),
               points, length(x))
}
