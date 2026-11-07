#' Composite midpoint rule
#'
#' Approximates a definite integral by evaluating the integrand at each subinterval midpoint.
#'
#' Formula: `h * sum(f((x[i] + x[i + 1]) / 2))`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_midpoint(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_midpoint <- function(f, lower, upper, n = 100L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n)
  h <- (z$b - z$a) / n
  xmid <- z$a + ((0:(n - 1L)) + 0.5) * h
  y <- .eval_f(f, xmid)
  .make_result(z$sign * h * sum(y), "midpoint", n, length(xmid))
}
