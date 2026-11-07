#' Composite trapezoidal rule
#'
#' Approximates a definite integral by summing the areas of trapezoids.
#'
#' Formula: `h / 2 * (f(x[1]) + 2 * sum(f(x[2:n])) + f(x[n + 1]))`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_trapezoid(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_trapezoid <- function(f, lower, upper, n = 100L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n)
  h <- (z$b - z$a) / n
  x <- seq(z$a, z$b, length.out = n + 1L)
  y <- .eval_f(f, x)
  interior <- if (n > 1L) sum(y[2:n]) else 0
  value <- h / 2 * (y[1L] + 2 * interior + y[n + 1L])
  .make_result(z$sign * value, "trapezoidal", n, length(x))
}
