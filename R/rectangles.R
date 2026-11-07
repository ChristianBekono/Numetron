#' Composite left-rectangle rule
#'
#' Approximates a definite integral using the left endpoint of each subinterval.
#'
#' Formula: `h * sum(f(x[1:n]))`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_left(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_left <- function(f, lower, upper, n = 100L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n)
  h <- (z$b - z$a) / n
  x <- z$a + (0:(n - 1L)) * h
  y <- .eval_f(f, x)
  .make_result(z$sign * h * sum(y), "left rectangles", n, length(x))
}

#' Composite right-rectangle rule
#'
#' Approximates a definite integral using the right endpoint of each subinterval.
#'
#' Formula: `h * sum(f(x[2:(n + 1)]))`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_right(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_right <- function(f, lower, upper, n = 100L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n)
  h <- (z$b - z$a) / n
  x <- z$a + (1:n) * h
  y <- .eval_f(f, x)
  .make_result(z$sign * h * sum(y), "right rectangles", n, length(x))
}
