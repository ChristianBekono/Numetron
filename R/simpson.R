#' Composite Simpson 1/3 rule
#'
#' Uses quadratic interpolation on pairs of subintervals. `n` must be even.
#'
#' Formula: `h / 3 * (y[1] + y[n + 1] + 4 * sum(odd y) + 2 * sum(interior even y))`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_simpson_13(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_simpson_13 <- function(f, lower, upper, n = 100L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n, min = 2L)
  if (n %% 2L != 0L) stop("Simpson 1/3 requires an even `n`.", call. = FALSE)
  h <- (z$b - z$a) / n
  x <- seq(z$a, z$b, length.out = n + 1L)
  y <- .eval_f(f, x)
  odd <- seq.int(2L, n, by = 2L)
  even <- if (n >= 4L) seq.int(3L, n - 1L, by = 2L) else integer()
  value <- h / 3 * (y[1L] + y[n + 1L] +
                      4 * sum(y[odd]) + 2 * sum(y[even]))
  .make_result(z$sign * value, "Simpson 1/3", n, length(x))
}

#' Composite Simpson 3/8 rule
#'
#' Uses cubic interpolation on groups of three subintervals. `n` must be a multiple of three.
#'
#' Formula: `3 * h / 8 * weighted_sum`
#'
#' @param f Vectorized function to integrate. It must accept a numeric vector
#'   and return a numeric vector of the same length.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param n Number of equal subintervals.
#' @return An object of class `numetron_result`; the numerical value is in
#'   `$estimate`.
#' @examples
#' integrate_simpson_38(function(x) x^2, 0, 1, n = 10)$estimate
#' @export
integrate_simpson_38 <- function(f, lower, upper, n = 99L) {
  z <- .prepare_interval(f, lower, upper)
  n <- .validate_n(n, min = 3L)
  if (n %% 3L != 0L) stop("Simpson 3/8 requires `n` to be a multiple of 3.",
                           call. = FALSE)
  h <- (z$b - z$a) / n
  x <- seq(z$a, z$b, length.out = n + 1L)
  y <- .eval_f(f, x)
  idx <- if (n > 1L) seq_len(n - 1L) else integer()
  interior <- y[2:n]
  weights <- ifelse(idx %% 3L == 0L, 2, 3)
  value <- 3 * h / 8 * (y[1L] + y[n + 1L] + sum(weights * interior))
  .make_result(z$sign * value, "Simpson 3/8", n, length(x))
}
