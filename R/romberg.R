#' Romberg integration
#'
#' Builds a Romberg table from successively refined trapezoidal estimates and
#' Richardson extrapolation.
#'
#' @param f Vectorized function to integrate.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param levels Number of refinement levels (at least 1).
#' @return A `numetron_result` object. The full extrapolation table is stored
#'   in `$table`.
#' @examples
#' integrate_romberg(function(x) x^2, 0, 1, levels = 4)$estimate
#' @export
integrate_romberg <- function(f, lower, upper, levels = 5L) {
  z <- .prepare_interval(f, lower, upper)
  levels <- .validate_n(levels, min = 1L)
  if (levels > 20L) stop("`levels` must not exceed 20.", call. = FALSE)
  tab <- matrix(NA_real_, nrow = levels, ncol = levels)
  evals <- 0L
  for (k in seq_len(levels)) {
    n <- 2^(k - 1L)
    h <- (z$b - z$a) / n
    x <- seq(z$a, z$b, length.out = n + 1L)
    y <- .eval_f(f, x)
    evals <- evals + length(x)
    interior <- if (n > 1L) sum(y[2:n]) else 0
    tab[k, 1L] <- h / 2 * (y[1L] + 2 * interior + y[n + 1L])
    if (k >= 2L) {
      for (j in 2:k) {
        tab[k, j] <- tab[k, j - 1L] +
          (tab[k, j - 1L] - tab[k - 1L, j - 1L]) / (4^(j - 1L) - 1)
      }
    }
  }
  value <- z$sign * tab[levels, levels]
  err <- if (levels >= 2L) abs(tab[levels, levels] - tab[levels - 1L, levels - 1L]) else NA_real_
  .make_result(value, "Romberg", 2^(levels - 1L), evals, err,
               details = list(table = tab))
}
