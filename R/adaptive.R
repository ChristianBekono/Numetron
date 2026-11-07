#' Adaptive Simpson integration
#'
#' Recursively subdivides intervals until the estimated local Simpson error
#' meets the requested tolerance or the maximum recursion depth is reached.
#'
#' @param f Vectorized function to integrate.
#' @param lower Lower integration bound.
#' @param upper Upper integration bound.
#' @param tol Requested absolute tolerance (positive finite number).
#' @param max_depth Maximum recursion depth.
#' @return A `numetron_result` object containing an estimated error and a
#'   convergence flag.
#' @examples
#' integrate_adaptive(function(x) exp(-x^2), 0, 1, tol = 1e-8)$estimate
#' @export
integrate_adaptive <- function(f, lower, upper, tol = 1e-8, max_depth = 20L) {
  z <- .prepare_interval(f, lower, upper)
  if (!is.numeric(tol) || length(tol) != 1L || !is.finite(tol) || tol <= 0)
    stop("`tol` must be one positive finite number.", call. = FALSE)
  max_depth <- .validate_n(max_depth, min = 1L)
  evals <- 0L
  simpson <- function(a, b, fa, fm, fb) (b - a) / 6 * (fa + 4 * fm + fb)

  recurse <- function(a, b, fa, fm, fb, whole, eps, depth) {
    m <- (a + b) / 2
    lm <- (a + m) / 2
    rm <- (m + b) / 2
    vals <- .eval_f(f, c(lm, rm))
    evals <<- evals + 2L
    flm <- vals[1L]; frm <- vals[2L]
    left <- simpson(a, m, fa, flm, fm)
    right <- simpson(m, b, fm, frm, fb)
    delta <- left + right - whole
    err <- abs(delta) / 15
    if (err <= eps) return(list(value = left + right + delta / 15,
                                error = err, converged = TRUE))
    if (depth >= max_depth)
      return(list(value = left + right + delta / 15,
                  error = err, converged = FALSE))
    L <- recurse(a, m, fa, flm, fm, left, eps / 2, depth + 1L)
    R <- recurse(m, b, fm, frm, fb, right, eps / 2, depth + 1L)
    list(value = L$value + R$value, error = L$error + R$error,
         converged = L$converged && R$converged)
  }

  a <- z$a; b <- z$b; m <- (a + b) / 2
  vals <- .eval_f(f, c(a, m, b))
  evals <- evals + 3L
  whole <- simpson(a, b, vals[1L], vals[2L], vals[3L])
  ans <- recurse(a, b, vals[1L], vals[2L], vals[3L], whole, tol, 0L)
  .make_result(z$sign * ans$value, "adaptive Simpson", NA_integer_,
               evals, ans$error, ans$converged)
}
