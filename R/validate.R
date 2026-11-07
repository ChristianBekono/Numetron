# Internal input validation helpers

.validate_integrand <- function(f) {
  if (!is.function(f)) stop("`f` must be a function.", call. = FALSE)
  invisible(TRUE)
}

.validate_bounds <- function(lower, upper) {
  if (!is.numeric(lower) || length(lower) != 1L || !is.finite(lower))
    stop("`lower` must be one finite number.", call. = FALSE)
  if (!is.numeric(upper) || length(upper) != 1L || !is.finite(upper))
    stop("`upper` must be one finite number.", call. = FALSE)
  if (lower == upper) stop("`lower` and `upper` must be different.", call. = FALSE)
  invisible(TRUE)
}

.validate_n <- function(n, min = 1L) {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
      n != as.integer(n) || n < min)
    stop(sprintf("`n` must be an integer greater than or equal to %d.", min),
         call. = FALSE)
  as.integer(n)
}

.eval_f <- function(f, x) {
  y <- tryCatch(f(x), error = function(e) {
    stop("Evaluation of `f` failed: ", conditionMessage(e), call. = FALSE)
  })
  if (!is.numeric(y) || length(y) != length(x) || any(!is.finite(y))) {
    stop("`f` must return a finite numeric vector with the same length as `x`.",
         call. = FALSE)
  }
  as.numeric(y)
}

.prepare_interval <- function(f, lower, upper) {
  .validate_integrand(f)
  .validate_bounds(lower, upper)
  sign <- if (upper > lower) 1 else -1
  a <- min(lower, upper)
  b <- max(lower, upper)
  list(a = a, b = b, sign = sign)
}

.make_result <- function(value, method, n = NA_integer_,
                         evaluations = NA_integer_, error_estimate = NA_real_,
                         converged = TRUE, details = list()) {
  structure(
    c(list(estimate = as.numeric(value), method = method, subdivisions = n,
           evaluations = evaluations, error_estimate = error_estimate,
           converged = converged), details),
    class = "numetron_result"
  )
}
