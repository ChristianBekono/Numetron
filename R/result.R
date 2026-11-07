#' Calculate integration error
#'
#' @param exact Exact value of the integral.
#' @param approximation Numerical approximation.
#' @return A named numeric vector containing absolute, relative, and percentage
#'   errors. Relative errors are `NA` when the exact value is zero.
#' @export
integration_error <- function(exact, approximation) {
  if (!is.numeric(exact) || length(exact) != 1L || !is.finite(exact))
    stop("`exact` must be one finite number.", call. = FALSE)
  if (!is.numeric(approximation) || length(approximation) != 1L ||
      !is.finite(approximation))
    stop("`approximation` must be one finite number.", call. = FALSE)
  absolute <- abs(exact - approximation)
  relative <- if (exact == 0) NA_real_ else absolute / abs(exact)
  c(absolute = absolute, relative = relative, percentage = 100 * relative)
}

#' @export
print.numetron_result <- function(x, ...) {
  cat("<numetron_result>\\n")
  cat("  Method:       ", x$method, "\\n", sep = "")
  cat("  Estimate:     ", format(x$estimate, digits = 12), "\\n", sep = "")
  if (!is.na(x$subdivisions))
    cat("  Subdivisions: ", x$subdivisions, "\\n", sep = "")
  if (!is.na(x$evaluations))
    cat("  Evaluations:  ", x$evaluations, "\\n", sep = "")
  if (!is.na(x$error_estimate))
    cat("  Error est.:   ", format(x$error_estimate, digits = 6), "\\n", sep = "")
  cat("  Converged:    ", x$converged, "\\n", sep = "")
  invisible(x)
}
