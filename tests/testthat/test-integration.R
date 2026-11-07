test_that("basic rules approximate x squared integral", {
  f <- function(x) x^2
  expect_equal(integrate_left(f, 0, 1, 1000)$estimate, 1/3, tolerance = 0.001)
  expect_equal(integrate_right(f, 0, 1, 1000)$estimate, 1/3, tolerance = 0.001)
  expect_equal(integrate_midpoint(f, 0, 1, 100)$estimate, 1/3, tolerance = 1e-4)
  expect_equal(integrate_trapezoid(f, 0, 1, 100)$estimate, 1/3, tolerance = 1e-4)
})

test_that("Simpson methods are exact for low-degree polynomials", {
  f <- function(x) x^2 + 2*x
  expect_equal(integrate_simpson_13(f, 0, 2, n = 4)$estimate, 20/3,
               tolerance = 1e-12)
  expect_equal(integrate_simpson_38(f, 0, 2, n = 3)$estimate, 20/3,
               tolerance = 1e-12)
})

test_that("Gaussian quadrature integrates polynomials within degree", {
  expect_equal(integrate_gauss_legendre(function(x) x^3, -1, 1, 2)$estimate,
               0, tolerance = 1e-12)
  expect_equal(integrate_gauss_legendre(function(x) x^4, -1, 1, 3)$estimate,
               2/5, tolerance = 1e-12)
})

test_that("Romberg and adaptive Simpson converge", {
  expect_equal(integrate_romberg(function(x) x^2, 0, 1, levels = 4)$estimate,
               1/3, tolerance = 1e-12)
  expect_equal(integrate_adaptive(function(x) exp(-x^2), 0, 1,
                                  tol = 1e-8)$estimate,
               integrate(function(x) exp(-x^2), 0, 1)$value,
               tolerance = 1e-7)
})

test_that("invalid Simpson subdivision counts are rejected", {
  expect_error(integrate_simpson_13(function(x) x, 0, 1, n = 3))
  expect_error(integrate_simpson_38(function(x) x, 0, 1, n = 4))
})

test_that("reversed bounds change the sign", {
  expect_equal(integrate_trapezoid(function(x) x, 1, 0, n = 10)$estimate,
               -0.5, tolerance = 1e-12)
})
