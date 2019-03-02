context("test-covar_fgn")

test_that("q must be integer", {
  expect_error(covar_fgn(1.5))
})

test_that("q cannot be zero", {
  expect_error(covar_fgn(0))
})

test_that("q cannot be negative", {
  expect_error(covar_fgn(-1, 0.7))
})

test_that("h cannot be less than zero", {
  expect_error(covar_fgn(1, -0.001))
})

test_that("h cannot be unity", {
  expect_error(covar_fgn(1, 1))
})

test_that("h cannot be greater than unity", {
  expect_error(covar_fgn(1, 1.001))
})

test_that("dimension of square output matrix is equal to q", {
  expect_equal(dim(covar_fgn(10, 0.9)), c(10, 10))
})

test_that("output matrix is identity when h is zero", {
  expect_equal(covar_fgn(10, h = 0), diag(10))
})

test_that("2x2 output matrix is equal to pen and paper calculation", {
  expect_equal(
    covar_fgn(2, h = 0.9),
    matrix(c(
      2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9,
      (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2
    ), 2, 2) / 2
  )
})

test_that("3x3 output matrix is equal to pen and paper calculation", {
  expect_equal(
    covar_fgn(3, h = 0.9),
    matrix(
      c(
        2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, (3^2)^0.9 - 2 * (2^2)^0.9 + (1^2)^0.9,
        (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9,
        (3^2)^0.9 - 2 * (2^2)^0.9 + (1^2)^0.9, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2
      ), 3, 3
    ) / 2
  )
})

test_that("8x8 output matrix with h=0.9 is positive definite", {
  expect_true(matrixcalc::is.positive.definite(covar_fgn(8, h = 0.9)))
})

test_that("8x8 output matrix with h=0 is positive definite", {
  expect_true(matrixcalc::is.positive.definite(covar_fgn(8, h = 0)))
})
