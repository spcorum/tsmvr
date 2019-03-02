context("test-regressor_matrix")

test_that("p is integer", {
  expect_error(regressor_matrix(p = 1.5, q = 1))
})

test_that("p cannot be zero", {
  expect_error(regressor_matrix(p = 0, q = 1))
})

test_that("p cannot be negative", {
  expect_error(regressor_matrix(p = -1, q = 1))
})

test_that("q is integer", {
  expect_error(regressor_matrix(p = 1, q = 1.5))
})

test_that("q cannot be zero", {
  expect_error(regressor_matrix(p = 1, q = 0))
})

test_that("q cannot be negative", {
  expect_error(regressor_matrix(p = 1, q = -1))
})

test_that("b1 cannot be negative", {
  expect_error(regressor_matrix(p = 1, q = 1, b1 = -10^-6, b2 = 0.5))
})

test_that("b1 cannot be greater than unity", {
  expect_error(regressor_matrix(p = 1, q = 1, b1 = 1 + 10^-6, b2 = 0.5))
})

test_that("b2 cannot be negative", {
  expect_error(regressor_matrix(p = 1, q = 1, b1 = 0.5, b2 = -10^-6))
})

test_that("b2 cannot be greater than unity", {
  expect_error(regressor_matrix(p = 1, q = 1, b1 = 0.5, b2 = 1 + 10^-6))
})

test_that("dimension of output matrix is p-by-q", {
  expect_equal(dim(regressor_matrix(p = 4, q = 11)), c(4, 11))
})
