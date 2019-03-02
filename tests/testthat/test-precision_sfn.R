context("test-precision_sfn")

test_that("q must be integer", {
  expect_error(precision_sfn(1.5))
})

test_that("q cannot be zero", {
  expect_error(precision_sfn(0))
})

test_that("q cannot be negative", {
  expect_error(precision_sfn(q = -1))
})

test_that("power cannot be zero", {
  expect_error(precision_sfn(q = 3, power = 0))
})

test_that("power cannot be less than zero", {
  expect_error(precision_sfn(q = 3, power = -10^6))
})

test_that("n_edge cannot be zero", {
  expect_error(precision_sfn(q = 3, n_edge = 0))
})

test_that("n_edge cannot be less than zero", {
  expect_error(precision_sfn(q = 3, n_edge = -10^6))
})

test_that("zero_appeal cannot be zero", {
  expect_error(precision_sfn(q = 3, zero_appeal = 0))
})

test_that("zero_appeal cannot be less than zero", {
  expect_error(precision_sfn(q = 3, zero_appeal = -10^6))
})


test_that("min_ev cannot be zero", {
  expect_error(precision_sfn(q = 3, min_ev = 0))
})

test_that("min_ev cannot be less than zero", {
  expect_error(precision_sfn(q = 3, min_ev = -10^6))
})

test_that("dimension of square output matrix is equal to q", {
  expect_equal(dim(precision_sfn(8)), c(8, 8))
})

test_that("output matrix is positive definite", {
  expect_true(matrixcalc::is.positive.definite(precision_sfn(8)))
})
