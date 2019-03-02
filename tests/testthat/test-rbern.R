context("test-rbern")

test_that("n cannot be negative", {
  expect_error(rbern(-1))
})

test_that("n cannot be zero", {
  expect_error(rbern(0))
})

test_that("p cannot be negative", {
  expect_error(rbern(1, -0.001))
})

test_that("p cannot be greater than unity", {
  expect_error(rbern(1, -0.001))
})

test_that("returned vector is of expected length", {
  expect_equal(length(rbern(10^3)), 10^3)
})

test_that("returned vector has entries of only zero or one", {
  expect_equal(sort(unique(rbern(10^3))), c(0, 1))
})

test_that("returned vector has entries of only zero when p=0", {
  expect_equal(unique(rbern(10^3, 0)), 0)
})

test_that("returned vector has entries of only zero when p=1", {
  expect_equal(unique(rbern(10^3, 1)), 1)
})
