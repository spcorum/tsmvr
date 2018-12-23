context("test-squared_error")

test_that("U cannot be numeric", {
  expect_error(squared_error(U = 123432, V = matrix(1, 1, 1)))
})

test_that("U cannot be a string", {
  expect_error(squared_error(U = "asdf", V = matrix(1, 1, 1)))
})

test_that("U cannot be a vector", {
  expect_error(squared_error(U = c(1), V = matrix(1, 1, 1)))
})

test_that("U cannot be a list", {
  expect_error(squared_error(U = list(), V = matrix(1, 1, 1)))
})

test_that("V cannot numeric", {
  expect_error(squared_error(U = matrix(1, 1, 1), V = 123432))
})

test_that("V cannot be a vector", {
  expect_error(squared_error(U = matrix(1, 1, 1), V = c(1)))
})

test_that("V cannot a list", {
  expect_error(squared_error(U = matrix(1, 1, 1), V = list()))
})

test_that("V cannot be a string", {
  expect_error(squared_error(U = matrix(1, 1, 1), V = "asdf"))
})

test_that("Sigma cannot be numeric", {
  expect_error(squared_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = 123432
  ))
})

test_that("Sigma cannot be a string", {
  expect_error(squared_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = "asdf"
  ))
})

test_that("Sigma cannot be a vector", {
  expect_error(squared_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = c(1)
  ))
})

test_that("Sigma cannot be a list", {
  expect_error(squared_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = list()
  ))
})

test_that("dimensions of V and V must be equal", {
  expect_error(squared_error(
    U = matrix(1, 1, 1),
    V = matrix(c(1, 1, 1, 1), 2, 2)
  ))
})

test_that("3x3 example with identity covariance gives pencel and
          paper result", {
  expect_equal(
    squared_error(
      U = matrix(c(1, -1, -1, 1, 0, 0, 5, 3, 7), 3, 3),
      V = matrix(c(2, 10, sqrt(2), 0, -1, 1, 0.5, 2, -2), 3, 3)
    ),
    matrixcalc::matrix.trace(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3) %*%
      t(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3))) / 3 / 3
  )
})
