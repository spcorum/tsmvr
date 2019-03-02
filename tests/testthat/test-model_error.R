context("test-model_error")

test_that("U cannot be some number", {
  expect_error(model_error(U = 123432, V = matrix(1, 1, 1)))
})

test_that("U cannot be some string number", {
  expect_error(model_error(U = "asdf", V = matrix(1, 1, 1)))
})

test_that("V cannot be some number", {
  expect_error(model_error(U = matrix(1, 1, 1), V = 123432))
})

test_that("V cannot be some string number", {
  expect_error(model_error(U = matrix(1, 1, 1), V = "asdf"))
})

test_that("Sigma cannot be some number", {
  expect_error(model_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = 123432
  ))
})

test_that("Sigma cannot be some string number", {
  expect_error(model_error(
    U = matrix(1, 1, 1), V = matrix(1, 1, 1),
    Sigma = "asdf"
  ))
})

test_that("dimensions of V and V must be equal", {
  expect_error(model_error(
    U = matrix(1, 1, 1),
    V = matrix(c(1, 1, 1, 1), 2, 2)
  ))
})

test_that("verticle dimension of V matches dimension of
          -Sigma must be equal", {
  expect_error(model_error(
    V = matrix(rep(1, 6), 3, 2),
    U = matrix(rep(1, 6), 3, 2),
    Sigma = matrix(rep(1, 4), 2, 2)
  ))
})


test_that("Sigma cannot be positive semi-definite", {
  expect_error(model_error(
    U = matrix(rep(1, 4), 2, 2),
    V = matrix(rep(1, 4), 2, 2),
    Sigma = matrix(rep(1, 4), 2, 2)
  ))
})

test_that("Sigma cannot be negative semi-definite", {
  expect_error(model_error(
    U = matrix(rep(1, 4), 2, 2),
    V = matrix(rep(1, 4), 2, 2),
    Sigma = matrix(rep(-1, 4), 2, 2)
  ))
})

test_that("Sigma cannot be indefinite", {
  expect_error(model_error(
    U = matrix(rep(1, 4), 2, 2),
    V = matrix(rep(1, 4), 2, 2),
    Sigma = matrix(rep(-1, 4), 2, 2)
  ))
})

test_that("Sigma cannot be negative definite", {
  expect_error(model_error(
    U = matrix(rep(1, 4), 2, 2),
    V = matrix(rep(1, 4), 2, 2),
    Sigma = -diag(2)
  ))
})

test_that("3x3 example with identity covariance gives pencel and
           paper result", {
  expect_equal(
    model_error(
      U = matrix(c(1, -1, -1, 1, 0, 0, 5, 3, 7), 3, 3),
      V = matrix(c(2, 10, sqrt(2), 0, -1, 1, 0.5, 2, -2), 3, 3)
    ),
    matrixcalc::matrix.trace(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3) %*%
      t(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3))) / 3 / 3
  )
})

test_that("3x3 example with AR1 covariance gives pencel and
           paper result", {
  expect_equal(
    model_error(
      U = matrix(c(1, -1, -1, 1, 0, 0, 5, 3, 7), 3, 3),
      V = matrix(c(2, 10, sqrt(2), 0, -1, 1, 0.5, 2, -2), 3, 3),
      Sigma = covar_ar1(q = 3, rho = 0.7)
    ),
    matrixcalc::matrix.trace(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3) %*%
      matrix(c(1, 0.7, 0.49, 0.7, 1, 0.7, 0.49, 0.7, 1), 3, 3) %*%
      t(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1, -4.5, -1, -9), 3, 3))) / 3 / 3
  )
})

test_that("3x2 example with AR1 covariance gives pencel and
           paper result", {
  expect_equal(
    model_error(
      U = matrix(c(1, -1, -1, 1, 0, 0), 3, 2),
      V = matrix(c(2, 10, sqrt(2), 0, -1, 1), 3, 2),
      Sigma = covar_ar1(q = 2, rho = 0.7)
    ),
    matrixcalc::matrix.trace(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1), 3, 2) %*%
      matrix(c(1, 0.7, 0.7, 1), 2, 2) %*%
      t(matrix(c(2 - 1, 10 + 1, sqrt(2) + 1, -1, -1, 1), 3, 2))) / 3 / 2
  )
})
