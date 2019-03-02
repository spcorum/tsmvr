context("test-covariance_matrix")

test_that("q must be integer", {
  expect_error(covariance_matrix(q = 1.5))
})

test_that("q cannot be zero", {
  expect_error(covariance_matrix(q = 0))
})

test_that("q cannot be negative", {
  expect_error(covariance_matrix(q = -1))
})

test_that("type cannot be some arbitrary string like 'asdf'", {
  expect_error(covariance_matrix(q = 3, type = "asdf"))
})

test_that("rho cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, rho = -0.001))
})

test_that("rho cannot be unity", {
  expect_error(covariance_matrix(q = 3, rho = 1))
})

test_that("rho cannot be greater than unity", {
  expect_error(covariance_matrix(q = 3, rho = 1.001))
})

test_that("h cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, h = -0.001))
})

test_that("h cannot be unity", {
  expect_error(covariance_matrix(q = 3, h = 1))
})

test_that("h cannot be greater than unity", {
  expect_error(covariance_matrix(q = 3, h = 1.001))
})

test_that("power cannot be zero", {
  expect_error(covariance_matrix(q = 3, power = 0))
})

test_that("power cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, power = -10^6))
})

test_that("n_edge cannot be zero", {
  expect_error(covariance_matrix(q = 3, n_edge = 0))
})

test_that("n_edge cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, n_edge = -10^6))
})

test_that("zero_appeal cannot be zero", {
  expect_error(covariance_matrix(q = 3, zero_appeal = 0))
})

test_that("zero_appeal cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, zero_appeal = -10^6))
})

test_that("min_ev cannot be zero", {
  expect_error(covariance_matrix(q = 3, min_ev = 0))
})

test_that("min_ev cannot be less than zero", {
  expect_error(covariance_matrix(q = 3, min_ev = -10^6))
})


test_that("object returned has length of 2", {
  expect_equal(length(covariance_matrix(q = 10)), 2)
})

test_that("returned object is a list", {
  expect_true(is.list(covariance_matrix(q = 3)))
})

test_that("returned list's names are c('covariance', 'precision')", {
  expect_equal(
    names(covariance_matrix(q = 3)),
    c("covariance", "precision")
  )
})

test_that("dimension of covariance matrix list element is equal to q", {
  expect_equal(dim(covariance_matrix(q = 10)$covariance), c(10, 10))
})

test_that("dimension of precision matrix list element is equal to q", {
  expect_equal(dim(covariance_matrix(q = 10)$precision), c(10, 10))
})

test_that("output matrix is identity when rho is zero", {
  expect_equal(
    covariance_matrix(q = 8, type = "AR1", rho = 0)$covariance,
    diag(8)
  )
})

test_that("2x2 AR1 output matrix is equal to pen and paper calculation", {
  expect_equal(
    covariance_matrix(
      q = 2, type = "AR1",
      rho = 0.7
    )$covariance,
    matrix(c(1, 0.7, 0.7, 1), 2, 2)
  )
})

test_that("3x3 AR1 output matrix is equal to pen and paper calculation", {
  expect_equal(
    covariance_matrix(q = 3, type = "AR1", rho = 0.7)$covariance,
    matrix(c(1, 0.7, 0.49, 0.7, 1, 0.7, 0.49, 0.7, 1), 3, 3)
  )
})

test_that("FGN output covariance matrix is identity when h is zero", {
  expect_equal(covariance_matrix(q = 10, type = "FGN", h = 0)$covariance, diag(10))
})

test_that("2x2 FGN output covarince matrix is equal to pen and paper calculation", {
  expect_equal(
    covariance_matrix(q = 2, type = "FGN", h = 0.9)$covariance,
    matrix(c(
      2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9,
      (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2
    ), 2, 2) / 2
  )
})

test_that("3x3 FGN output covariance matrix is equal to pen and paper calculation", {
  expect_equal(
    covariance_matrix(q = 3, type = "FGN", h = 0.9)$covariance,
    matrix(
      c(
        2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, (3^2)^0.9 - 2 * (2^2)^0.9 + (1^2)^0.9,
        (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9,
        (3^2)^0.9 - 2 * (2^2)^0.9 + (1^2)^0.9, (2^2)^0.9 - 2 * (1^2)^0.9 - (0^2)^0.9, 2
      ), 3, 3
    ) / 2
  )
})

test_that("AR1 output covariance matrix is positive definite", {
  expect_true(matrixcalc::is.positive.definite(
    covariance_matrix(q = 8, type = "AR1")$covariance
  ))
})

test_that("FGN output covariance matrix is positive definite", {
  expect_true(matrixcalc::is.positive.definite(
    covariance_matrix(q = 8, type = "FGN")$covariance
  ))
})

test_that("SFN output covariance matrix is positive definite", {
  expect_true(matrixcalc::is.positive.definite(
    covariance_matrix(q = 8, type = "SFN")$covariance
  ))
})

test_that("AR1 output covariance and precision matrices are inverses of each other", {
  expect_equal(covariance_matrix(q = 8, type = "AR1")$precision,
    solve(covariance_matrix(q = 8, type = "AR1")$covariance),
    tolerance = 10^-6
  )
})

test_that("FGN output covariance and precision matrices are inverses of each other", {
  expect_equal(covariance_matrix(q = 8, type = "FGN")$precision,
    solve(covariance_matrix(q = 8, type = "FGN")$covariance),
    tolerance = 10^-6
  )
})
