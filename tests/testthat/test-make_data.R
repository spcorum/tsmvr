context("test-make_data")

test_that("n cannot be non-integer numeric", {
  expect_error(make_data(n = 1.5, p = 3, q = 2))
})

test_that("n cannot be 0", {
  expect_error(make_data(n = 0, p = 3, q = 2))
})

test_that("n cannot be negative (-1)", {
  expect_error(make_data(n = -1, p = 3, q = 2))
})

test_that("n cannot be a string", {
  expect_error(make_data(n = "", p = 3, q = 2))
})

test_that("n cannot be a list", {
  expect_error(make_data(n = list(), p = 3, q = 2))
})

test_that("n cannot be non-integer numeric", {
  expect_error(make_data(n = 1.5, p = 3, q = 2))
})

test_that("p cannot be non-integer numeric", {
  expect_error(make_data(n = 10, p = 3.5, q = 2))
})

test_that("p cannot be 0", {
  expect_error(make_data(n = 10, p = 0, q = 2))
})

test_that("p cannot be negative (-1)", {
  expect_error(make_data(n = 10, p = 0, q = 2))
})

test_that("p cannot be a string", {
  expect_error(make_data(n = 10, p = "", q = 2))
})

test_that("p cannot be a list", {
  expect_error(make_data(n = 10, p = list(), q = 2))
})

test_that("q cannot be non-integer numeric", {
  expect_error(make_data(n = 10, p = 5, q = 2.5))
})

test_that("q cannot be 0", {
  expect_error(make_data(n = 10, p = 3, q = 0))
})

test_that("q cannot be negative (-1)", {
  expect_error(make_data(n = 10, p = 3, q = -1))
})

test_that("q cannot be a string", {
  expect_error(make_data(n = 10, p = 3, q = ""))
})

test_that("q cannot be a list", {
  expect_error(make_data(n = 10, p = 3, q = list()))
})

test_that("sigma cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, sigma = -1e-32))
})

test_that("rho_x cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_x = -1e-32))
})

test_that("rho_x cannot be one", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_x = 1))
})

test_that("rho_x cannot be greater than one", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_x = 1 + 1e-6))
})

test_that("rho_err cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_err = -1e-32))
})

test_that("rho_err cannot be one", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_err = 1))
})

test_that("rho_err cannot be greater than one", {
  expect_error(make_data(n = 10, p = 3, q = 2, rho_err = 1 + 1e-6))
})

test_that("h cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, h = -10^-17))
})

test_that("h cannot be one", {
  expect_error(make_data(n = 10, p = 3, q = 2, h = 1))
})

test_that("h cannot be greater than one", {
  expect_error(make_data(n = 10, p = 3, q = 2, h = 1 + 1e-6))
})

test_that("power cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, h = -1e-32))
})

test_that("zero_appeal cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, zero_appeal = -1e-32))
})

test_that("n_edge cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, n_edge = -1e-32))
})

test_that("min_ev cannot be negative", {
  expect_error(make_data(n = 10, p = 3, q = 2, min_ev = -1e-32))
})

test_that("type cannot be numeric", {
  expect_error(make_data(n = 10, p = 3, q = 2, type = 1))
})

test_that("type cannot be a list", {
  expect_error(make_data(n = 10, p = 3, q = 2, type = ""))
})

test_that("type is not some arbitrary string", {
  expect_error(make_data(n = 10, p = 3, q = 2, type = "asjdk;"))
})

test_that("length of returned list is as expected", {
  reps <- 10
  data <- make_data(n = 5, p = 3, q = 2, reps = reps)
  expect_true(length(data) == reps)
})

test_that("length of all returned sublists are the same", {
  data <- make_data(n = 5, p = 3, q = 2, reps = 10)
  expect_true(length(unique(sapply(data, length))) == 1)
})

test_that("length of a given returned sublist is as expected: 7", {
  data <- make_data(n = 5, p = 3, q = 2, reps = 10)
  expect_true(length(data[[3]]) == 7)
})

test_that("returned sublists all have the same labeled elements", {
  expect_true(
    length(unique(lapply(make_data(n = 5, p = 3, q = 2, reps = 10), labels))) == 1
  )
})

test_that("labeled elements of a given returned sublist is as
          expected", {
  expect_true(
    all.equal(
      lapply(make_data(n = 5, p = 3, q = 2, reps = 10), labels)[[3]],
      c("X", "B", "Y", "E", "Sigma", "Omega", "Sigma_x")
    )
  )
})

test_that("At least one entry labeled 'X' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      0.150, 0.067, 0.169,
      0.699, -1.616, -0.892,
      0.970, -0.289, -0.012,
      0.112, -0.257, -0.585,
      0.800, 0.106, 0.318
    ),
    5, 3,
    byrow = T
  )
  expect_equal(round(data$X, 3), test_mat)
})

test_that("At least one entry labeled 'Y' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
       0.382,  0.000,
      -0.116, -1.373,
       1.180, -0.359,
      -0.557, -0.077,
       1.346, -0.060
    ),
    5, 2,
    byrow = T
  )
  expect_equal(round(data$Y, 3), test_mat)
})

test_that("At least one entry labeled 'B' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      0.215, 0,
      0.000, 0,
      1.086, 0
    ),
    3, 2,
    byrow = T
  )
  expect_equal(round(data$B, 3), test_mat)
})

test_that("At least one entry labeled 'E' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      0.166, 0.000,
      0.703, -1.373,
      0.985, -0.359,
      0.054, -0.077,
      0.828, -0.060
    ),
    5, 2,
    byrow = T
  )
  expect_equal(round(data$E, 3), test_mat)
})

test_that("At least one entry labeled 'Sigma' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      1.0, 0.7,
      0.7, 1.0
    ),
    2, 2,
    byrow = T
  )
  expect_equal(round(data$Sigma, 3), test_mat)
})

test_that("At least one entry labeled 'Omega' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      1.961, -1.373,
      -1.373, 1.961
    ),
    2, 2,
    byrow = T
  )
  expect_equal(round(data$Omega, 3), test_mat)
})

test_that("At least one entry labeled 'Sigma_x' in returned output lists is
          what it is expected to be for problem size (n,p,q) =
          (5,3,2)", {
  data <- make_data(
    n = 5, p = 3, q = 2, b1 = sqrt(0.5),
    b2 = sqrt(0.5), sigma = 1, rho_x = 0.6,
    type = "AR1", rho_err = 0.7, seed = 1729
  )[[1]]
  test_mat <- matrix(
    c(
      1.00, 0.6, 0.36,
      0.60, 1.0, 0.60,
      0.36, 0.6, 1.00
    ),
    3, 3,
    byrow = T
  )
  expect_equal(data$Sigma_x$covariance, test_mat)
})
