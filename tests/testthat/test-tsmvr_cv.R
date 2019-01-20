context("test-tsmvr_cv")

test_that("X cannot be a character string", {
  expect_error(
    tsmvr_cv(X = "", Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2)
  )
})

test_that("X cannot be a list()", {
  expect_error(
    tsmvr_cv(X = list(), Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2)
  )
})

test_that("X cannot be a vector", {
  expect_error(
    tsmvr_cv(X = rep(0, 2), Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2)
  )
})

test_that("Y cannot be a character string", {
  expect_error(
    tsmvr_cv(X = matrix(rep(0, 2), 2, 1), Y = "", s1 = 1, s2 = 2)
  )
})

test_that("Y cannot be a list()", {
  expect_error(
    tsmvr_cv(X = matrix(rep(0, 2), 2, 1), Y = list(), s1 = 1, s2 = 2)
  )
})

test_that("Y cannot be a vector", {
  expect_error(
    tsmvr_cv(X = matrix(rep(0, 2), 2, 1), Y = rep(0, 2), s1 = 1, s2 = 2)
  )
})

test_that("s1 cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = "", s2 = 2
    )
  )
})

test_that("s1 cannot be a list()", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = list(), s2 = 2
    )
  )
})

test_that("s1 cannot be non-integer valued", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1.5, s2 = 2
    )
  )
})

test_that("s1 cannot be negative", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = -1, s2 = 2
    )
  )
})

test_that("s1 cannot greater than dim(X)[2] x dim(Y)[2]", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 9, s2 = 2
    )
  )
})

test_that("s2 cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = ""
    )
  )
})

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = list()
    )
  )
})

test_that("s2 cannot be non-integer valued", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = 2.5
    )
  )
})

test_that("s2 cannot be less than dim(Y)[2]", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 1, s2 = 1
    )
  )
})

test_that("s2 cannot greater than (dim(Y)[2])^2", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 1, s2 = 5
    )
  )
})

test_that("k cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = ""
    )
  )
})

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = list()
    )
  )
})

test_that("s1 cannot be non-integer numeric", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 1.5, s2 = 2
    )
  )
})

test_that("s2 cannot be non-integer numeric", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2.5
    )
  )
})

test_that("k cannot be non-integer valued", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = 2.5
    )
  )
})

test_that("k cannot be negative", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = -1
    )
  )
})


test_that("k cannot be 0", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = 2.5
    )
  )
})

test_that("k cannot be 1", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = 2.5
    )
  )
})


test_that("B_type cannot numeric", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, B_type = 1
    )
  )
})

test_that("B_type cannot be a list", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, B_type = list()
    )
  )
})

test_that("B_type cannot be some random character string", {
  library(stringi)
  test_string <- ""
  while (TRUE) {
    test_string <- stri_rand_strings(
      n = 1,
      length = sample.int(n = 10, size = 1),
      pattern = "[A-Za-z0-9]"
    )
    if (!(test_string %in% c("gd", "ls"))) break
  }
  test_string
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, B_type = test_string
    )
  )
})

test_that("Omega_type cannot numeric", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, Omega_type = 1
    )
  )
})

test_that("Omega_type cannot be a list", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, Omega_type = list()
    )
  )
})

test_that("Omega_type cannot be some random character string", {
  library(stringi)
  test_string <- ""
  while (TRUE) {
    test_string <- stri_rand_strings(
      n = 1,
      length = sample.int(n = 10, size = 1),
      pattern = "[A-Za-z0-9]"
    )
    if (!(test_string %in% c("gd", "ls", "min"))) break
  }
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 2, s2 = 2, Omega_type = test_string
    )
  )
})

test_that("epsilon cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, epsilon = ""
    )
  )
})

test_that("epsilon cannot be a list", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, epsilon = list()
    )
  )
})

test_that("epsilon cannot be zero", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, epsilon = 0
    )
  )
})

test_that("epsilon cannot be less than zero", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, epsilon = -1e-32
    )
  )
})

test_that("max_iter cannot be a list", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, max_iter = list()
    )
  )
})

test_that("max_iter cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, max_iter = ""
    )
  )
})

test_that("max_iter cannot non-integer valued", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, max_iter = 1.5
    )
  )
})

test_that("max_iter cannot be zero", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, max_iter = 0
    )
  )
})

test_that("max_iter cannot be less than zero", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, max_iter = -1
    )
  )
})

test_that("seed cannot be a character string", {
  expect_error(
    tsmvr_cv(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, seed = ""
    )
  )
})

test_that("tsmvr_cv returns a list and that list has length 4", {
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  z <- tsmvr_cv(
    X = X, Y = Y, k = 2,
    s1 = round(0.5 * 4 * 3), s2 = 7,
    B_type = "ls", Omega_type = "ls",
    rho1 = 1, rho2 = 1,
    max_iter = 1, quiet = T, suppress = T
  )
  expect_true(all(is.list(z), length(z) == 4))
})

test_that("the labels of the list returned by tsmvr_cv are as
          expected", {
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  z <- tsmvr_cv(
    X = X, Y = Y, k = 2,
    s1 = round(0.5 * 4 * 3), s2 = 7,
    B_type = "ls", Omega_type = "ls",
    rho1 = 1, rho2 = 1,
    max_iter = 1, quiet = T, suppress = T
  )
  expect_true(all(labels(z) ==
    c("error_mean", "error_sd", "num_folds", "time")))
})

test_that("the returned sublist labeled 'error_mean' is numeric", {
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  z <- tsmvr_cv(
    X = X, Y = Y, k = 2,
    s1 = round(0.5 * 4 * 3), s2 = 7,
    B_type = "ls", Omega_type = "ls",
    rho1 = 1, rho2 = 1,
    max_iter = 1, quiet = T, suppress = T
  )
  expect_true(is.numeric(z$error_mean))
})

test_that("the returned sublist labeled 'error_mean' is numeric", {
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  z <- tsmvr_cv(
    X = X, Y = Y, k = 2,
    s1 = round(0.5 * 4 * 3), s2 = 7,
    B_type = "ls", Omega_type = "ls",
    rho1 = 1, rho2 = 1,
    max_iter = 1, quiet = T, suppress = T
  )
  expect_true(is.numeric(z$error_sd))
})

test_that("the returned sublist labeled 'num_folds' is numeric", {
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  z <- tsmvr_cv(
    X = X, Y = Y, k = 2,
    s1 = round(0.5 * 4 * 3), s2 = 7,
    B_type = "ls", Omega_type = "ls",
    rho1 = 1, rho2 = 1,
    max_iter = 1, quiet = T, suppress = T
  )
  expect_true(is.numeric(z$num_folds))
})
