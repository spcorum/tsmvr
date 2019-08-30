context("test-tsmvr_replicate")

test_that("X cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = "", Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("X cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = list(), Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("X cannot be a vector", {
  expect_error(
    tsmvr_replicate(X = rep(0, 2), Y = matrix(rep(0, 2), 2, 1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0, 2), 2, 1), Y = "", s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0, 2), 2, 1), Y = list(), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a vector", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0, 2), 2, 1), Y = rep(0, 2), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("s1 cannot be a character string", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = "", s2 = 2, quiet = T
    )
  )
})

test_that("s1 cannot be a list()", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = list(), s2 = 2, quiet = T
    )
  )
})

test_that("s1 cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1.5, s2 = 2, quiet = T
    )
  )
})

test_that("s1 cannot be negative", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = -1, s2 = 2, quiet = T
    )
  )
})

test_that("s1 cannot greater than dim(X)[2] x dim(Y)[2]", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 9, s2 = 2, quiet = T
    )
  )
})

test_that("s2 cannot be a character string", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = "", quiet = T
    )
  )
})

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = list(), quiet = T
    )
  )
})

test_that("s2 cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1 = 1, s2 = 2.5, quiet = T
    )
  )
})

test_that("s2 cannot be less than dim(Y)[2]", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 1, s2 = 1, quiet = T
    )
  )
})

test_that("s2 cannot greater than (dim(Y)[2])^2", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1 = 1, s2 = 5, quiet = T
    )
  )
})

# test_that("k cannot be a character string", {
#   expect_error(
#     tsmvr_replicate(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1 = 2, s2 = 2, k = "", quiet = T
#     )
#   )
# })

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2, k = list(), quiet = T
    )
  )
})

test_that("s1 cannot be non-integer numeric", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 1.5, s2 = 2, quiet = T
    )
  )
})

test_that("s2 cannot be non-integer numeric", {
  expect_error(
    tsmvr_replicate(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1 = 2, s2 = 2.5, quiet = T
    )
  )
})

test_that("tsmvr_replicate returns a list, the length of that list is 7,
          and the labels of that list are as expected", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(
    all(
      is.list(z), length(z) == 7,
      all(labels(z) == c(
        "rep_error_mean", "rep_error_sd",
        "fold_error_means", "fold_error_sds", "folds", "reps",
        "time"
      ))
    )
  )
})

test_that("from the returned list, the item labeled 'rep_error_mean' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$rep_error_mean))
})

test_that("the listed item labeled 'rep_error_sd' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$rep_error_sd))
})

test_that("the listed item labeled 'fold_error_means' has length reps", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_equal(length(z$fold_error_means), 2)
})

test_that("the listed item labeled 'fold_error_sds' has length reps", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_equal(length(z$fold_error_sds), 2)
})


test_that("the listed item labeled 'folds' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$folds))
})

test_that("the listed item labeled 'fold_error_sds' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$reps))
})

test_that("the listed item labeled 'folds' is integer valued", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(z$folds %% 1 == 0)
})

test_that("the listed item labeled 'reps' is integer valued", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(z$reps %% 1 == 0)
})

test_that("lines covered by quiet = F can execute", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.list(z))
})

test_that("the returned sublist labeled 'num_folds' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters = set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_replicate(
    X = X, Y = Y, s1 = round(0.5 * 4 * 3), s2 = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$folds))
})
