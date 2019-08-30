context("test-tsmvr_gridsearch")

test_that("X cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(X = "", Y = matrix(rep(0, 2), 2, 1), s1_grid = 1, s2_grid_grid = 2, quiet = T)
  )
})

test_that("X cannot be a list()", {
  expect_error(
    tsmvr_gridsearch(X = list(), Y = matrix(rep(0, 2), 2, 1), s1_grid = 1, s2_grid_grid = 2, quiet = T)
  )
})

test_that("X cannot be a vector", {
  expect_error(
    tsmvr_gridsearch(X = rep(0, 2), Y = matrix(rep(0, 2), 2, 1), s1_grid = 1, s2_grid_grid = 2, quiet = T)
  )
})

test_that("Y cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(X = matrix(rep(0, 2), 2, 1), Y = "", s1_grid = 1, s2_grid_grid = 2, quiet = T)
  )
})

test_that("Y cannot be a list()", {
  expect_error(
    tsmvr_gridsearch(X = matrix(rep(0, 2), 2, 1), Y = list(), s1_grid = 1, s2_grid_grid = 2, quiet = T)
  )
})

test_that("Y cannot be a vector", {
  expect_error(
    tsmvr_gridsearch(X = matrix(rep(0, 2), 2, 1), Y = rep(0, 2), s1_grid = 1, s2_grid = 2, quiet = T)
  )
})

test_that("s1_grid cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = "", s2_grid = 2, quiet = T
    )
  )
})

test_that("s1_grid cannot be a list()", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = list(), s2_grid = 2, quiet = T
    )
  )
})

test_that("s1_grid cannot be non-integer valued", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = 1.5, s2_grid = 2, quiet = T
    )
  )
})

test_that("s1_grid cannot be negative", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = -1, s2_grid = 2, quiet = T
    )
  )
})

test_that("s1_grid cannot greater than dim(X)[2] x dim(Y)[2]", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = 9, s2_grid = 2, quiet = T
    )
  )
})

test_that("s2_grid cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = 1, s2_grid = "", quiet = T
    )
  )
})

test_that("s2_grid cannot be a list()", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = 1, s2_grid = list(), quiet = T
    )
  )
})

test_that("s2_grid cannot be non-integer valued", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 1),
      s1_grid = 1, s2_grid = 2.5, quiet = T
    )
  )
})

test_that("s2_grid cannot be less than dim(Y)[2]", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 1, s2_grid = 1, quiet = T
    )
  )
})

test_that("s2_grid cannot greater than (dim(Y)[2])^2", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 1, s2_grid = 5, quiet = T
    )
  )
})

test_that("k cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = "", quiet = T
    )
  )
})

test_that("s2_grid cannot be a list()", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = list(), quiet = T
    )
  )
})

test_that("s1_grid cannot be non-integer numeric", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 1.5, s2_grid = 2, quiet = T
    )
  )
})

test_that("s2_grid cannot be non-integer numeric", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2.5, quiet = T
    )
  )
})

test_that("k cannot be non-integer valued", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = 2.5, quiet = T
    )
  )
})

test_that("k cannot be negative", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = -1, quiet = T
    )
  )
})


test_that("k cannot be 0", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = 2.5, quiet = T
    )
  )
})

test_that("k cannot be 1", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, k = 2.5, quiet = T
    )
  )
})


test_that("reps cannot be non-integer valued", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, reps = 2.5, quiet = T
    )
  )
})

test_that("reps cannot be negative", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, reps = -1, quiet = T
    )
  )
})


test_that("reps cannot be 0", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, reps = 0, quiet = T
    )
  )
})



test_that("B_type cannot numeric", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, B_type = 1, quiet = T
    )
  )
})

test_that("B_type cannot be a list", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, B_type = list(), quiet = T
    )
  )
})

test_that("B_type cannot be some random character string", {
  library(stringi)
  test_string <- ""
  while (T) {
    test_string <- stri_rand_strings(
      n = 1,
      length = sample.int(n = 10, size = 1),
      pattern = "[A-Za-z0-9]"
    )
    if (!(test_string %in% c("gd"))) break
  }
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, B_type = test_string, quiet = T
    )
  )
})

test_that("Omega_type cannot numeric", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, Omega_type = 1, quiet = T
    )
  )
})

test_that("Omega_type cannot be a list", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, Omega_type = list(), quiet = T
    )
  )
})

test_that("Omega_type cannot be some random character string", {
  library(stringi)
  test_string <- ""
  while (T) {
    test_string <- stri_rand_strings(
      n = 1,
      length = sample.int(n = 10, size = 1),
      pattern = "[A-Za-z0-9]"
    )
    if (!(test_string %in% c("gd"))) break
  }
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
      s1_grid = 2, s2_grid = 2, Omega_type = test_string, quiet = T
    )
  )
})

test_that("epsilon cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, epsilon = "", quiet = T
    )
  )
})

test_that("epsilon cannot be a list", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, epsilon = list(), quiet = T
    )
  )
})

test_that("epsilon cannot be zero", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, epsilon = 0, quiet = T
    )
  )
})

test_that("epsilon cannot be less than zero", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, epsilon = -1e-32, quiet = T
    )
  )
})

test_that("max_iter cannot be a list", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, max_iter = list(), quiet = T
    )
  )
})

test_that("max_iter cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, max_iter = "", quiet = T
    )
  )
})

test_that("max_iter cannot non-integer valued", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, max_iter = 1.5, quiet = T
    )
  )
})

test_that("max_iter cannot be zero", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, max_iter = 0, quiet = T
    )
  )
})

test_that("max_iter cannot be less than zero", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, max_iter = -1, quiet = T
    )
  )
})

test_that("seed cannot be a character string", {
  expect_error(
    tsmvr_gridsearch(
      X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
      s1_grid = 2, s2_grid = 2, seed = "", quiet = T
    )
  )
})

test_that("tsmvr_replicate returns a list, the length of that list is 10,
          and the labels of that list are as expected", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(
    all(
      is.list(z), length(z) == 10,
      all(labels(z) == c(
        "solution",
        "error_min", "error_min_sd",
        "s1_min", "s2_min", "s1_vec", "s2_vec",
        "folds", "reps", "gs_time"
      ))
    )
  )
})

test_that("the listed item labeled 'solution' is a list", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.list(z$solution))
})

test_that("the listed item labeled 'error_min' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$error_min))
})

test_that("the listed item labeled 'error_min_sd' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$error_min_sd))
})


test_that("the listed item labeled 's1_min' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$s1_min))
})

test_that("the listed item labeled 's2_min' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$s2_min))
})

test_that("the listed item labeled 'error_min' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$error_min))
})

test_that("the listed item labeled 'error_min_sd' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$error_min_sd))
})

test_that("the listed item labeled 'gs_time' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$gs_time))
})


test_that("the listed item labeled 'folds' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$folds))
})

test_that("the listed item labeled 'reps' is numeric", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.numeric(z$reps))
})

test_that("the listed item labeled 'error_min' is a vector", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.vector(z$error_min))
})

test_that("the listed item labeled 'error_min_sd' is a vector", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.vector(z$error_min_sd))
})

test_that("the listed item labeled 'k' is integer valued", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(z$folds %% 1 == 0)
})


test_that("the listed item labeled 'reps' is integer valued", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = T, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(z$reps %% 1 == 0)
})

test_that("lines covered by quiet = F can execute", {
  set.seed(1)
  X <- matrix(rnorm(n = 40, sd = 0.1), 10, 4)
  Y <- matrix(rnorm(n = 30, mean = 1), 10, 3)
  parameters <- set_parameters(
    B_type = "ls", Omega_type = "ls", rho1 = 1, rho2 = 1,
    k = 2, reps = 2, max_iter = 1, quiet = F, suppress = T
  )
  z <- tsmvr_gridsearch(
    X = X, Y = Y, s1_vec = round(0.5 * 4 * 3), s2_vec = 7, pars = parameters,
    seed = 1729
  )
  expect_true(is.list(z))
})
