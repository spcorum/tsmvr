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



# test_that("B_type cannot numeric", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, B_type = 1, quiet = T
#     )
#   )
# })
#
# test_that("B_type cannot be a list", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, B_type = list(), quiet = T
#     )
#   )
# })
#
# test_that("B_type cannot be some random character string", {
#   library(stringi)
#   test_string <- ""
#   while (!(test_string %in% c("gd"))) {
#     test_string <- stri_rand_strings(
#       n = 1,
#       length = sample.int(n = 10, size = 1),
#       pattern = "[A-Za-z0-9]"
#     )
#   }
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, B_type = test_string, quiet = T
#     )
#   )
# })
#
# test_that("Omega_type cannot numeric", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, Omega_type = 1, quiet = T
#     )
#   )
# })
#
# test_that("Omega_type cannot be a list", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, Omega_type = list(), quiet = T
#     )
#   )
# })
#
# test_that("Omega_type cannot be some random character string", {
#   library(stringi)
#   test_string <- ""
#   while (!(test_string %in% c("gd"))) {
#     test_string <- stri_rand_strings(
#       n = 1,
#       length = sample.int(n = 10, size = 1),
#       pattern = "[A-Za-z0-9]"
#     )
#   }
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 2), 2, 2),
#       s1_grid = 2, s2_grid = 2, Omega_type = test_string, quiet = T
#     )
#   )
# })
#
#
# test_that("epsilon cannot be a character string", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, epsilon = "", quiet = T
#     )
#   )
# })
#
# test_that("epsilon cannot be a list", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, epsilon = list(), quiet = T
#     )
#   )
# })
#
# test_that("epsilon cannot be zero", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, epsilon = 0, quiet = T
#     )
#   )
# })
#
# test_that("epsilon cannot be less than zero", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, epsilon = -1e-32, quiet = T
#     )
#   )
# })
#
# test_that("max_iter cannot be a list", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, max_iter = list(), quiet = T
#     )
#   )
# })
#
# test_that("max_iter cannot be a character string", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, max_iter = "", quiet = T
#     )
#   )
# })
#
# test_that("max_iter cannot non-integer valued", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, max_iter = 1.5, quiet = T
#     )
#   )
# })
#
# test_that("max_iter cannot be zero", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, max_iter = 0, quiet = T
#     )
#   )
# })
#
# test_that("max_iter cannot be less than zero", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, max_iter = -1, quiet = T
#     )
#   )
# })
#
# test_that("seed cannot be a character string", {
#   expect_error(
#     tsmvr_gridsearch(
#       X = matrix(rep(0, 4), 2, 2), Y = matrix(rep(0, 4), 2, 2),
#       s1_grid = 2, s2_grid = 2, seed = "", quiet = T
#     )
#   )
# })
#
# test_that("tsmvr_gridsearch returns a list", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(is.list(z))
# })
#
# test_that("tsmvr_gridsearch returns a list of length 6", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_equal(length(z), 7)
# })
#
# test_that("the labels of the list returned by tsmvr_gridsearch are as
#           expected", {
#             seed <- 1
#             set.seed(seed)
#             X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#             Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#             z <- tsmvr_gridsearch(
#               X = X, Y = Y, k = 2, reps = 2,
#               s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#               quiet = T, seed = seed
#             )
#             expect_true(all(labels(z) ==
#                               c(
#                                 "rep_error_mean", "rep_error_sd",
#                                 "fold_error_means", "fold_error_sds",
#                                 "folds", "reps", "time"
#                               )))
#             })
#
# test_that("the listed item labeled 'rep_error_mean' is numeric", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(is.numeric(z$rep_error_mean))
# })
#
# test_that("the listed item labeled 'rep_error_sd' is numeric", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(is.numeric(z$rep_error_sd))
# })
#
# test_that("the listed item labeled 'fold_error_means' has length reps", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_equal(length(z$fold_error_means), 2)
# })
#
# test_that("the listed item labeled 'fold_error_sds' has length reps", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, max_iter = 40000, seed = seed
#   )
#   expect_equal(length(z$fold_error_sds), 2)
# })
#
#
# test_that("the listed item labeled 'folds' is numeric", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(is.numeric(z$folds))
# })
#
# test_that("the listed item labeled 'fold_error_sds' is numeric", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(is.numeric(z$reps))
# })
#
# test_that("the listed item labeled 'folds' is integer valued", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(z$folds %% 1 == 0)
# })
#
# test_that("the listed item labeled 'reps' is integer valued", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = T, seed = seed
#   )
#   expect_true(z$reps %% 1 == 0)
# })
#
# test_that("lines covered by quiet = F can execute", {
#   seed <- 1
#   set.seed(seed)
#   X <- matrix(rnorm(n = 1000, sd = 0.1), 100, 10)
#   Y <- matrix(rnorm(n = 300, mean = 1), 100, 3)
#   z <- tsmvr_gridsearch(
#     X = X, Y = Y, k = 2, reps = 2,
#     s1_grid = round(0.9 * 10 * 3), s2_grid = 3 + 2 * (3 - 1),
#     quiet = F, seed = seed
#   )
#   expect_true(is.list(z))
# })
#
# # test_that("the returned sublist labeled 'num_folds' is numeric", {
# #   X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
# #   Y = matrix(rnorm(n = 300, mean = 1),100,3)
# #   z = tsmvr_gridsearch(
# #     X = X, Y = Y, k = 10,
# #     s1_grid = round(0.9*10*3), s2_grid = 3 + 2*(3-1),
# #     max_iter = 1000,
# #     quiet = T
# #   )
# #   expect_true(is.numeric(z$num_folds))
# # })
#
