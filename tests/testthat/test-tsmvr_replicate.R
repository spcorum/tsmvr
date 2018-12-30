context("test-tsmvr_replicate")

test_that("X cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = '', Y = matrix(rep(0,2),2,1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("X cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = list(), Y = matrix(rep(0,2),2,1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("X cannot be a vector", {
  expect_error(
    tsmvr_replicate(X = rep(0,2), Y = matrix(rep(0,2),2,1), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,2),2,1), Y = '', s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,2),2,1), Y = list(), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("Y cannot be a vector", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,2),2,1), Y = rep(0,2), s1 = 1, s2 = 2, quiet = T)
  )
})

test_that("s1 cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = '', s2 = 2, quiet = T)
  )
})

test_that("s1 cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = list(), s2 = 2, quiet = T)
  )
})

test_that("s1 cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = 1.5, s2 = 2, quiet = T)
  )
})

test_that("s1 cannot be negative", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = -1, s2 = 2, quiet = T)
  )
})

test_that("s1 cannot greater than dim(X)[2] x dim(Y)[2]", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = 9, s2 = 2, quiet = T)
  )
})

test_that("s2 cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = 1, s2 = '', quiet = T)
  )
})

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = 1, s2 = list(), quiet = T)
  )
})

test_that("s2 cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,1),
             s1 = 1, s2 = 2.5, quiet = T)
  )
})

test_that("s2 cannot be less than dim(Y)[2]", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 1, s2 = 1, quiet = T)
  )
})

test_that("s2 cannot greater than (dim(Y)[2])^2", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 1, s2 = 5, quiet = T)
  )
})

test_that("k cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = '', quiet = T)
  )
})

test_that("s2 cannot be a list()", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = list(), quiet = T)
  )
})

test_that("s1 cannot be non-integer numeric", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 1.5, s2 = 2, quiet = T)
  )
})

test_that("s2 cannot be non-integer numeric", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2.5, quiet = T)
  )
})

test_that("k cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = 2.5, quiet = T)
  )
})

test_that("k cannot be negative", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = -1, quiet = T)
  )
})


test_that("k cannot be 0", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = 2.5, quiet = T)
  )
})

test_that("k cannot be 1", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, k = 2.5, quiet = T)
  )
})


test_that("reps cannot be non-integer valued", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
                    s1 = 2, s2 = 2, reps = 2.5, quiet = T)
  )
})

test_that("reps cannot be negative", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
                    s1 = 2, s2 = 2, reps = -1, quiet = T)
  )
})


test_that("reps cannot be 0", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
                    s1 = 2, s2 = 2, reps = 0, quiet = T)
  )
})



test_that("B_type cannot numeric", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, B_type = 1, quiet = T)
  )
})

test_that("B_type cannot be a list", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, B_type = list(), quiet = T)
  )
})

test_that("B_type cannot be some random character string", {
  library(stringi)
  test_string = ''
  while (!(test_string %in% c('gd'))) {
    test_string = stri_rand_strings(
      n=1,
      length=sample.int(n=10,size=1),
      pattern = "[A-Za-z0-9]"
    )
  }
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, B_type = test_string, quiet = T)
  )
})

test_that("Omega_type cannot numeric", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, Omega_type = 1, quiet = T)
  )
})

test_that("Omega_type cannot be a list", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, Omega_type = list(), quiet = T)
  )
})

test_that("Omega_type cannot be some random character string", {
  library(stringi)
  test_string = ''
  while (!(test_string %in% c('gd'))) {
    test_string = stri_rand_strings(
      n=1,
      length=sample.int(n=10,size=1),
      pattern = "[A-Za-z0-9]"
    )
  }
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,2),2,2),
             s1 = 2, s2 = 2, Omega_type = test_string, quiet = T)
  )
})


test_that("epsilon cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, epsilon = '', quiet = T)
  )
})

test_that("epsilon cannot be a list", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, epsilon = list(), quiet = T)
  )
})

test_that("epsilon cannot be zero", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, epsilon = 0, quiet = T)
  )
})

test_that("epsilon cannot be less than zero", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, epsilon = -1e-32, quiet = T)
  )
})

test_that("max_iter cannot be a list", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, max_iter = list(), quiet = T)
  )
})

test_that("max_iter cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, max_iter = '', quiet = T)
  )
})

test_that("max_iter cannot non-integer valued", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, max_iter = 1.5, quiet = T)
  )
})

test_that("max_iter cannot be zero", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, max_iter = 0, quiet = T)
  )
})

test_that("max_iter cannot be less than zero", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, max_iter = -1, quiet = T)
  )
})

test_that("seed cannot be a character string", {
  expect_error(
    tsmvr_replicate(X = matrix(rep(0,4),2,2), Y = matrix(rep(0,4),2,2),
             s1 = 2, s2 = 2, seed = '', quiet = T)
  )
})

test_that("tsmvr_replicate returns a list", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 40000,
    quiet = T
  )
  expect_true(is.list(z))
})

test_that("tsmvr_replicate returns a list of length 6", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 40000,
    quiet = T
  )
  expect_equal(length(z),6)
})

test_that("the labels of the list returned by tsmvr_replicate are as
          expected", {
            X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
            Y = matrix(rnorm(n = 300, mean = 1),100,3)
            z = tsmvr_replicate(
              X = X, Y = Y, k = 2, reps = 2,
              s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
              max_iter = 40000,
              quiet = T
            )
            expect_true(all(labels(z) ==
                    c('rep_error_mean', 'rep_error_sd',
                      'fold_error_means', 'fold_error_sds',
                      'folds', 'reps')))
            })

test_that("the listed item labeled 'rep_error_mean' is numeric", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(is.numeric(z$rep_error_mean))
})

test_that("the listed item labeled 'rep_error_sd' is numeric", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(is.numeric(z$rep_error_sd))
})

test_that("the listed item labeled 'fold_error_means' has length reps", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_equal(length(z$fold_error_means), reps)
})

test_that("the listed item labeled 'fold_error_sds' has length reps", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_equal(length(z$fold_error_sds),reps)
})


test_that("the listed item labeled 'folds' is numeric", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(is.numeric(z$folds))
})

test_that("the listed item labeled 'fold_error_sds' is a list", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(is.numeric(z$reps))
})

test_that("the listed item labeled 'folds' is integer valued", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(z$folds %% 1 == 0)
})

test_that("the listed item labeled 'reps' is integer valued", {
  X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
  Y = matrix(rnorm(n = 300, mean = 1),100,3)
  z = tsmvr_replicate(
    X = X, Y = Y, k = 2, reps = 2,
    s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
    max_iter = 20000,
    quiet = T
  )
  expect_true(z$reps %% 1 == 0)
})

# test_that("the returned sublist labeled 'num_folds' is numeric", {
#   X = matrix(rnorm(n = 1000, sd = 0.1),100,10)
#   Y = matrix(rnorm(n = 300, mean = 1),100,3)
#   z = tsmvr_replicate(
#     X = X, Y = Y, k = 10,
#     s1 = round(0.9*10*3), s2 = 3 + 2*(3-1),
#     max_iter = 1000,
#     quiet = T
#   )
#   expect_true(is.numeric(z$num_folds))
# })
