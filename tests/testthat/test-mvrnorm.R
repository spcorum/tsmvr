context("test-mvrnorm")

# test_that("output matrix is of dimension n-by-length(mu)", {
#   expect_equal(
#     dim(mvrnorm(n = 100, mu = rep(0, 10), Sigma = diag(10))),
#     c(100, 10)
#   )
# })

test_that("replicate output matrices are of dimension n-by-length(mu)", {
  expect_equal(dim(mvrnorm(
    n = 100, mu = rep(0, 10), Sigma = diag(10),
    reps = 2
  )[[2]]), c(100, 10))
})

test_that("length of output list is same as reps", {
  expect_equal(length(mvrnorm(
    n = 100, mu = rep(0, 10), Sigma = diag(10),
    reps = 3
  )), 3)
})

test_that("replicate output matrix is of dimension n-by-length(mu)", {
  expect_equal(
    dim(mvrnorm(n = 100, mu = rep(0, 10), Sigma = diag(10), reps = 2)[[2]]),
    c(100, 10)
  )
})

test_that("reps cannot be zero", {
  expect_error(mvrnorm(n = 100, mu = rep(0, 10), Sigma = diag(10), reps = 0))
})

test_that("reps cannot be less than zero", {
  expect_error(mvrnorm(n = 100, mu = rep(0, 10), Sigma = diag(10), reps = -1))
})

test_that("reps must be integer", {
  expect_error(mvrnorm(n = 100, mu = rep(0, 10), Sigma = diag(10), reps = 1.5))
})

test_that("Sigma must be positive semi-definite", {
  expect_error(mvrnorm(
    n = 100, mu = rep(0, 2),
    Sigma = matrix(c(0, 1, 1, 0), 2, 2)
  ))
})

test_that("length of mu must be dim of Sigma", {
  expect_error(mvrnorm(
    n = 100, mu = rep(0, 3),
    Sigma = matrix(c(1, 0, 0, 1), 2, 2)
  ))
})
