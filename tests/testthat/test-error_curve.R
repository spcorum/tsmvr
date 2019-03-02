context("test-error_curve")

test_that("Hat.list cannot be NULL", {
  expect_error(error_curve(Hat.list = NULL))
})

test_that("Hat.list cannot be numeric", {
  expect_error(error_curve(Hat.list = 1, Star = NULL))
})

test_that("Hat.list cannot be a string", {
  expect_error(error_curve(Hat.list = "a", Star = NULL))
})

test_that("Hat.list cannot be a vector", {
  expect_error(error_curve(Hat.list = c(1, 2)))
})

test_that("Hat.list cannot be a matrix", {
  expect_error(error_curve(Hat.list = matrix(c(1, 2), 1, 2)))
})

test_that("If Hat.list is a list, it must be a list of all
          matrices", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  k <- sample(x = seq.int(from = 1, to = steps), size = 1)
  Hat.list[[k]] <- 0
  expect_error(error_curve(Hat.list = matrix(c(1, 2), 1, 2)))
})

test_that("Sigma cannot be numeric", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_error(error_curve(Hat.list, Star = 1))
})

test_that("Sigma cannot be a string", {
  expect_error(error_curve(Hat.list = "a", Star = "a"))
})

test_that("Sigma cannot be a vector", {
  expect_error(error_curve(Hat.list = c(1, 2), Star = c(1, 2)))
})

test_that("Sigma cannot be a matrix", {
  expect_error(error_curve(Hat.list = list(), Star = list()))
})

test_that("output is a vector", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rep(0, 9), 3, 3)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_true(is.vector(error_curve(Hat.list, Star)))
})

test_that("output is a vector", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rep(0, 9), 3, 3)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_true(is.vector(error_curve(Hat.list, Star)))
})

test_that("output vector is of expected length", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rep(0, 9), 3, 3)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_equal(length(error_curve(Hat.list, Star)), steps)
})

test_that("all entries output vector are numeric", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rep(0, 9), 3, 3)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_true(all(sapply(error_curve(Hat.list, Star), is.numeric)))
})

test_that("all entries output vector are numeric 'double' ", {
  steps <- 100
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rep(0, 9), 3, 3)
  for (i in 1:steps) {
    mat_entry <- (1 - (i - 1) / steps) / 1000
    Hat.list[[i]] <- matrix(rep(mat_entry, 9), 3, 3)
  }
  expect_true(all(sapply(error_curve(Hat.list, Star), typeof) == "double"))
})

test_that("output vector entries are all non-negative", {
  steps <- 1000
  Hat.list <- rep(list(NULL), steps)
  Star <- matrix(rnorm(n = 9, sd = 0.001), 3, 3)
  for (i in 1:steps) {
    Hat.list[[i]] <- matrix(rnorm(n = 9, sd = 0.001), 3, 3)
  }
  expect_true(all(error_curve(Hat.list, Star) >= 0))
})
