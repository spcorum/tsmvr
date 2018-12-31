context("test-tsmvr_predict")

test_that("tsmvr_list cannot be numeric", {
  set.seed(1)
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  expect_error(tsmvr_predict(tsmvr_list = 1, X))
})

test_that("tsmvr_list cannot a character string", {
  set.seed(1)
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  expect_error(tsmvr_predict(tsmvr_list = "", X))
})

test_that("tsmvr_list cannot have an arbritrary named element", {
  seed = 1
  set.seed(seed)
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  tsmvr_list <- list()
  tsmvr_list$any_old_label <- 0
  expect_error(tsmvr_predict(tsmvr_list, X))
})

test_that("tsmvr_list$B_hat cannot be a list", {
  seed = 1
  set.seed(seed)
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  tsmvr_list <- list()
  tsmvr_list$B_hat <- list()
  expect_error(tsmvr_predict(tsmvr_list, X))
})

test_that("tsmvr_list$B_hat cannot be a character string", {
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  tsmvr_list <- list()
  tsmvr_list$B_hat <- ""
  expect_error(tsmvr_predict(tsmvr_list, X))
})

test_that("X cannot be a list", {
  tsmvr_list <- list()
  tsmvr_list$B_hat <- matrix(sample.int(20, 8, replace = T) - 10, 4, 2)
  expect_error(tsmvr_predict(tsmvr_list = 1, X = list()))
})

test_that("X be a character string", {
  tsmvr_list <- list()
  tsmvr_list$B_hat <- matrix(sample.int(20, 8, replace = T) - 10, 4, 2)
  expect_error(tsmvr_predict(tsmvr_list = 1, X = ""))
})

test_that("the inner dimensions of X and tsmvr_list$B_hat cannot be different", {
  seed = 1
  set.seed(seed)
  X <- matrix(sample.int(20, 24, replace = T) - 10, 8, 3)
  tsmvr_list <- list()
  tsmvr_list$B_hat <- matrix(sample.int(20, 8, replace = T) - 10, 4, 2)
  expect_error(tsmvr_predict(tsmvr_list, X, seed = seed))
})

test_that("tsmvr_predict gives the right answer", {
  seed = 1
  set.seed(seed)
  X <- matrix(sample.int(20, 32, replace = T) - 10, 8, 4)
  tsmvr_list <- list()
  tsmvr_list$B_hat <- matrix(sample.int(20, 8, replace = T) - 10, 4, 2)
  expect_equal(
    tsmvr_predict(tsmvr_list, X), X %*% tsmvr_list$B_hat
  )
})
