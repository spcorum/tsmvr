context("test-k_folds")

test_that("n cannot be a string", {
  expect_error(k_folds("", 2))
})

test_that("n cannot be a list", {
  expect_error(k_folds(list(), 2))
})

test_that("n cannot be non-integer numeric", {
  while (TRUE) {
    test_num <- runif(1, -100, 100)
    if (test_num %% 1 != 0) break
  }
  expect_error(k_folds(test_num, 2))
})

test_that("n cannot be negative", {
  expect_error(k_folds(-1, 2))
})

test_that("n cannot be zero", {
  expect_error(k_folds(0, 2))
})

test_that("k cannot be a string", {
  expect_error(k_folds(10, ""))
})

test_that("k cannot be a list", {
  expect_error(k_folds(10, list()))
})

test_that("k cannot be non-integer numeric", {
  while (TRUE) {
    test_num <- runif(1, -100, 100)
    if (test_num %% 1 != 0) break
  }
  expect_error(k_folds(10, test_num))
})

test_that("k cannot be negative", {
  expect_error(k_folds(10, -1))
})

test_that("k cannot be zero", {
  expect_error(k_folds(10, -1))
})

test_that("k cannot be one", {
  expect_error(k_folds(10, 1))
})

test_that("the returned object is a list, that list has length 2,
          the listed names are as expected, each sublist has
          length k", {
  test_k <- sample.int(10, 1) + 1
  z <- k_folds(100, test_k)
  expect_true(
    all(
      is.list(z), length(z) == 2,
      all(labels(z) == c("train", "val")),
      all(lapply(z, length) == test_k)
    )
  )
})

test_that("the sublists of the returned sublist labeled 'train' are
          numeric, each with length greater than zero", {
  test_k <- sample.int(10, 1) + 1
  z <- k_folds(100, test_k)
  expect_true(
    all(
      all(sapply(z$train, is.numeric)),
      all(sapply(z$train, length) > 0)
    )
  )
})


test_that("the sublists of the returned sublist labeled 'val' are
          numeric, each with length greater than zero", {
  test_k <- sample.int(10, 1) + 1
  z <- k_folds(100, test_k)
  expect_true(
    all(
      all(sapply(z$val, is.numeric)),
      all(sapply(z$val, length) > 0)
    )
  )
})
