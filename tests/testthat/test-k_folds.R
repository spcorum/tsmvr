context("test-k_folds")

test_that("n is not a string", {
  expect_error(k_folds(n = "", k = 2))
})

test_that("n is not a list", {
  expect_error(k_folds(n = list(), k = 2))
})

test_that("n is not a vector", {
  expect_error(k_folds(n = c(1), k = 2))
})

test_that("n is not a non-integer valued numeric", {
  expect_error(k_folds(n = 3.5, k = 2))
})

test_that("n cannot be 1", {
  expect_error(k_folds(n = 1, k = 2))
})

test_that("n cannot be 0", {
  expect_error(k_folds(n = 0, k = 2))
})

test_that("n cannot be -1", {
  expect_error(k_folds(n = -1, k = 2))
})

test_that("n cannot be -10", {
  expect_error(k_folds(n = -10, k = 2))
})

test_that("k is not a string", {
  expect_error(k_folds(n = 2, k = ""))
})

test_that("k is not a list", {
  expect_error(k_folds(n = 2, k = list()))
})

test_that("k is not a vector", {
  expect_error(k_folds(n = 2, k = c(1)))
})

test_that("k is not a non-integer valued numeric", {
  expect_error(k_folds(n = 2, k = 2.5))
})

test_that("k cannot be 1", {
  expect_error(k_folds(n = 2, k = 1))
})

test_that("k cannot be 0", {
  expect_error(k_folds(n = 2, k = 0))
})

test_that("k cannot be -1", {
  expect_error(k_folds(n = 2, k = -1))
})

test_that("k cannot be -10", {
  expect_error(k_folds(n = 2, k = -10))
})

test_that("n = 2, k = 3 returns an error (k cannot be greater
          than n", {
  expect_error(k_folds(n = n, k = 3))
})

test_that("n = 99, k = 100 returns an error (k can be equivalent
          to n", {
  expect_error(k_folds(n = 99, k = 100))
})

test_that("the returned object is a list", {
  expect_true(is.list(k_folds(n = 2, k = 2)))
})

test_that("the length of the returned list is 2", {
  expect_true(length(k_folds(n = 2, k = 2)) == 2)
})

test_that("returned list's names are c('train', 'val')", {
  expect_equal(
    names(k_folds(n = 2, k = 2)),
    c("train", "val")
  )
})

test_that("the length of the returned sublist labeled 'train' is 2
          when k = 2", {
  expect_true(length(k_folds(n = 100, k = 2)$train) == 2)
})

test_that("the length of the returned sublist labeled 'train' is 10
          when k = 10", {
  expect_true(length(
    k_folds(n = 100, k = 10)$train
  ) == 10)
})

test_that("the length of the returned sublist labeled 'val' is 2
          when k = 2", {
  expect_true(
    length(k_folds(n = 100, k = 2)$val) == 2
  )
})

test_that("the length of the returned sublist labeled 'val' is 10
          when k = 10", {
  expect_true(length(
    k_folds(n = 100, k = 10)$val
  ) == 10)
})

test_that("the lengths of the vectors in k = 10 length
           sublists train and val add up to 100 when n = 10", {
  expect_true(
    all.equal(
      sapply(k_folds(n = 100, k = 10)$val, length) +
        sapply(k_folds(n = 100, k = 10)$train, length),
      rep(100, 10)
    )
  )
})


test_that("all elements of the val sublist are vectors", {
  expect_true(all(sapply(k_folds(n = 100, k = 10)$val, is.vector)))
})

test_that("all elements of the train sublist are vectors", {
  expect_true(all(sapply(k_folds(n = 100, k = 10)$val, is.vector)))
})
