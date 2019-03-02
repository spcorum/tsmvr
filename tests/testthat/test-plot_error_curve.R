context("test-plot_error_curve")

test_that("est cannot be a string", {
  expect_error(plot_error_curve(est = "", tru = 1))
})

test_that("est cannot be a list", {
  expect_error(plot_error_curve(est = list(), tru = 1))
})

test_that("est cannot be NULL", {
  expect_error(plot_error_curve(est = NULL, tru = 1))
})

test_that("tru cannot be a string", {
  expect_error(plot_error_curve(est = 1, tru = ""))
})

test_that("tru cannot be a list", {
  expect_error(plot_error_curve(est = 1, tru = list()))
})

test_that("tru cannot be NULL", {
  expect_error(plot_error_curve(est = 1, tru = NULL))
})

test_that("est and tru must have the same length", {
  est <- rep(0, sample.int(1000, 1))
  tru <- rep(0, sample.int(1000, 1))
  expect_error(plot_error_curve(est, tru))
})

test_that("up cannot be non-integer valued numeric", {
  expect_error(plot_error_curve(est = 1, tru = 1, up = 0.5))
})

test_that("low cannot be non-integer valued numeric", {
  expect_error(plot_error_curve(est = 1, tru = 1, low = 0.5))
})

test_that("left cannot be non-integer valued numeric", {
  expect_error(plot_error_curve(est = 1, tru = 1, left = 0.5))
})

test_that("right cannot be non-integer valued numeric", {
  expect_error(plot_error_curve(est = 1, tru = 1, right = 0.5))
})

test_that("up cannot be less than low", {
  low <- runif(1) * 200 - 100
  up <- low - 1e6
  expect_error(plot_error_curve(
    est = 1, tru = 1,
    up = up, low = low
  ))
})

test_that("left cannot be less than right", {
  right <- runif(1) * 200 - 100
  left <- right - 1e6
  expect_error(plot_error_curve(
    est = 1, tru = 1,
    left = left, right = right
  ))
})

test_that("ggplot object is returned", {
  library(ggplot2)
  expect_true(is.ggplot(plot_error_curve(est = 1, tru = 1)))
})
