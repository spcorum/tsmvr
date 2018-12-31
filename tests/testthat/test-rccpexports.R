context("test-rccpexports")

# test_that("X cannot be numeric", {
#   set.seed(1)
#   n = 100
#   p = 10
#   q = 3
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   X = 1
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("X cannot be a character string", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   X = ''
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("X cannot be a list()", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   X = list()
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("X cannot be a vector", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   X = rep(0,2)
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("Y cannot be a character string", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   Y = ''
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("Y cannot be a character string", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   Y = ''
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("Y cannot be a list()", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   Y = list()
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
#
# test_that("Y cannot be a vector", {
#   n = 100
#   p = 10
#   q = 3
#   set.seed(1)
#   X = matrix(rnorm(n = 1000, sd = 0.1), n, p)
#   Y = matrix(rnorm(n = 300, mean = 1), n, q)
#   s1 = round(0.9 * p * q)
#   s2 = q + 2 * (q - 1)
#   Y = rep(0,2)
#   expect_error(tsmvr_solve(X, Y, s1, s2))
# })
