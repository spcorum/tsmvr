context("test-evaluate_model")

test_that("Star cannot be numeric", {
  expect_error(evaluate_model(Star = numeric(), Hat = matrix()))
})

test_that("Star cannot be a vector", {
  expect_error(evaluate_model(Star = c(), Hat = matrix()))
})

test_that("Star cannot be a string", {
  expect_error(evaluate_model(Star = "", Hat = matrix()))
})

test_that("Star cannot be a list", {
  expect_error(evaluate_model(Star = list(), Sigma = matrix()))
})

test_that("Hat cannot be numeric", {
  expect_error(evaluate_model(Star = matrix(), Hat = numaric()))
})

test_that("Hat cannot be a vector", {
  expect_error(evaluate_model(Star = matrix(), Hat = c()))
})

test_that("Hat cannot be a string", {
  expect_error(evaluate_model(Star = matrix(), Hat = ""))
})

test_that("Hat cannot be a list", {
  expect_error(evaluate_model(Star = matrix(), Hat = list()))
})

test_that("Sigma cannot be numeric", {
  expect_error(evaluate_model(Star = matrix(), Hat = matrix(), Sigma = numeric()))
})

test_that("Sigma cannot be a vector", {
  expect_error(evaluate_model(Star = matrix(), Hat = matrix(), Sigma = c()))
})

test_that("Sigma cannot be a string", {
  expect_error(evaluate_model(Star = matrix(), Hat = matrix(), Sigma = ""))
})

test_that("Sigma cannot be a list", {
  expect_error(evaluate_model(Star = matrix(), Hat = matrix(), Sigma = list()))
})

test_that("output squared_error is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$
    squared_error, 11 / 18)
})

test_that("output model_error is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$
    model_error, 1 / 2)
})

test_that("output number of positives is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$p, 8)
})

test_that("output number of negatives is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$n, 10)
})

test_that("output number of true positives is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$tp, 2)
})

test_that("output number of false positives is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$fp, 3)
})

test_that("output number of true negatives is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$tn, 7)
})

test_that("output number of false negatvies is as expected", {
  expect_equal(evaluate_model(
    Star = matrix(c(
      -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
      0, -1, 0, 0, 0.5, 0
    ), 6, 3),
    Hat = matrix(c(
      0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
      -1, 0.5, 0, 0
    ), 6, 3),
    Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
  )$fn, 6)
})

test_that("output true positive rate is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$tpr,
    1 / 4
  )
})

test_that("output false positive rate is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$fpr,
    3 / 8
  )
})

test_that("output true negative rate is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$tnr,
    7 / 10
  )
})

test_that("output false negative rate is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$fpr,
    3 / 8
  )
})

test_that("output accuracy is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$acc,
    1 / 2
  )
})

test_that("output area under the curve is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$auc,
    pROC::roc(
      response = as.vector((matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0,
        0, 0.5, 0, -0.5, 0, -1, 0.5,
        0, 0
      ), 6, 3) != 0) * 1),
      predictor = as.vector((matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0,
        1, 1, -0.5, 0, 0, -1, 0, 0,
        0.5, 0
      ), 6, 3) != 0) * 1)
    )$auc * 1
  )
})

test_that("output f1 score is as expected", {
  expect_equal(
    evaluate_model(
      Star = matrix(c(
        -1, 0.5, 0, 1, 0, 0, 0, 0, 1, 1, -0.5, 0,
        0, -1, 0, 0, 0.5, 0
      ), 6, 3),
      Hat = matrix(c(
        0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0.5, 0, -0.5, 0,
        -1, 0.5, 0, 0
      ), 6, 3),
      Sigma = matrix(c(1, -0.5, 0, -0.5, 1, -0.5, 0, -0.5, 1), 3, 3)
    )$f1,
    4 / 13
  )
})
