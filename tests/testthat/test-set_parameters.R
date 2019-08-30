context("test-set_parameters")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("B_type is not numeric", {
  expect_error(
    set_parameters(B_type = numeric(0))
  )
})

test_that("B_type is not a list", {
  expect_error(
    set_parameters(B_type = list())
  )
})

test_that("Omega_type is not numeric", {
  expect_error(
    set_parameters(Omega_type = numeric(0))
  )
})

test_that("Omega_type is not a list", {
  expect_error(
    set_parameters(Omega_type = list())
  )
})

test_that("eta1 is not a string", {
  expect_error(
    set_parameters(eta1 = "")
  )
})

test_that("eta1 is not a list", {
  expect_error(
    set_parameters(eta1 = list())
  )
})

test_that("eta2 is not a string", {
  expect_error(
    set_parameters(eta2 = "")
  )
})

test_that("eta2 is not a list", {
  expect_error(
    set_parameters(eta2 = list())
  )
})

test_that("lam1 is not a string", {
  expect_error(
    set_parameters(lam1 = "")
  )
})

test_that("lam1 is not a list", {
  expect_error(
    set_parameters(lam1 = list())
  )
})

test_that("lam2 is not a string", {
  expect_error(
    set_parameters(lam2 = "")
  )
})

test_that("lam2 is not a list", {
  expect_error(
    set_parameters(lam2 = list())
  )
})

test_that("del1 is not a string", {
  expect_error(
    set_parameters(del1 = "")
  )
})

test_that("del1 is not a list", {
  expect_error(
    set_parameters(del1 = list())
  )
})

test_that("del2 is not a string", {
  expect_error(
    set_parameters(del2 = "")
  )
})

test_that("del2 is not a list", {
  expect_error(
    set_parameters(del2 = list())
  )
})

test_that("del3 is not a string", {
  expect_error(
    set_parameters(del2 = "")
  )
})

test_that("del3 is not a list", {
  expect_error(
    set_parameters(del2 = list())
  )
})

test_that("rho1 is not a string", {
  expect_error(
    set_parameters(rho1 = "")
  )
})

test_that("rho1 is not a list", {
  expect_error(
    set_parameters(rho1 = list())
  )
})

test_that("rho2 is not a string", {
  expect_error(
    set_parameters(rho2 = "")
  )
})

test_that("rho2 is not a list", {
  expect_error(
    set_parameters(rho2 = list())
  )
})

test_that("beta1 is not a string", {
  expect_error(
    set_parameters(beta1 = "")
  )
})

test_that("beta2 is not a list()", {
  expect_error(
    set_parameters(beta1 = list())
  )
})

test_that("beta2 is not a string", {
  expect_error(
    set_parameters(beta2 = "")
  )
})

test_that("beta2 is not a list", {
  expect_error(
    set_parameters(beta2 = list())
  )
})

test_that("qmax1 is not a string", {
  expect_error(
    set_parameters(qmax1 = "")
  )
})

test_that("qmax1 is not a list", {
  expect_error(
    set_parameters(qmax1 = list())
  )
})

test_that("qmax2 is not a string", {
  expect_error(
    set_parameters(qmax2 = "")
  )
})

test_that("eps1 is not a string", {
  expect_error(
    set_parameters(eps1 = "")
  )
})

test_that("eps1 is not a list()", {
  expect_error(
    set_parameters(eps1 = list())
  )
})

test_that("eps2 is not a string", {
  expect_error(
    set_parameters(eps2 = "")
  )
})

test_that("eps2 is not a list", {
  expect_error(
    set_parameters(eps2 = list())
  )
})

test_that("max_iter is not a string", {
  expect_error(
    set_parameters(max_iter = "")
  )
})

test_that("max_iter is not a list", {
  expect_error(
    set_parameters(max_iter = list())
  )
})

test_that("skip is not a string", {
  expect_error(
    set_parameters(skip = "")
  )
})

test_that("skip is not a list", {
  expect_error(
    set_parameters(skip = list())
  )
})

test_that("quiet is not numeric", {
  expect_error(
    set_parameters(quiet = numeric(0))
  )
})

test_that("quiet is not a string", {
  expect_error(
    set_parameters(quiet = "")
  )
})

test_that("quiet is not a list", {
  expect_error(
    set_parameters(quiet = list())
  )
})

test_that("suppress is not numeric", {
  expect_error(
    set_parameters(suppress = numeric(0))
  )
})

test_that("suppress is not a string", {
  expect_error(
    set_parameters(suppress = "")
  )
})

test_that("disp_min_ev is not a list", {
  expect_error(
    set_parameters(disp_min_ev = list())
  )
})

test_that("disp_min_ev is not numeric", {
  expect_error(
    set_parameters(disp_min_ev = numeric(0))
  )
})

test_that("disp_min_ev is not a string", {
  expect_error(
    set_parameters(disp_min_ev = "")
  )
})

test_that("suppress is not a list", {
  expect_error(
    set_parameters(suppress = list())
  )
})

test_that("save_history is not numeric", {
  expect_error(
    set_parameters(save_history = numeric(0))
  )
})

test_that("save_history is not a string", {
  expect_error(
    set_parameters(save_history = "")
  )
})

test_that("save_history is not a list", {
  expect_error(
    set_parameters(save_history = list())
  )
})

test_that("set_parameters returns a list, the length of that list is 10,
          and the labels of that list are as expected", {
  z <- set_parameters()
  expect_true(
    all(
      is.list(z), length(z) == 25,
      all(labels(z) == c(
        "B_type", "Omega_type", "eta1", "eta2",
        "lam1", "lam2", "del1", "del2", "del3",
        "rho1", "rho2", "beta1", "beta2",
        "qmax1", "qmax2", "eps1", "eps2",
        "k", "reps", "max_iter", "skip", "quiet",
        "suppress", "disp_min_ev",
        "save_history"
      ))
    )
  )
})

test_that("B_type is a string", {
  expect_true(
    is.character(set_parameters()$B_type)
  )
})

test_that("Omega_type is a string", {
  expect_true(
    is.character(set_parameters()$B_type)
  )
})

test_that("eta1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$eta1)
  )
})

test_that("eta1 is positive", {
  expect_true(
    set_parameters()$eta1 > 0
  )
})

test_that("eta2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$eta2)
  )
})

test_that("eta1 is positive", {
  expect_true(
    set_parameters()$eta2 > 0
  )
})

test_that("del1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$del1)
  )
})

test_that("del1 is not negative", {
  expect_true(
    set_parameters()$del1 >= 0
  )
})

test_that("del2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$del2)
  )
})

test_that("del2 is not negative", {
  expect_true(
    set_parameters()$del2 >= 0
  )
})

test_that("del3 is numeric", {
  expect_true(
    is.numeric(set_parameters()$del3)
  )
})

test_that("del3 is not negative", {
  expect_true(
    set_parameters()$del3 >= 0
  )
})

test_that("rho1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$rho1)
  )
})

test_that("rho1 is not negative", {
  expect_true(
    set_parameters()$rho1 >= 0
  )
})

test_that("rho2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$rho2)
  )
})

test_that("rho2 is not negative", {
  expect_true(
    set_parameters()$rho2 >= 0
  )
})

test_that("beta1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$beta1)
  )
})

test_that("beta1 is not negative", {
  expect_true(
    set_parameters()$beta1 >= 0
  )
})

test_that("beta2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$beta2)
  )
})

test_that("beta2 is not negative", {
  expect_true(
    set_parameters()$beta2 >= 0
  )
})

test_that("qmax1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$qmax1)
  )
})

test_that("qmax1 is positive", {
  expect_true(
    set_parameters()$qmax1 > 0
  )
})

test_that("qmax2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$qmax2)
  )
})

test_that("qmax2 is positive", {
  expect_true(
    set_parameters()$qmax2 > 0
  )
})

test_that("qmax1 is integer", {
  expect_true(
    set_parameters()$qmax1 %% 1 == 0
  )
})

test_that("qmax2 is integer", {
  expect_true(
    set_parameters()$qmax2 %% 1 == 0
  )
})

test_that("eps1 is positive", {
  expect_true(
    set_parameters()$eps1 > 0
  )
})

test_that("eps1 is numeric", {
  expect_true(
    is.numeric(set_parameters()$eps1)
  )
})

test_that("eps2 is positive", {
  expect_true(
    set_parameters()$eps2 > 0
  )
})

test_that("eps2 is numeric", {
  expect_true(
    is.numeric(set_parameters()$eps2)
  )
})

test_that("max_iter is numeric", {
  expect_true(
    is.numeric(set_parameters()$max_iter)
  )
})

test_that("max_iter is positive", {
  expect_true(
    set_parameters()$max_iter > 0
  )
})

test_that("max_iter is integer", {
  expect_true(
    set_parameters()$max_iter %% 1 == 0
  )
})

test_that("skip is numeric", {
  expect_true(
    is.numeric(set_parameters()$skip)
  )
})

test_that("skip is positive", {
  expect_true(
    set_parameters()$skip > 0
  )
})

test_that("skip is integer", {
  expect_true(
    set_parameters()$skip %% 1 == 0
  )
})
