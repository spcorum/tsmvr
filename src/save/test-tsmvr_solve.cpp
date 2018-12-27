/*

 * This file uses the Catch unit testing library, alongside
 * testthat's simple bindings, to test a C++ function.
 *
 * For your own packages, ensure that your test files are
 * placed within the `src/` folder, and that you include
 * `LinkingTo: testthat` within your DESCRIPTION file.



// All test files should include the <testthat.h>
// header file.
#include <testthat.h>
#include <tsmvr.h>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// Normally this would be a function from your package's
// compiled library -- you might instead just include a header
// file providing the definition, and let R CMD INSTALL
// handle building and linking.
int twoPlusTwo() {
  return 2 + 2;
}

// Initialize a unit test context. This is similar to how you
// might begin an R test file with 'context()', expect the
// associated context should be wrapped in braced.
context("Sample unit tests") {

  // The format for specifying tests is similar to that of
  // testthat's R functions. Use 'test_that()' to define a
  // unit test, and use 'expect_true()' and 'expect_false()'
  // to test the desired conditions.
  test_that("two plus two equals four") {
    expect_true(twoPlusTwo() == 4);
  }
}

// Initialize a unit test context. This is similar to how you
// might begin an R test file with 'context()', expect the
// associated context should be wrapped in braced.
context("Sample unit tests") {

  // The format for specifying tests is similar to that of
  // testthat's R functions. Use 'test_that()' to define a
  // unit test, and use 'expect_true()' and 'expect_false()'
  // to test the desired conditions.
  test_that("returns the positive part of a matrix") {
    arma::mat X;
    arma::mat Y;
    X <<  0     <<  0.880 << -0.680 << -0.084 << endr
      << -0.835 <<  0.320 << -0.770 <<  0.185 << endr
      << -0.326 << -0.949 << -0.400 <<  0.003 << endr
      << -0.406 <<  1.142 <<  0.123 <<  0     << endr;
    Y <<  0     <<  0.880 <<  0     <<  0     << endr
      <<  0     <<  0.320 <<  0     <<  0.185 << endr
      <<  0     <<  0     <<  0     <<  0.003 << endr
      <<  0     <<  1.142 <<  0.123 <<  0     << endr;
    expect_true(tsmvr::ppmat(X) == Y);
  }
}

*/
