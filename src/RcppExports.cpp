// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// tsmvr_solve
List tsmvr_solve(const arma::mat& X, const arma::mat& Y, const int& s1, const int& s2, const String& BType, const String& OmegaType, const double& eta1, const double& eta2, const double& epsilon, const int& maxiter, const int& skip, const bool& quiet);
RcppExport SEXP _tsmvr_tsmvr_solve(SEXP XSEXP, SEXP YSEXP, SEXP s1SEXP, SEXP s2SEXP, SEXP BTypeSEXP, SEXP OmegaTypeSEXP, SEXP eta1SEXP, SEXP eta2SEXP, SEXP epsilonSEXP, SEXP maxiterSEXP, SEXP skipSEXP, SEXP quietSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const int& >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< const int& >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< const String& >::type BType(BTypeSEXP);
    Rcpp::traits::input_parameter< const String& >::type OmegaType(OmegaTypeSEXP);
    Rcpp::traits::input_parameter< const double& >::type eta1(eta1SEXP);
    Rcpp::traits::input_parameter< const double& >::type eta2(eta2SEXP);
    Rcpp::traits::input_parameter< const double& >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< const int& >::type maxiter(maxiterSEXP);
    Rcpp::traits::input_parameter< const int& >::type skip(skipSEXP);
    Rcpp::traits::input_parameter< const bool& >::type quiet(quietSEXP);
    rcpp_result_gen = Rcpp::wrap(tsmvr_solve(X, Y, s1, s2, BType, OmegaType, eta1, eta2, epsilon, maxiter, skip, quiet));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tsmvr_tsmvr_solve", (DL_FUNC) &_tsmvr_tsmvr_solve, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_tsmvr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}