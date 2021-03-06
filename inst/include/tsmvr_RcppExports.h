// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_tsmvr_RCPPEXPORTS_H_GEN_
#define RCPP_tsmvr_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace tsmvr {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("tsmvr", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("tsmvr", "_tsmvr_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in tsmvr");
            }
        }
    }

    inline arma::mat ht(arma::mat X, int s, bool ss = false) {
        typedef SEXP(*Ptr_ht)(SEXP,SEXP,SEXP);
        static Ptr_ht p_ht = NULL;
        if (p_ht == NULL) {
            validateSignature("arma::mat(*ht)(arma::mat,int,bool)");
            p_ht = (Ptr_ht)R_GetCCallable("tsmvr", "_tsmvr_ht");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ht(Shield<SEXP>(Rcpp::wrap(X)), Shield<SEXP>(Rcpp::wrap(s)), Shield<SEXP>(Rcpp::wrap(ss)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline List tsmvr_solve(const arma::mat& X, const arma::mat& Y, const int& s1, const int& s2, const Rcpp::List& pars) {
        typedef SEXP(*Ptr_tsmvr_solve)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_tsmvr_solve p_tsmvr_solve = NULL;
        if (p_tsmvr_solve == NULL) {
            validateSignature("List(*tsmvr_solve)(const arma::mat&,const arma::mat&,const int&,const int&,const Rcpp::List&)");
            p_tsmvr_solve = (Ptr_tsmvr_solve)R_GetCCallable("tsmvr", "_tsmvr_tsmvr_solve");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_tsmvr_solve(Shield<SEXP>(Rcpp::wrap(X)), Shield<SEXP>(Rcpp::wrap(Y)), Shield<SEXP>(Rcpp::wrap(s1)), Shield<SEXP>(Rcpp::wrap(s2)), Shield<SEXP>(Rcpp::wrap(pars)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<List >(rcpp_result_gen);
    }

}

#endif // RCPP_tsmvr_RCPPEXPORTS_H_GEN_
