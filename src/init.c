#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _tsmvr_ht(SEXP, SEXP, SEXP);
extern SEXP _tsmvr_project_pdc(SEXP, SEXP);
extern SEXP _tsmvr_RcppExport_registerCCallable();
extern SEXP _tsmvr_tsmvr_solve(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
    {"_tsmvr_ht",                           (DL_FUNC) &_tsmvr_ht,                            3},
    {"_tsmvr_project_pdc",                  (DL_FUNC) &_tsmvr_project_pdc,                   2},
    {"_tsmvr_RcppExport_registerCCallable", (DL_FUNC) &_tsmvr_RcppExport_registerCCallable,  0},
    {"_tsmvr_tsmvr_solve",                  (DL_FUNC) &_tsmvr_tsmvr_solve,                  17},
    {"run_testthat_tests",                  (DL_FUNC) &run_testthat_tests,                   0},
    {NULL, NULL, 0}
};

void R_init_tsmvr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
