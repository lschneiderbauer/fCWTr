// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fcwt_raw
std::vector<float> fcwt_raw(std::vector<float> input, int startoctave, int noctaves, int nsuboctaves, float sigma, bool optplans);
RcppExport SEXP _fcwtr_fcwt_raw(SEXP inputSEXP, SEXP startoctaveSEXP, SEXP noctavesSEXP, SEXP nsuboctavesSEXP, SEXP sigmaSEXP, SEXP optplansSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<float> >::type input(inputSEXP);
    Rcpp::traits::input_parameter< int >::type startoctave(startoctaveSEXP);
    Rcpp::traits::input_parameter< int >::type noctaves(noctavesSEXP);
    Rcpp::traits::input_parameter< int >::type nsuboctaves(nsuboctavesSEXP);
    Rcpp::traits::input_parameter< float >::type sigma(sigmaSEXP);
    Rcpp::traits::input_parameter< bool >::type optplans(optplansSEXP);
    rcpp_result_gen = Rcpp::wrap(fcwt_raw(input, startoctave, noctaves, nsuboctaves, sigma, optplans));
    return rcpp_result_gen;
END_RCPP
}
// create_optimization_schemes_raw
void create_optimization_schemes_raw(int maxsize, int threads, String flag);
RcppExport SEXP _fcwtr_create_optimization_schemes_raw(SEXP maxsizeSEXP, SEXP threadsSEXP, SEXP flagSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type maxsize(maxsizeSEXP);
    Rcpp::traits::input_parameter< int >::type threads(threadsSEXP);
    Rcpp::traits::input_parameter< String >::type flag(flagSEXP);
    create_optimization_schemes_raw(maxsize, threads, flag);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fcwtr_fcwt_raw", (DL_FUNC) &_fcwtr_fcwt_raw, 6},
    {"_fcwtr_create_optimization_schemes_raw", (DL_FUNC) &_fcwtr_create_optimization_schemes_raw, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_fcwtr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
