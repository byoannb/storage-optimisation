// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ffd_bin_packing_Rcpp
std::vector<std::vector<int>> ffd_bin_packing_Rcpp(std::vector<int>& items, int bin_size);
RcppExport SEXP _StorageOptimisation_ffd_bin_packing_Rcpp(SEXP itemsSEXP, SEXP bin_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int>& >::type items(itemsSEXP);
    Rcpp::traits::input_parameter< int >::type bin_size(bin_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(ffd_bin_packing_Rcpp(items, bin_size));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StorageOptimisation_ffd_bin_packing_Rcpp", (DL_FUNC) &_StorageOptimisation_ffd_bin_packing_Rcpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_StorageOptimisation(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
