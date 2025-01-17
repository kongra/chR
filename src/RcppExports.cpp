// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// chDiagnCount
std::uint64_t chDiagnCount();
RcppExport SEXP _chR_chDiagnCount() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(chDiagnCount());
    return rcpp_result_gen;
END_RCPP
}
// chDiagnCountReset
void chDiagnCountReset();
RcppExport SEXP _chR_chDiagnCountReset() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    chDiagnCountReset();
    return R_NilValue;
END_RCPP
}
// chDiagnCountInc
void chDiagnCountInc();
RcppExport SEXP _chR_chDiagnCountInc() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    chDiagnCountInc();
    return R_NilValue;
END_RCPP
}
// chLL
SEXP chLL(const Function pred, const SEXP x, const bool asPred, const Function errMessage);
RcppExport SEXP _chR_chLL(SEXP predSEXP, SEXP xSEXP, SEXP asPredSEXP, SEXP errMessageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Function >::type pred(predSEXP);
    Rcpp::traits::input_parameter< const SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type asPred(asPredSEXP);
    Rcpp::traits::input_parameter< const Function >::type errMessage(errMessageSEXP);
    rcpp_result_gen = Rcpp::wrap(chLL(pred, x, asPred, errMessage));
    return rcpp_result_gen;
END_RCPP
}
// arePosInts
bool arePosInts(const IntegerVector xs);
RcppExport SEXP _chR_arePosInts(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(arePosInts(xs));
    return rcpp_result_gen;
END_RCPP
}
// arePosIntsOrNAs
bool arePosIntsOrNAs(const IntegerVector xs);
RcppExport SEXP _chR_arePosIntsOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(arePosIntsOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNegInts
bool areNegInts(const IntegerVector xs);
RcppExport SEXP _chR_areNegInts(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNegInts(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNegIntsOrNAs
bool areNegIntsOrNAs(const IntegerVector xs);
RcppExport SEXP _chR_areNegIntsOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNegIntsOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNatInts
bool areNatInts(const IntegerVector xs);
RcppExport SEXP _chR_areNatInts(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNatInts(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNatIntsOrNAs
bool areNatIntsOrNAs(const IntegerVector xs);
RcppExport SEXP _chR_areNatIntsOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const IntegerVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNatIntsOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}
// arePosDoubles
bool arePosDoubles(const DoubleVector xs);
RcppExport SEXP _chR_arePosDoubles(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(arePosDoubles(xs));
    return rcpp_result_gen;
END_RCPP
}
// arePosDoublesOrNAs
bool arePosDoublesOrNAs(const DoubleVector xs);
RcppExport SEXP _chR_arePosDoublesOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(arePosDoublesOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNegDoubles
bool areNegDoubles(const DoubleVector xs);
RcppExport SEXP _chR_areNegDoubles(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNegDoubles(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNegDoublesOrNAs
bool areNegDoublesOrNAs(const DoubleVector xs);
RcppExport SEXP _chR_areNegDoublesOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNegDoublesOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNonNegDoubles
bool areNonNegDoubles(const DoubleVector xs);
RcppExport SEXP _chR_areNonNegDoubles(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNonNegDoubles(xs));
    return rcpp_result_gen;
END_RCPP
}
// areNonNegDoublesOrNAs
bool areNonNegDoublesOrNAs(const DoubleVector xs);
RcppExport SEXP _chR_areNonNegDoublesOrNAs(SEXP xsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const DoubleVector >::type xs(xsSEXP);
    rcpp_result_gen = Rcpp::wrap(areNonNegDoublesOrNAs(xs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_chR_chDiagnCount", (DL_FUNC) &_chR_chDiagnCount, 0},
    {"_chR_chDiagnCountReset", (DL_FUNC) &_chR_chDiagnCountReset, 0},
    {"_chR_chDiagnCountInc", (DL_FUNC) &_chR_chDiagnCountInc, 0},
    {"_chR_chLL", (DL_FUNC) &_chR_chLL, 4},
    {"_chR_arePosInts", (DL_FUNC) &_chR_arePosInts, 1},
    {"_chR_arePosIntsOrNAs", (DL_FUNC) &_chR_arePosIntsOrNAs, 1},
    {"_chR_areNegInts", (DL_FUNC) &_chR_areNegInts, 1},
    {"_chR_areNegIntsOrNAs", (DL_FUNC) &_chR_areNegIntsOrNAs, 1},
    {"_chR_areNatInts", (DL_FUNC) &_chR_areNatInts, 1},
    {"_chR_areNatIntsOrNAs", (DL_FUNC) &_chR_areNatIntsOrNAs, 1},
    {"_chR_arePosDoubles", (DL_FUNC) &_chR_arePosDoubles, 1},
    {"_chR_arePosDoublesOrNAs", (DL_FUNC) &_chR_arePosDoublesOrNAs, 1},
    {"_chR_areNegDoubles", (DL_FUNC) &_chR_areNegDoubles, 1},
    {"_chR_areNegDoublesOrNAs", (DL_FUNC) &_chR_areNegDoublesOrNAs, 1},
    {"_chR_areNonNegDoubles", (DL_FUNC) &_chR_areNonNegDoubles, 1},
    {"_chR_areNonNegDoublesOrNAs", (DL_FUNC) &_chR_areNonNegDoublesOrNAs, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_chR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
