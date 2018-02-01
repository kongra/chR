// Copyright (c) Konrad Grzanek
// Created 2017-08-12
//
#include <atomic>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// CALLING THE PREDICATES
//
static std::atomic<std::uint64_t> chCallsCounter(0);

//' Returns the value of calls counter
//' @export
// [[Rcpp::export]]
std::uint64_t callsCount()
{
  return chCallsCounter;
}

//' Low Level ch(eck) call
//' @export
// [[Rcpp::export]]
SEXP chLL(const Function pred,
          const SEXP x,
          const bool asPred,
          const Function errMessage) {
  chCallsCounter++;
  const SEXP r = pred(x);
  if (asPred) return r;
  if (!as<bool>(r)) stop(as<const char*>(errMessage(x)));
  return x;
}

// ABSTRACTION FOR CHECKING PREDICATES ON VECTORS
//
template<typename V, typename F>
static inline bool everyInVector(const V xs, const F&& pred) {
  const int n = xs.size();
  for (int i = 0; i < n; i++) if (!pred(xs[i])) return false;
  return true;
}

template<typename V, typename F>
static inline bool everyInVectorOrNAs(const V xs, const F&& pred) {
  const int n = xs.size();
  for (int i = 0; i < n; i++) {
    if (V::is_na(xs[i])) continue;
    if (!pred   (xs[i])) return false;
  }
  return true;
}

// INTEGRALS
//

//' Returns true iff all the xs are positive
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool arePosInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n > 0; });
}

//' Returns true iff all the xs are positive or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool arePosIntsOrNAs(const IntegerVector xs) {
  return everyInVectorOrNAs(xs, [](int n) { return n > 0; });
}

//' Returns true iff all the xs are negative
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNegInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n < 0; });
}

//' Returns true iff all the xs are negative or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNegIntsOrNAs(const IntegerVector xs) {
  return everyInVectorOrNAs(xs, [](int n) { return n < 0; });
}

//' Returns true iff all the xs are naturals (>= 0)
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNatInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n >= 0; });
}

//' Returns true iff all the xs are naturals (>= 0) or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNatIntsOrNAs(const IntegerVector xs) {
  return everyInVectorOrNAs(xs, [](int n) { return n >= 0; });
}

// DOUBLES
//

//' Returns true iff all the xs are positive
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool arePosDoubles(const DoubleVector xs) {
  return everyInVector(xs, [](double d) { return d > 0; });
}

//' Returns true iff all the xs are positive or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool arePosDoublesOrNAs(const DoubleVector xs) {
  return everyInVectorOrNAs(xs, [](double d) { return d > 0; });
}

//' Returns true iff all the xs are negative
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNegDoubles(const DoubleVector xs) {
  return everyInVector(xs, [](double d) { return d < 0; });
}

//' Returns true iff all the xs are negative or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNegDoublesOrNAs(const DoubleVector xs) {
  return everyInVectorOrNAs(xs, [](double d) { return d < 0; });
}

//' Returns true iff all the xs are non-negative
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNonNegDoubles(const DoubleVector xs) {
  return everyInVector(xs, [](double d) { return d >= 0; });
}

//' Returns true iff all the xs are non-negative or NAs
//' @param xs vector to check
//' @return true or false
//' @export
// [[Rcpp::export]]
bool areNonNegDoublesOrNAs(const DoubleVector xs) {
  return everyInVectorOrNAs(xs, [](double d) { return d >= 0; });
}
