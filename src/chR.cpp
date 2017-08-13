// Copyright (c) Konrad Grzanek
// Created 2017-08-12
//
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// ABSTRACTION FOR CHECKING PREDICATES ON VECTORS
//

template<typename V, typename F>
static inline bool everyInVector(const V xs, const F&& pred) {
  const int n = xs.size();
  for (int i = 0; i < n; i++) if (!pred(xs[i])) return false;
  return true;
}

// INTEGRALS
//

//' Returns true iff all the xs are positive
//' @param xs
//' @export
// [[Rcpp::export]]
bool arePosInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n > 0; });
}

//' Returns true iff all the xs are negative
//' @param xs
//' @export
// [[Rcpp::export]]
bool areNegInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n < 0; });
}

//' Returns true iff all the xs are naturals (>= 0)
//' @param xs
//' @export
// [[Rcpp::export]]
bool areNatInts(const IntegerVector xs) {
  return everyInVector(xs, [](int n) { return n >= 0; });
}

// DOUBLES
//
