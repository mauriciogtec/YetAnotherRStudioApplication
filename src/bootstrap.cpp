#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace arma;
using namespace std;


// //' @title resample with replacement
// //' @description Provides the most basic scheme of bootstramp resample with replacement
// // [Rcpp::export]
// std::list<std::vector<int>> generate_resamples(int n, int times) {
//   std::list<std::vector<int>>
//   for (int i = 0; i < times; i ++) {
//     Rcpp::IntegerVector idx;
//     for (int k = 0; k < n; k++) {
//       idx.push_back()
//     }
//     l.push_back(idx);
//   }
// }