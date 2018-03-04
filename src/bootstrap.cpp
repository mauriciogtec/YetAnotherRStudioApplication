#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

//' @title parametric bootstrap regression
//' @export
// [[Rcpp::export]]
Rcpp::List bootstrap_robust_regression(
      arma::mat &X,
      arma::vec &y,
      int times
    ){
  uword n = X.n_rows, p = X.n_cols, B = times;
  
  // Step 1: Obtain the projection matrix and leverages
  mat pseudo_inv = pinv(X);
  mat proj_mat = X * pseudo_inv;
  vec leverage = proj_mat.diag();
  
  // Step 2: Find coefficients and robust residuals
  vec coeffs = pseudo_inv * y;
  vec fitted = X * coeffs;
  vec err = y - fitted;
  
  // Step 3: Modified residuals
  vec rres = err / sqrt(1 - leverage);
  vec centered_rres = rres - mean(rres);
  
  // Step 4: Resample obtained bootstrapped coefficients
  mat b_coefs(B, p);
  vec b_se(B);
  for (uword i = 0; i < B; i++) {
    // Sample indices
    uvec idx(n); 
    for (uword i = 0; i < n; i++) {
      idx(i) = rand() % n;  // main area of opportunity for improvement
    }
    vec y_i = fitted + centered_rres.elem(idx);
    // Fit regression
    vec coefs_i =  pseudo_inv * y_i;
    b_coefs.row(i) =  coefs_i.t();
    vec err_i = y_i - X * coefs_i;
    b_se(i) = sqrt(sum(square(err_i)) / (n - p)) ;
  }

  return Rcpp::List::create(Named("coefficients") = wrap(b_coefs),
                            Named("se") = wrap(b_se),
                            Named("residuals") = wrap(err),
                            Named("leverages") = wrap(leverage),
                            Named("modified residuals") = wrap(rres));
}
