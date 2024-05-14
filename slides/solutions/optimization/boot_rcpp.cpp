#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector
boot_rcpp(const DataFrame& population,
          const IntegerVector& bootstrap_index,
          int bootstrap_size) {
  NumericVector median_vals(bootstrap_index.size());
  LogicalVector analysis_flag = population["analysis_flag"];
  NumericVector dummy_measurement = population["dummy_measurement"];
  
  for (int i = 0; i < bootstrap_index.size(); i++) {
    // I found this helpful:
    // https://dirk.eddelbuettel.com/code/rcpp/html/sample_8h_source.html
    IntegerVector bootstrap_data_inds = sample(
      population.nrow(), 
      bootstrap_size, 
      true, // replace
      R_NilValue,
      false // not 1-based, so 0-based, because indexing in C++ is 0 based below.
    );
    LogicalVector bootstrap_flags = analysis_flag[bootstrap_data_inds];
    NumericVector bootstrap_vals = dummy_measurement[bootstrap_data_inds];
    NumericVector analysis_pop_vals = bootstrap_vals[bootstrap_flags];
    median_vals[i] = median(analysis_pop_vals);
  }
  return median_vals;
}
