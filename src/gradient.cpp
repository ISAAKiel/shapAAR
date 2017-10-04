#include <Rcpp.h>
using namespace Rcpp;

//' A simple script to get the central difference gradient of a (image) matrix
//'
//' @param x a matrix representing the image
//' @return the first derivative (central difference gradient) of that matrix
//'
//' @export
// [[Rcpp::export]]
NumericMatrix grad1_c(NumericMatrix x) {
  // cache rows and columns
  int nrow_x = x.nrow();
  int ncol_x = x.ncol();

  // initialise the output matrix
  NumericMatrix out(nrow_x,ncol_x);

  // for every rows
  for (int i = 0; i < nrow_x; ++i)
    // for every column except first and last
    for (int t = 1; t < ncol_x-1; ++t)
      // value at i,j is difference between its neigbours
      out(i,t) = (x(i,t+1) - x(i,t-1))/2;

  // populate first and last column with difference of self to 1 neighbour
  out(_,0) = x(_,1) - x(_,0);
  out(_,ncol_x-1) = x(_,ncol_x-1) - x(_,ncol_x-2);

  // and return
  return out;
}