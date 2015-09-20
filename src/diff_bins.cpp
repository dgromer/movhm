#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector diff_bins(NumericVector x, NumericVector y, double len_x,
                        double len_y, String difference)
{
  // Number of rows in bins_*
  int n = x.size();
  
  // If relative difference is requested, then weight frequencies before 
  // substration
  double x_weight = 1.0, y_weight = 1.0;
  
  if (difference == "relative")
  {
    x_weight = (double)len_x;
    y_weight = (double)len_y;
  }
  
  NumericVector out(n);
  
  for (int i = 0; i < n; i++)
  {
    out[i] = x[i] / x_weight - y[i] / y_weight;
  }
  
  return out;
}
