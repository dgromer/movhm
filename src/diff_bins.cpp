#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector diff_bins(NumericVector x, NumericVector y, double len_x,
                        double len_y, String difference)
{
  // Number of rows in *$f
  int n = x.size();

  // If relative difference is requested, then weight frequencies before
  // substration. A score of 1 then means, that all of group 1 visited the
  // cell and none of group 2.
  double x_weight = 1.0, y_weight = 1.0;

  if (difference == "relative")
  {
    x_weight = (double)len_x;
    y_weight = (double)len_y;
  }

  NumericVector out(n);

  for (int i = 0; i < n; i++)
  {
    if (R_IsNA(x[i]) && R_IsNA(y[i]))
    {
      out[i] = NA_REAL;
    }
    else if (R_IsNA(x[i]))
    {
      out[i] = 0 - y[i] / y_weight;
    }
    else if (R_IsNA(x[i]))
    {
      out[i] = 0 - x[i] / x_weight;
    }
    else
    {
      out[i] = x[i] / x_weight - y[i] / y_weight;
    }
  }

  return out;
}
