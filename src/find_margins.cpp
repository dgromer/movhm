#include <algorithm>    // std::min
#include <math.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector find_margins(List l, String x, String y, double bin_width)
{
  // Length of the list
  int n = l.size();
  
  // Initialize variables holding margins with sensible values
  double xmin = as<NumericVector>(as<List>(l[0])[x])[0];
  double ymin = as<NumericVector>(as<List>(l[0])[y])[0];
  double xmax = as<NumericVector>(as<List>(l[0])[x])[0];
  double ymax = as<NumericVector>(as<List>(l[0])[y])[0];
  
  // Iterate through all lists in l
  for (int i = 0; i < n; i++)
  {
    // Extract current list element from l
    List current_list = as<List>(l[i]);
    
    // Extract x and y values from the current list
    NumericVector xs = as<NumericVector>(current_list[x]);
    NumericVector ys = as<NumericVector>(current_list[y]);
    
    // Check whether the current list contains more extreme margins
    xmin = std::min(xmin, static_cast<double>(min(xs)));
    ymin = std::min(ymin, static_cast<double>(min(ys)));
    xmax = std::max(xmax, static_cast<double>(max(xs)));
    ymax = std::max(ymax, static_cast<double>(max(ys)));
  }
  
  // Write results to vector
  NumericVector res = NumericVector::create(xmin, ymin, xmax, ymax);
  
  // Round values to bin_width
  for (int i = 0; i < res.size(); i++)
  {
    res[i] = round(res[i] / bin_width) * bin_width;
  }
  
  return res;
}

double round(double number)
{
  return number < 0.0 ? ceil(number - 0.5) : floor(number + 0.5);
}