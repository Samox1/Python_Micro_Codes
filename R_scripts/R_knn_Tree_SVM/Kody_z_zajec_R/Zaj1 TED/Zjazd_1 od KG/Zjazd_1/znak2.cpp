#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
String znak2( double x ){
  if( x < 0 ){ return("Liczba jest ujemna"); }
  else if( x > 0 ){ return("Liczba jest dodatnia"); }
  else { return("Liczba jest równa 0"); }
}

/*** R
znak2(42)
*/
