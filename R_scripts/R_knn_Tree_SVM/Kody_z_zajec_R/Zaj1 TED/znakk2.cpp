#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
String znakk2(double x){
	if (x>0) {return ("Ech");}
	else if (x<0) {return("Och");}
	else {return("Uch");}
}

/*** R
znakk2(42)
*/

