#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix matrixCpp(NumericVector v, int row, int col, bool byrow=0){
	int k=0;
	NumericMatrix macierz(row,col);
	if (byrow=0){
		for(int i=0;i<macierz.ncol();i++){
		for(int j=0;j<macierz.nrow();i++){
			macierz(j,i)=v[k];
			k++;
			}
		}
	}
		else{
		for(int i=0;i<macierz.nrow();i++){
		for(int j=0;j<macierz.ncol();i++){
			macierz(i,j)=v[k];
			k++;
				}
			}
		}
	return (macierz);
}

/*** R
matrixCpp(1:10,3,4,T)
*/



