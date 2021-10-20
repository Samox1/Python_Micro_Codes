#Zadanie 1
library(Rcpp)
cppFunction({'NumericMatrix matrixCpp(NumericVector v, int row, int col, bool byrow=0){
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
'})

#Zadanie 2
cppFunction({'NumericVector cumsumCpp(NumericVector v){
  double a = 0;
  NumericVector vect(v.size());
  for(int i = 0; i < v.size(); i++){
    a += v[i];
    vect[i] = a;
  }
  return vect;
}
'})

#Zadanie 3
cppFunction({'NumericVector revCpp(NumericVector v){
  int a = v.size();
  NumericVector vect(a);
  for(int i = 0; i<a; i++){
        vect[i] = v[a-i-1];
    }
  return vect;
}
'})

#Zadanie 4
matmultR <- function(m1,m2){
  
  for()
}

# Zadanie 5
library(microbenchmark)
a <- matrix(c(5,6,7,8),2,2)
a
microbenchmark(matrix(c(5,6,7,8),2,2),matrixCpp(c(5,6,7,8),2,2))
microbenchmark(cumsum(1:30),cumsumCpp(1:30))
microbenchmark(rev(1:5), revCpp(1:5))

