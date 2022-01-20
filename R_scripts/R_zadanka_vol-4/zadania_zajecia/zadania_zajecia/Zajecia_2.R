# Integracja R z C++
install.packages("Rcpp")
library("Rcpp")

cppFunction({
' 
  String znak1 ( double x ){
    if( x > 0 ){ return("liczba dodatnia"); }
    else if( x < 0 ){ return("liczba ujemna"); }
    else{ return("liczba rowna 0"); }
  }
'
})
znak1
znak1(-5);znak1(0);znak1(5)
znak1(-5:5)

sourceCpp("C:/Users/student.WZIM.000/Desktop/znak2.cpp")
znak2(2)

suma_wbudowana <- sum
suma_R <- function( x ){
  wynik <- 0
  for( i in x ){
    wynik <- wynik + i 
  }
  return(wynik)
} 

cppFunction({' 
  double suma_C ( NumericVector x ){
    double wynik = 0;
    int n = x.size();
    for( int i = 0; i < n; i++ ){
      wynik += x[i];
    }
    return( wynik );
  }
'})

y <- 1:1000000

install.packages("microbenchmark")
library("microbenchmark")
microbenchmark( suma_wbudowana(y), suma_R(y), suma_C(y) )

# https://teuder.github.io/rcpp4everyone_en/index.html

### Przetwarzanie rownolegle
# install.packages("parallel")
library("parallel")
install.packages("doParallel")
library("doParallel")
install.packages("doSNOW")
library("doSNOW")

nCores <- detectCores()
klaster <- makeCluster( nCores )

lapply( 1:4, max )
parLapply( cl = klaster, X = 1:4, fun = max )

parLapply( cl = klaster, X = 1:4, 
           fun = function( x ){ repeat{} } )

stopCluster( klaster )
gc()

p <- 2
power <- function( x ){
  x ^ p
}

klaster <- makeCluster( 4 )
lapply( 1:4, power )
parLapply( cl = klaster, x = 1:4, fun = power )

clusterExport( cl = klaster, list = c("p") )
parLapply( cl = klaster, x = 1:4, fun = power )

for(i in 1:4) {
  print( i )
}

registerDoParallel( klaster )
foreach( i = 1:4 ) %dopar% {
  i
}

Foreach_lista <- foreach( i = 1:4 ) %dopar% {
  i^2
}
do.call( "c", Foreach_lista )
do.call( "rbind", Foreach_lista )

Foreach_lista <- foreach( i = 1:4, 
                          .combine = "c" ) %dopar% {
  i^2
}

foreach( i = 1:4 ) %dopar% {
  repeat{}
}