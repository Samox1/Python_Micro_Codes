# Pomiar czasu wykonania kodu

suma_wbudowana <- sum

suma_R <- function( x ){
  wynik <- 0
  for( i in x ){
    wynik <- wynik + i
  }
  return(wynik)
}
suma_wbudowana(1:10); suma_R(1:10)

y <- 1:1000000
system.time( suma_wbudowana(y) )
system.time( suma_R(y) )

install.packages(rbenchmark)
library(rbenchmark)

benchmark( suma_wbudowana(y), suma_R(y) )

install.packages(microbenchmark)
library(microbenchmark)

microbenchmark( suma_wbudowana(y), suma_R(y) )

# Profilowanie kodu
install.packages(profvis)
library(profvis)
install.packages(ggplot2)
library(ggplot2)

profvis({
  data( diamonds, package = "ggplot2" )
  plot( price ~ carat, data = diamonds )
  reg_lin <- lm( price ~ carat, data = diamonds )
  summary( reg_lin )
  abline( reg_lin, col = "red" )  
})

# Integracja R i C++

install.packages(Rcpp)
library(Rcpp)

cppFunction({'
  String znak1( double x ){
    if( x < 0 ){ return("Liczba jest ujemna"); }
    else if( x > 0 ){ return("Liczba jest dodatnia"); }
    else { return("Liczba jest równa 0"); }
  }
'})
znak1

znak1(-1); znak1(0); znak1(1);
znak1(-1:1);
sapply(-1:1, znak1)

sourceCpp("znak2.cpp")
sapply(-1:1, znak2)

cppFunction({'
  double suma_C( NumericVector x ){
    double wynik = 0.0;
    int n = x.size();
    for( int i = 0; i < n; ++i ){
      wynik += x[i];
    }
    return( wynik );
  }
'})

microbenchmark( suma_wbudowana(y), suma_C(y), suma_R(y) )


cppFunction({'
  List lapply_C( List x, Function f ){
    int n = x.size();
    List wynik(n);
    for( int i = 0; i < n; ++i ){
      wynik[i] = f( x[i] );
    }
    return( wynik );
  }
'})
lista1 <- list( 1:10, 20:30 )
lapply( lista1, sum )
lapply_C( lista1, sum )


