
suma_wbudowana <- sum
suma_R <- function( x ){
  wynik <- 0
  for( i in x ){
    wynik <- wynik + i 
  }
  return(wynik)
} 

y <- 1:1000000
suma_wbudowana(y); suma_R(y)

system.time( suma_wbudowana(y) )
system.time( suma_R(y) )

library(rbenchmark)
benchmark( suma_wbudowana(y) , suma_R(y) )

library(microbenchmark)
microbenchmark( suma_wbudowana(y) , suma_R(y) )

library(profvis)
library(ggplot2)

profvis({
  data(diamonds, package = "ggplot2")
  plot( price ~ carat, data = diamonds)
  regresja_lin <- lm( price ~ carat, data = diamonds )
  summary(regresja_lin)
  abline( regresja_lin, col = "red" )
})

