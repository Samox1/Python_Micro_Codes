# Plik proszę nazwać numerem swojego indeksu.
#
# Zadanie 1:
# a) Opracuj uogólnienie funkcji "trainNN" przyjmującej nastęujące parametry: "Yname", "Xnames", "data", "h", "lr", "iter", "seed".
#    Znaczenie parametrów: Yname - nazwa zmiennej celu z parametru data.
#                          Xnames - nazwy potencjalnych zmiennych objaśniających z parametru data.
#                          data - analizowany zbiór danych.
#                          h - wektor wskazujący liczbę warst ukrytych oraz liczbę neuronów ukrytych,
#                              np. c(3,2) definiuje dwie warstwy ukryte, odpowiednio z trzema oraz dwoma neuronami.
#                          lr - szybkość uczenia.
#                          iter - maksymalna liczba iteracji.
#                          seed - punkt początkowy dla PRNG.
# b) Zakładami, że wszystkie zmienne objaśniające są ciągłe oraz rozwiązujemy problem klasyfikacji binarnej.

library( ggplot2 )


sigmoid <- function( x ) {
  return( 1 / (1 + exp( -x ) ) )
}

dsigmoid <- function( x ) {
  return( x * (1 - x) )
}

ReLu <- function( x ) {
  return( ifelse( x <= 0, 0, x ) )
}

dReLu <- function( x ) {
  return( ifelse( x <= 0, 0, 1 ) )
}

lossSS <- function( y_tar, y_hat ) {
  return( 1/2 * sum( ( y_tar - y_hat )^2 ) )
}

SoftMax <- function(x) {
  exp( x ) / sum( exp( x ) )
}

MinMax <- function( x ) {
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}  

MinMaxOdwrot <- function( x, y_min, y_max ) {
  return(  x * (y_max - y_min) + y_min )
}  


wprzod <- function(X, W){
  #h1 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W1 )  )
  #h2 <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( h1 %*% W2 )  )
  
  # y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
  # if (type == "bin") {
  #   y_hat <- sigmoid( h2 %*% W3 )}
  # y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
  # else if (type == "multiclass") {
  # y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) )}
  # y_hat <- h2 %*% W3 # regresja
  # else {
  #   y_hat <- h2 %*% W3}
  # return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
  H <- list()
  H[[1]] <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( X %*% W[[1]] )  )
  if(length(W) > 2){
    for(i in 1:(length(W)-2)){
      H[[i+1]] <- cbind( matrix( 1, nrow = nrow(X) ), sigmoid( H[[i]] %*% W[[i+1]] )  )
    }
  }
  y_hat <- sigmoid( H[[length(W)-1]] %*% W[[length(W)]] )
  
  return(list(y_hat = y_hat, H = H))
  
}#wprzod



wstecz <- function(Y, X, Y_hat, W, H, lr){
  # dy_hat <- (y_tar - y_hat) * dsigmoid( y_hat ) # klasyfikacja binarna
  # dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
  # dy_hat <- (y_tar - y_hat) # regresja
  #Z ZAJEC
  # dW3 <- t(H2) %*% dy_hat
  # dH2<- dy_hat %*% t(W3) * dsigmoid( H2 )
  # dW2 <- t(H1) %*% dH2[,-1]
  # dH1<- dH2[,-1] %*% t(W2) * dsigmoid( H1 )
  # dW1 <- t(X) %*% dH1[,-1]
  # W1 <- W1 + lr * dW1
  # W2 <- W2 + lr * dW2
  # W3 <- W3 + lr * dW3
  
  dy_hat <- (as.numeric(Y) - Y_hat) * dsigmoid( Y_hat )
  #dy_hat <- (y_tar - Y_hat) * dsigmoid( Y_hat )
  dW = list()
  dH = list()
  
  dW[[length(W)]] <- t(H[[length(H)]]) %*% dy_hat
  dH[[length(H)]] <- dy_hat %*% t(W[[length(W)]]) * dsigmoid(H[[length(H)]])
  
  if(length(W)>2){
    for(i in (length(W)-1):2){
      dW[[i]]   <- t(H[[i-1]]) %*% dH[[i]][, -1]
      dH[[i-1]] <- dH[[i]][, -1] %*% t(W[[i]]) * dsigmoid (H[[i - 1]])
    }
  }
  dW[[1]] <- t(X) %*% dH[[1]][, -1]
  
  W_b = list()
  for(i in 1:length(W)){
    W_b[[i]] <- W[[i]] + lr * dW[[i]]
  }
  
  return(W = W_b)
  # return( list( W1 = W1, W2 = W2, W3 = W3 ) )
}#wstecz

trainNN<- function( Yname, Xnames, data, h = c(2,3), lr = 0.01, iter = 1000, seed = 123 ){
  
  # W1, .... W_liczba_warst_ukrytych+1
  # assign( paste0( "W", 1:W_liczba_warst_ukrytych+1), matrix( rnorm( h + 1 ) ) )
  # W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  # W2 <- matrix( runif( (h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1 )
  # W3 <- matrix( runif( (h[2]+1) * ncol(y_tar), -1, 1 ), nrow = h[2] + 1 )
  
  
  X <- as.matrix(data[, Xnames])
  Y <- as.numeric(as.character(data[, Yname]))
  
  
  set.seed( seed )
  X <- cbind( rep( 1, nrow(X) ), X ) #WSP
  
  W = list()
  W[[1]] <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  
  if(length(h)>1){
    for(i in 1:(length(h)-1)){
      # W_in lists
      W[[i+1]] <- matrix( runif( (h[i]+1) * h[i+1], -1, 1 ), nrow = h[i] + 1 )
    }
  }
  
  W[[length(h)+1]] <- matrix( runif( (h[length(h)]+1) * ncol(y_tar), -1, 1 ), nrow = h[length(h)] + 1 )
  
  error <- double( iter )
  
  for( i in 1:iter ){
    sygnalwprzod <- forward_prop( X, W )# W = list( W1, W2, W3 )
    sygnalwtyl   <- backword_prop( X, Y, Y_hat = sygnalwprzod$y_hat, W, H = sygnalwprzod$H, lr )
    #                           (X, Y, Y_hat, W, H, lr)
    W <- sygnalwtyl
    cat( paste0( "\rIteracja: ", i ) )
    error[i] <- lossSS( Y, sygnalwprzod$y_hat )
  }
  xwartosci <- seq( 1, iter, length = 1000 )
  print( qplot( xwartosci, error[xwartosci], geom = "line", main = "Error", xlab = "Iteracje" ) )
  # 
  return( list( y_hat = sygnalwprzod$y_hat, W = W ) )
  
} #trainNN

predNN <- function( xnew, NN ){
  xnew <- cbind( rep( 1, nrow(xnew) ), xnew )
  h1 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( xnew %*% NN$W1 )  )
  h2 <- cbind( matrix( 1, nrow = nrow(xnew) ), sigmoid( h1 %*% NN$W2 )  )
  y_hat <- sigmoid( h2 %*% NN$W3 )
  return( y_hat )
} #predNN

