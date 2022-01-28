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
# Pomoce:
# https://en.wikipedia.org/wiki/Backpropagation
# https://machinelearningmastery.com/the-chain-rule-of-calculus-for-univariate-and-multivariate-functions/
# https://towardsdatascience.com/understanding-backpropagation-algorithm-7bb3aa2f95fd
# https://ml-cheatsheet.readthedocs.io/en/latest/backpropagation.html



MinMax <- function( x ){
  return( (x-min(x)) / (max(x)-min(x)) )
}

sigmoid <- function( x ){
  return( 1 / (1 + exp(-x) ) )
}

dsigmoid <- function( x ){
  return( x * (1 - x) )
}


wprzod_NOWE_K <- function( X, W1, W2, W3){
  H1 <- cbind( matrix( 1, nrow = nrow(X)), sigmoid(X %*% W1))
  H2 <- cbind( matrix( 1, nrow = nrow(X)), sigmoid(H1 %*% W2))
  lista_wprzod <- list( y_hat = sigmoid(H2 %*% W3), H1 = H1, H2 = H2)
  return(lista_wprzod)
}



wstecz_NOWE_K <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr){
  dy_hat <- (y_tar - y_hat) * dsigmoid(y_hat)
  dW3 <- t(H2) %*% dy_hat
  dH2<- dy_hat %*% t(W3) * dsigmoid(H2)
  dW2 <- t(H1) %*% dH2[,-1]
  dH1<- dH2[,-1] %*% t(W2) * dsigmoid(H1)
  dW1 <- t(X) %*% dH1[,-1]
  W1 <- W1 + lr * dW1
  W2 <- W2 + lr * dW2
  W3 <- W3 + lr * dW3
  lista_wag <- list(W1 = W1, W2 = W2, W3 = W3)
  return(lista_wag)
}


trainNN_NOWE_K <- function( Yname, Xnames, data, h, lr, Maxiter, seed){
  set.seed(seed)
  
  y_tar = data[,Yname]
  X <- cbind( rep( 1, nrow(data[,Xnames]) ), data[,Xnames] )
  h = unlist(h, use.names = FALSE)
  W1 <- matrix(runif(ncol(X) * h[1], -1, 1 ), nrow = ncol(X))
  W2 <- matrix(runif((h[1]+1) * h[2], -1, 1 ), nrow = h[1] + 1)
  W3 <- matrix(runif((h[2]+1) * 1, -1, 1 ), nrow = h[2] + 1)
  
  for( i in 1:Maxiter ){
    Sygnal_w_przod <- wprzod_NOWE_K(X, W1, W2, W3)
    Sygnal_w_tyl <- wstecz_NOWE_K(X, y_tar, y_hat = Sygnal_w_przod$y_hat, W1, W2, W3, H1 = Sygnal_w_przod$H1, H2 = Sygnal_w_przod$H2, lr)
    W1 <- Sygnal_w_tyl$W1
    W2 <- Sygnal_w_tyl$W2
    W3 <- Sygnal_w_tyl$W3
    cat( paste0( "\rIteracje wykonane: ", i , " / ", Maxiter) )
  }
  
  return( list( y_hat = Sygnal_w_przod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}


predNN_NOWE_K <- function( Xnew, Siec_NN){
  Xnew <- cbind(rep(1, nrow(Xnew)), Xnew)
  H1 <- cbind(matrix(1, nrow = nrow(Xnew)), sigmoid(Xnew %*% Siec_NN$W1))
  H2 <- cbind(matrix(1, nrow = nrow(Xnew)), sigmoid(H1 %*% Siec_NN$W2))
  return(sigmoid(H2 %*% Siec_NN$W3))
}



iris_minmax <- iris[ iris$Species !="setosa", ]
iris_minmax$Species <- ifelse( iris_minmax$Species == "versicolor", 1, 0 )
iris_minmax <- sapply( iris_minmax, MinMax )
summary(iris_minmax)

Xnames <- colnames(iris_minmax)[-5]
Yname <- colnames(iris_minmax)[5]

Siec <- trainNN_NOWE_K(Yname = Yname, Xnames = Xnames, data = iris_minmax, h = c(3,7), lr =  0.01, Maxiter = 1000, seed = 525)    # Wieksze -> Maxiter = lepsza predykcja

x_pred <- iris_minmax[,]
y_pred <- predNN_NOWE_K( x_pred[,Xnames], Siec)

table(x_pred[,Yname], ifelse(y_pred >= 0.5, 1, 0))

