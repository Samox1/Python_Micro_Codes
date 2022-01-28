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




funkcja_sigmoid <- function(x){
  wynik <- 1 / (1 + exp(-x))
  return(wynik)
}

funkcja_dsigmoid <- function(x){
  wynik <- (1 - x) * x 
  return(wynik)
}

funkcja_min_max <- function(x){
  wynik <- (x-min(x)) / (max(x)-min(x))
  return(wynik)
}



propagacja_wprzod <- function( x_tar, W1, W2, W3){
  z1 <- cbind( matrix( 1, nrow = nrow(x_tar)), funkcja_sigmoid(x_tar %*% W1))
  z2 <- cbind( matrix( 1, nrow = nrow(x_tar)), funkcja_sigmoid(z1 %*% W2))
  return(list( y_hat = funkcja_sigmoid(z2 %*% W3), z1 = z1, z2 = z2))
}



propagacja_wstecz <- function( x_tar, y_tar, y_hat, z1, z2, W1, W2, W3, lr){
  
  dy_hat <- (y_tar - y_hat) * funkcja_dsigmoid(y_hat)
  
  dW3 <- t(z2) %*% dy_hat
  
  dz2<- dy_hat %*% t(W3) * funkcja_dsigmoid(z2)
  dW2 <- t(z1) %*% dz2[,-1]
  
  dz1<- dz2[,-1] %*% t(W2) * funkcja_dsigmoid(z1)
  dW1 <- t(x_tar) %*% dz1[,-1]
  
  W1 <- (lr * dW1) + W1
  W2 <- (lr * dW2) + W2
  W3 <- (lr * dW3) + W3

  return(list(W1 = W1, W2 = W2, W3 = W3))
}


trainNN <- function( Yname, Xnames, data, h, lr, iteracje_max, seed){
  set.seed(seed)
  
  h <- unlist(h, use.names = FALSE)
  
  y_tar <- data[,Yname]
  x_tar <- cbind(rep(1, nrow(data[,Xnames]) ), data[,Xnames])
  
  W1 <- matrix(runif(ncol(x_tar)*h[1], -1, 1), nrow = ncol(x_tar))
  W2 <- matrix(runif((h[1]+1)*h[2], -1, 1), nrow = 1+h[1])
  W3 <- matrix(runif((h[2]+1), -1, 1), nrow = 1+h[2])
  
  for( i in 1:iteracje_max )
  {
    prop_wp <- propagacja_wprzod(x_tar, W1, W2, W3)
    prop_ws <- propagacja_wstecz(x_tar, y_tar, y_hat = prop_wp$y_hat, z1 = prop_wp$z1, z2 = prop_wp$z2, W1, W2, W3, lr)
    
    W1 <- prop_ws$W1
    W2 <- prop_ws$W2
    W3 <- prop_ws$W3
    
    cat(paste( "\rIteracja uczenia Sieci Neuronowej: ", i , "/", iteracje_max))
  }
  
  return(list( y_hat = prop_wp$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}


predykcja_NN <- function(x_do_pred, NN_nauczona){
  
  x_do_pred <- cbind(rep(1, nrow(x_do_pred)), x_do_pred)
  
  z1 <- cbind(matrix(1, nrow = nrow(x_do_pred)), funkcja_sigmoid(x_do_pred %*% NN_nauczona$W1))
  z2 <- cbind(matrix(1, nrow = nrow(x_do_pred)), funkcja_sigmoid(z1 %*% NN_nauczona$W2))
  H3 <- funkcja_sigmoid(z2 %*% NN_nauczona$W3)
  
  return(H3)
}



iris_data <- iris[iris$Species != "setosa",]
iris_data$Species <- ifelse(iris_data$Species == "versicolor", 1, 0)
iris_data <- sapply(iris_data, funkcja_min_max)
summary(iris_data)

Siec_NN <- trainNN(Yname = "Species", Xnames = colnames(iris_data)[-5], data = iris_data, h = c(3,2), lr =  0.01, iteracje_max = 1000, seed = 357)

X_iris <- iris_data[,-5]
Y_iris_Predykcja <- predykcja_NN( X_iris[,-5], Siec_NN)

table(iris_data[,5], ifelse(Y_iris_Predykcja >= 0.5, 1, 0))

print("Siec NN: h = c(3,2), lr =  0.01, iteracje_max = 1000")
table(iris_data[,5], ifelse(Siec_NN$y_hat >= 0.5, 1, 0))


Siec_NN_2 <- trainNN(Yname = "Species", Xnames = colnames(iris_data)[-5], data = iris_data, h = c(4,4), lr =  0.001, iteracje_max = 2000, seed = 357)

print("Siec NN: h = c(4,4), lr =  0.001, iteracje_max = 2000")
table(iris_data[,5], ifelse(Siec_NN_2$y_hat >= 0.5, 1, 0))



