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



# unregister_dopar <- function() {
#   env <- foreach:::.foreachGlobals
#   rm(list=ls(name=env), pos=env)
# }
# unregister_dopar()


funkcja_sigmoid <- function(x){
  wynik <- 1 / (1 + exp(-x))
  return(wynik)
}

funkcja_dsigmoid <- function(x){
  wynik <- (1 - x) * x 
  return(wynik)
}

funkcja_ReLu <- function( x ){
  return( ifelse( x <= 0, 0, x ) )
}

funkcja_dReLu <- function( x ){
  return( ifelse( x <= 0, 0, 1 ) )
}

funkcja_min_max <- function(x){
  wynik <- (x-min(x)) / (max(x)-min(x))
  return(wynik)
}



propagacja_wprzod_test <- function(x, W_in, wagi, W_out){
  
  z_in <- cbind( matrix( 1, nrow = nrow(x)), funkcja_sigmoid(x %*% W_in))
  
  if(length(wagi[[1]]) > 1)
  {
    z_wagi <- list()
    z_wagi[[1]] <- cbind( matrix( 1, nrow = nrow(x)), funkcja_sigmoid(z_in %*% (wagi[[1]])))

    if(length(wagi) > 1)
    {
      for (i in 2:length(wagi))
      {
        z_wagi[[i]] <- cbind( matrix( 1, nrow = nrow(x)), funkcja_sigmoid(z_wagi[[i-1]] %*% wagi[[i]]))
      }
    }
    y_hat = funkcja_sigmoid(z_wagi[[length(z_wagi)]] %*% W_out)
  }
  else
  {
    z_wagi <- list(0)
    y_hat = funkcja_sigmoid(z_in %*% W_out)
  }
  return(list(z_in = z_in, z_wagi = z_wagi, y_hat = y_hat))
}


propagacja_wstecz_test <- function(x, y_tar, y_hat, z_in, z_wagi, W_in, wagi, W_out, lr){
  
  dy_hat <- (y_tar - y_hat) * funkcja_dsigmoid(y_hat)
  
  if(length(wagi[[1]]) > 1)
  {
    dW_out <- t(z_wagi[[length(z_wagi)]]) %*% dy_hat
    
    dz <- list()
    dw_wagi <- list()  
    
    if(length(z_wagi) == 1)
    {
      dz[[(length(z_wagi)+1)]] <- dy_hat %*% t(W_out) * funkcja_dsigmoid(z_wagi[[length(z_wagi)]])
      dw_wagi[[(length(z_wagi)+1)]] <- t(z_in) %*% dz[[(length(z_wagi)+1)]][,-1]
    }
    else
    {
      dz[[(length(z_wagi)+1)]] <- dy_hat %*% t(W_out) * funkcja_dsigmoid(z_wagi[[length(z_wagi)]])
      dw_wagi[[(length(z_wagi)+1)]] <- t(z_wagi[[length(z_wagi)-1]]) %*% dz[[(length(z_wagi)+1)]][,-1]
    }
    
    if(length(z_wagi) > 1)
    {
      if(length(z_wagi) > 2)
      {
        for (i in (length(z_wagi)):3)
        {
          dz[[i]] <- dz[[i+1]][,-1] %*% t(wagi[[i]]) * funkcja_dsigmoid(z_wagi[[i-1]])
          dw_wagi[[i]] <- t(z_wagi[[i-2]]) %*% dz[[i]][,-1]
        }
      }
      dz[[2]] <- dz[[3]][,-1] %*% t(wagi[[2]]) * funkcja_dsigmoid(z_wagi[[1]])
      dw_wagi[[2]] <- t(z_in) %*% dz[[2]][,-1]
    }
    dz[[1]] <- dz[[2]][,-1] %*% t(wagi[[1]]) * funkcja_dsigmoid(z_in)
    dw_wagi[[1]] <- t(x) %*% dz[[1]][,-1]
    
    W_in <- (lr * dw_wagi[[1]]) + W_in
    for (w in 1:length(wagi)) {
      wagi[[w]] <- (lr * dw_wagi[[w+1]]) + wagi[[w]]
    }
    W_out <- (lr * dW_out) + W_out
  }
  else
  {
    dW_out <- t(z_in) %*% dy_hat
    
    dz_in <-  dy_hat %*% t(W_out) * funkcja_dsigmoid(z_in)
    dW_in <- t(x) %*% dz_in[,-1]

    W_in <- (lr * dW_in) + W_in
    W_out <- (lr * dW_out) + W_out
  }
  return(list(W_in = W_in, wagi = wagi, W_out = W_out))
}



trainNN_test <- function( Yname, Xnames, data, h, lr, iteracje_max, seed){
  set.seed(seed)
  h <- unlist(h, use.names = FALSE)
  
  y_tar <- data[,Yname]
  x <- cbind(rep(1, nrow(data[,Xnames]) ), data[,Xnames])
  
  W_in <- matrix(runif(ncol(x) * h[1], -1, 1), nrow = ncol(x))
  W_out <- matrix(runif((h[length(h)] + 1), -1, 1), nrow = h[length(h)] + 1 )
  wagi <- list()
  
  if(length(h) > 1)
  {
    for (i in 1:(length(h)-1)) 
    {
      wagi[[i]] <- matrix(runif((h[i]+1)*h[i+1], -1, 1), nrow = 1+h[i])
    }
  }
  else
  {
    wagi[[1]] <- 0
  }

  for(i in 1:iteracje_max)
  {
    prop_wp <- propagacja_wprzod_test(x, W_in, wagi, W_out)
    prop_ws <- propagacja_wstecz_test(x, y_tar, y_hat = prop_wp$y_hat, z_in = prop_wp$z_in, z_wagi = prop_wp$z_wagi, W_in, wagi, W_out, lr)

    W_in <- prop_ws$W_in
    wagi <- prop_ws$wagi
    W_out <- prop_ws$W_out

    cat(paste( "\rIteracja uczenia Sieci Neuronowej: ", i , "/", iteracje_max))
  }
  wynik <- list( y_hat = prop_wp$y_hat, W_in = W_in, wagi = wagi, W_out = W_out )
  return(wynik)
}


predykcja_NN_test <- function(x_do_pred, NN_nauczona){
  
  x_do_pred <- cbind(rep(1, nrow(x_do_pred)), x_do_pred)
  
  z1 <- cbind(matrix(1, nrow = nrow(x_do_pred)), funkcja_sigmoid(x_do_pred %*% NN_nauczona$W_in))
  
  if(length(NN_nauczona$wagi[[1]]) > 1)
  {
    z2 <- list()
    z2[[1]] <- cbind(matrix(1, nrow = nrow(x_do_pred)), funkcja_sigmoid(z1 %*% NN_nauczona$wagi[[1]]))
    
    if(length(NN_nauczona$wagi) > 1)
    {
      for (i in 2:length(NN_nauczona$wagi))
      {
        z2[[i]] <- cbind(matrix(1, nrow = nrow(x_do_pred)), funkcja_sigmoid(z2[[i-1]] %*% NN_nauczona$wagi[[i]]))
      }
    }
    y_hat <- funkcja_sigmoid(z2[[length(NN_nauczona$wagi)]] %*% NN_nauczona$W_out)
  }
  else
  {
    y_hat <- funkcja_sigmoid(z1 %*% NN_nauczona$W_out)
  }

  return(y_hat)
}


iris_data <- iris[iris$Species != "setosa",]
iris_data$Species <- ifelse(iris_data$Species == "versicolor", 1, 0)
iris_data <- sapply(iris_data, funkcja_min_max)
summary(iris_data)


TEST_SIEC_NN <- trainNN_test(Yname = "Species", Xnames = colnames(iris_data)[-5], data = iris_data, h = c(2,5,8,5), lr =  0.01, iteracje_max = 1000, seed = 357)
TEST_SIEC_NN
predykcja_NN_test(iris_data[,-5], TEST_SIEC_NN)

table(iris_data[,5], ifelse(TEST_SIEC_NN$y_hat >= 0.5, 1, 0))
table(iris_data[,5], ifelse(predykcja_NN_test(iris_data[,-5], TEST_SIEC_NN) >= 0.5, 1, 0))

