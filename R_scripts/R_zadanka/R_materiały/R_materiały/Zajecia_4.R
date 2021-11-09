# Normalizacja 

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}

set.seed(123)
y <- runif( 100 )

summary( MinMax( y ) )
summary( MinMax( y, 10, 25 ) )

# Miary odległości

d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

set.seed(123)
zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10) )
zbiorD

d_euklides( zbiorD[1,], zbiorD[2,] )

dEuklides <- function( dane ){
  n <- nrow( dane )
  odl <- matrix( 0, n, n )
  for( i in 1:n ){
    for( j in i:n ){
      odl[ j, i ] <- d_euklides( dane[i,], dane[j,] )
    }
  }
  return( odl )
}
dEuklides( zbiorD )
dist( zbiorD )

# K-NN

zbiorD <- data.frame( zbiorD, y = ifelse( zbiorD$x1 == 1, zbiorD$x3 + 1, zbiorD$x3 + 10 ) )
zbiorD

KNNtrain <- function( X, y_tar, k = 5 ){
  knn <- list()
  knn[["X"]] <- X
  knn[["y"]] <- y_tar
  knn[["k"]] <- k
  return( knn )
}

KNNreg <- KNNtrain( zbiorD[,-4], zbiorD[,4], k = 5 )

KNNpred <- function( model, X ){
  nTrain <- nrow( model$X )
  nPred <- nrow( X )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[i,j] <- d_euklides( model$X[i,], X[j,] )
    }
  }
  pred <- double( nPred )
  for( i in 1:nPred ){
    kNaj <- order( odl[,i] )
    kNaj <- kNaj[1:model$k]
    y_hat <- mean( model$y[ kNaj ] )
    pred[ i ] <- y_hat
  }
  return( pred )
}

KNNpred( KNNreg, zbiorD[,-4] )

library(caret)
KNNreg_pakiet <- knnreg( zbiorD[,-4], zbiorD[,4], k = 5 )
str( KNNreg_pakiet )

predict( KNNreg_pakiet, zbiorD[,-4] )
KNNpred( KNNreg, zbiorD[,-4] )

