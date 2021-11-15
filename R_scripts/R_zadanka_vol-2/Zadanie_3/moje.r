 
# Zadanie 1:

set.seed(123)
X <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10)  )
X

y_tar <- c(1,0,1,1,0,1,1,0,0,0)

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew){
  if(is.na(X)==FALSE  && is.na(y_tar)==FALSE && k>0 && (class(X)== "data.frame" || class(X)=="matrix")){
    Xnew <- X
    for( i in 1:ncol(Xnew)){
      Xnew[,i] <- MinMax(X[,i], new_min = XminNew, new_max = XmaxNew)
    }
    
    minimalne <- c()
    for( i in 1:ncol(X)){
      minimalne[i] <- min(X[,i])
    }
    attr(Xnew, "minOrg") <- minimalne
    
    maksymalne <- c()
    for( i in 1:ncol(X)){
      maksymalne[i] <- max(X[,i])
    }
    
    attr(Xnew, "maxOrg") <- maksymalne
    minmaxnew <- c(XminNew, XmaxNew)
    attr(Xnew, "minmaxNew") <- minmaxnew
    
    wynik <- list()
    wynik$X <- Xnew
    wynik$y <- y_tar
    wynik$k <- k
    
  }else{
    stop("Bledne dane")
  } 
  
  return(wynik)
  
}

KNNtrain(X,y_tar, k=4, XminNew =1, XmaxNew =  3)


# Zadanie 2:

set.seed(123)
zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10)  )
zbiorD

KNNpred<-function(KNNmodel, X){
  if(is.na(X)==FALSE && all(colnames(X), colnames(KNNmodel$X))==TRUE){
    klasy<-sapply(X, class)
    if(unique(klasy)=="numeric"){
      d_euklides <- function( x_i, x_n ){
        return( sqrt( sum( ( x_i - x_n )^2 ) ) )
      }
      dEuklides  <- function( dane ) {
        n <- nrow( dane )
        odl <- matrix( 0, n, n )
        for( i in 1:n ){
          for( j in i:n ){
            odl[ j, i ] <- d_euklides( dane[i,], dane[j,] )
          }
        }
        return( odl )
      }
    }
    if(unique(klasy)=="factor"){
      hamming_binary <- function(X, Y = NULL) {
        if (is.null(Y)) {
          D <- t(1 - X) %*% X
          D + t(D)
        } else {
          t(1 - X) %*% Y + t(X) %*% (1 - Y)
        }
      }
      
    }
  }else{
    stop("Bledne dane") 
  }
}
  hamming_binary(c(1,0,0,1,1,1,0,1 ),c(1,0,1,1,1,0,0,1))
  dEuklides(zbiorD)
  