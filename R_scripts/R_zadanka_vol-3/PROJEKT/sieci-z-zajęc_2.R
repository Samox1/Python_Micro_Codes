sigmoid <- function( x ){
  return( 1 / (1 + exp(-x) ) )
}
d_sigmoid <- function( x ){
  return( x * ( 1 - x ) )
}
Relu<- function( x ){
  return( ifelse( x <= 0, 0, x ) )
}
d_Relu <- function( x ){
  return( ifelse( x <= 0, 0, 1 ) )
}
bladSS <- function( y_tar, y_hat ){
  return( 0.5 * sum( ( y_tar - y_hat )^2 ) )
}
bladEntropia <- function( y_tar, y_hat ){
  return( sum( ( y_tar - log( y_hat ) ) / nrow( y_tar ) ) )
}
SoftMax <- function( x ){
  return( exp( x ) / sum( exp( x ) ) )
}

wyjscie <- c(0.3,0.7,0.6)
psto <- wyjscie / sum( wyjscie )
psto
sum( psto)

Y_iris_mat <- model.matrix( ~ Species - 1, iris )
data.frame( iris$Species, Y_iris_mat )

wprzod <- function( X, W1, W2, W3 ){
  h1 <- sigmoid( X %*% W1 )
  h2 <- sigmoid( h1 %*% W2 )
  # y_hat <- sigmoid( h2 %*% W3 ) # klasyfikacja binarna
  # y_hat <- matrix( t( apply( h2 %*% W3, 1, SoftMax ) ), nrow = nrow(X) ) # klasyfikacja wieloklasowa
  y_hat <- h2 %*% W3 # regresja
  return( list( y_hat = y_hat, H1 = h1, H2 = h2 ) )
}

wstecz <- function( X, y_tar, y_hat, W1, W2, W3, H1, H2, lr ){
  # dy_hat <- (y_tar - y_hat) * d_sigmoid( y_hat ) # klasyfikacja binarna
  # dy_hat <- (y_tar - y_hat) / nrow( X ) # klasyfikacja wieloklasowa
  dy_hat <- (y_tar - y_hat) # regresja
  dW3 <- t(H2) %*% dy_hat
  dH2<- dy_hat %*% t(W3) * d_sigmoid( H2 )
  dW2 <- t(H1) %*% dH2
  dH1<- dH2 %*% t(W2) * d_sigmoid( H1 )
  dW1 <- t(X) %*% dH1
  W1 <- W1 + lr * dW1
  W2 <- W2 + lr * dW2
  W3 <- W3 + lr * dW3
  return( list( W1 = W1, W2 = W2, W3 = W3 ) )
}

trainNN <- function( X, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123 ){
  set.seed( seed )
  W1 <- matrix( runif( ncol(X) * h[1], -1, 1 ), nrow = ncol(X) )
  W2 <- matrix( runif( (h[1]) * h[2], -1, 1 ), nrow = h[1] )
  W3 <- matrix( runif( (h[2]) * ncol(y_tar), -1, 1 ), nrow = h[2] )
  error <- double( iter )
  for( i in 1:iter ){
    sygnalwprzod <- wprzod( X, W1, W2, W3 )
    sygnalwtyl <- wstecz( X, y_tar, y_hat = sygnalwprzod$y_hat, W1, W2, W3, H1 = sygnalwprzod$H1, H2 = sygnalwprzod$H2, lr )
    W1 <- sygnalwtyl$W1
    W2 <- sygnalwtyl$W2
    W3 <- sygnalwtyl$W3
    cat( paste0( "\rIteracja: ", i ) )
  }
  return( list( y_hat = sygnalwprzod$y_hat, W1 = W1, W2 = W2, W3 = W3 ) )
}

#### Binarna ####
Xiris <- iris[ iris$Species != "setosa",]
y_tar <- matrix( ifelse( Xiris$Species == "versicolor", 0, 1 ) )
Xiris <- Xiris[,-c(1:2,5)]  

MinMax <- function( x ){
  return( ( x - min(x) ) / ( max(x) - min(x) ) )
}  
MinMaxOdwrot <- function( x, y_min, y_max ){
  return(  x * (y_max - y_min) + y_min )
}  

Xiris <- sapply( Xiris, MinMax )
summary(Xiris) 

siec <- trainNN( Xiris, y_tar, h = c(5,5), lr = 0.01, iter = 5000, seed = 123 )
siec

table( y_tar, ifelse( siec$y_hat < 0.5, 0, 1 ) )

#### Wieloklasowa ####
X <- as.matrix( iris[,-5] )
y_tar <- model.matrix( ~ Species - 1, iris )

# zmienic wnętzre funkcji wprzod i w tyl
siec <- trainNN( X, y_tar, h = c(5,5), lr = 0.01, iter = 20000, seed = 123 )
siec

etykiety <- levels( iris[,5] )
head(siec$y_hat)
pred_iris <- as.factor( etykiety[apply( siec$y_hat, 1,  which.max )] )
table( iris[,5], pred_iris )

rowSums(siec$y_hat)

#### Regresja ####
X <- sapply( iris[,-c(4,5)], MinMax )
summary(X) 

y_tar <- sapply( iris[ , 4, drop = F ], MinMax )
y_min <- min( iris[, 4] )
y_max <- max( iris[, 4] )

siecReg <- trainNN( X, y_tar, h = c(5,5), lr = 0.01, iter = 500, seed = 123 )
siecReg

bladSS( y_tar, siecReg$y_hat )
bladSS( MinMaxOdwrot( y_tar, y_min, y_max ), MinMaxOdwrot( siecReg$y_hat, y_min, y_max ) )
