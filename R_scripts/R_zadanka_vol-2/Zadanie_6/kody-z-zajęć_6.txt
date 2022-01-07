sigmoid <- function( x ){
  return( 1 / (1 + exp(-x) ) )
}

wprzod <- function( X, Win, Wout ){
  z1 <- cbind( 1, X )  %*% Win
  H <- sigmoid( z1 )
  z2 <- cbind( 1, H )  %*% Wout
  y_hat <- sigmoid( z2 )
  return( list( y_hat = y_hat, H = H )  )
}

wstecz <- function( X, y_tar, y_hat, Win, Wout, H, lr ){
  dWout <- t( cbind( 1, H ) ) %*% ( y_hat - y_tar )
  dH <- ( y_hat - y_tar ) %*% t( Wout[-1,,drop=F] )
  dWin <- t( cbind( 1, X ) ) %*% ( H * (1-H) * dH )
  Win <- Win - lr * dWin
  Wout <- Wout - lr * dWout
  return( list( Win = Win, Wout = Wout ) )
}
c(5,2)
trainNN <- function( X, y_tar, h = 5, lr =  0.01, Maxiter = 10000 ){
  p <- ncol( X )
  Win <- matrix( rnorm( (p + 1) * h  ), p + 1, h )
  Wout <- matrix( rnorm( h + 1) )
  for( i in 1:Maxiter ){
    SygnalWprzod <- wprzod( X = X, Win = Win, Wout = Wout )
    SygnalWstecz <- wstecz( X = X, y_tar = y_tar, y_hat = SygnalWprzod$y_hat, 
                            Win = Win, Wout = Wout, SygnalWprzod$H, lr = lr )
    Win <- SygnalWstecz$Win
    Wout <- SygnalWstecz$Wout
    print( i )
  }
  return( list( y_hat = SygnalWprzod$y_hat, Win = Win, Wout = Wout ) )
}

predNN <- function( Xnew, NN ){
  z1 <- cbind( 1, Xnew )  %*% NN$Win
  H <- sigmoid( z1 )
  z2 <- cbind( 1, H )  %*% NN$Wout
  y_hat <- sigmoid( z2 )
  return( y_hat  )
}

Xiris <- iris[ iris$Species !="setosa", ]
Y_tar <- ifelse( Xiris$Species == "versicolor", 1, 0 )
Xiris <- Xiris[,-5]

MinMax <- function( x ){
  return( (x-min(x)) / (max(x)-min(x)) )
}

Xiris <- sapply( Xiris, MinMax )
summary(Xiris)

Siec <- trainNN( Xiris, Y_tar, h = 5, lr =  0.01, Maxiter = 10000 )
Siec

table( Y_tar, ifelse( Siec$y_hat >= 0.5, 1, 0 ) )
table( Y_tar, ifelse( predNN( Xiris, Siec ) >= 0.5, 1, 0 ) )

wprzod <- function( X, list( 1= Win, 2 = Wout ) ){
  n <- length( list() )
  for( i 1:n ){
    H1 <- sigmoid( cbind( 1, X ) %*% list[[1]] )
    H2 <- sigmoid( cbind( 1, H1 ) %*% list[[2]] )
  }
  x1 <- x2 <- Xiris[1:5,]
  w1 <- w2 <- matrix( runif(16), 4, 4  )
  x1 %*% w1
  eval( parse( text = paste0( "x", 1, "%*%", "w", 1 ) ) )
  sprintf("x_%d",1)
  L <- list(w1,w2)
  eval( parse( 
    text = sprintf("H1 <-sigmoid( x%d %%*%% L[[%d]])", 1, 1 ) ) )
  # z1 <- cbind( 1, X )  %*% Win
  # H <- sigmoid( z1 )
  # z2 <- cbind( 1, H )  %*% Wout
  # y_hat <- sigmoid( z2 )
  return( list( y_hat = y_hat, H = H )  )
}