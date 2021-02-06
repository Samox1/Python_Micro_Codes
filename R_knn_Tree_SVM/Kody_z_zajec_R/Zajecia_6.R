X <- data.frame( x1 = c(0.4,.55,.65,.9,1,.35,.5,.15,0.2,.85),
                 x2 = c(0.85,.95,.8,.87,.5,.55,.5,.2,.1,.3) )
y_tar <- c(1,1,1,1,1,0,0,1,0,0)

plot( X$x1, X$x2, col = ifelse( y_tar == 0, "red", "blue" ) )
plot(Regresja$Wytrzymalosc, Regresja$Wiek, col = ifelse( y_tar == 0, "red", "blue" ))

# Zdefiniujmy λ i jako decyzję (θ T x i + θ 0 ), wtedy:
Decyzja <- function( X, theta, theta0 ){
  X %*% t( theta ) + theta0
}

# Jeśli (y i λ i ) > 1, to klasyfikator prawidłowo przewiduje znak
Margines <- function( X, y, theta, theta0 ){
  y * Decyzja( X, theta, theta0 )
}

# L = n X kθ 2 k + C max(0, 1 − y i (θ T x i + θ 0 ))
Koszt <- function( margines, theta, C ){
  ( 1/ 2 ) * theta %*% t(theta) + C * sum( pmax( 0, 1 - margines ) ) # apply( tabela, 1, max )
}

trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 ){
  n <- nrow( X )
  p <- ncol( X )
  theta <- matrix( runif( p ), nrow = 1 ) 
  theta0 <- 0
  koszt <- double( maxiter )
  for( i in 1:maxiter ){
    margines <- Margines( X, y, theta, theta0 )
    koszt[i] <- Koszt( margines, theta, C )
    indxMiss <- which( margines < 1 )
    d_theta <- theta - C * y[ indxMiss ] %*% X[ indxMiss, ]
    d_theta0 <- - C * sum( y[ indxMiss ] )
    theta <- theta - lr * d_theta
    theta0 <- theta0 - lr * d_theta0
  }
  SuppVector_Niepoprawnie <- which( Margines( X, y, theta, theta0 ) <= 1 )
  return( list( Theta = theta, Theta0 = theta0, SupVecNiepop = SuppVector_Niepoprawnie ) )
}

predSVM <- function( X, theta, theta0 ){
  sign( Decyzja( X, theta, theta0 ) )
}

y_tar2 <- ifelse( y_tar == 0, -1, y_tar )
model1 <- trainSVM( as.matrix( X ), y_tar2, C = 100, lr = 0.001, maxiter = 500 )

plot( X$x1, X$x2, col = ifelse( y_tar == 0, "red", "blue" ) )
x1x2 <- seq( 0, 1, by = 0.1 )
decyzja <- outer( x1x2, x1x2, function( x1, x2 ){ Decyzja( cbind(x1,x2), model1$Theta, model1$Theta0 ) } )
contour( x1x2, x1x2, decyzja, add = T, lwd = 2, levels = c(-1,0,1), col = c("red","blue","red"), lty = c(2,1,2) )
points( X$x1[model1$SupVecNiepop], X$x2[model1$SupVecNiepop], lwd = 3, 
        col = ifelse( y_tar2[model1$SupVecNiepop] == -1, "red", "blue" ) )

library( e1071 )
model2 <- svm( y = factor(y_tar2), x = X, kernel = "linear" )

table( tar = y_tar2, pred = predSVM( as.matrix( X ), model1$Theta, model1$Theta0 ) )
table( tar = y_tar2, pred = predict( model2, X ) )

iris_ <- iris[ iris$Species != "setosa", ]
X <- iris_[,3:4]
y_tar2 <- ifelse( iris_$Species == "versicolor", -1, 1 )

model3 <- trainSVM( as.matrix( X ), y_tar2, C = 20, lr = 0.001, maxiter = 5000 )
table( tar = y_tar2, pred = predSVM( as.matrix( X ), model3$Theta, model3$Theta0 ) )

plot( X$Petal.Length, X$Petal.Width, col = ifelse( y_tar2 == -1, "red", "blue" ) )
x1x2 <- seq( 1, 7, by = 0.1 )
decyzja <- outer( x1x2, x1x2, function( x1, x2 ){ Decyzja( cbind(x1,x2), model3$Theta, model3$Theta0 ) } )
contour( x1x2, x1x2, decyzja, add = T, lwd = 2, levels = c(-1,0,1), col = c("red","blue","red"), lty = c(2,1,2) )
points( X$Petal.Length[model3$SupVecNiepop], X$Petal.Width[model3$SupVecNiepop], lwd = 3, 
        col = ifelse( y_tar2[model3$SupVecNiepop] == -1, "red", "blue" ) )