# Regresja

set.seed( 123 )
y_tar <- rnorm( 10 )
y_hat <- y_tar + 0.1

MAE <- function( y_t, y_h ){
  return( sum( abs( y_t - y_h ) ) )
}

MAE( y_tar, y_tar ) 
MAE( y_tar, y_hat )

# Klasyfikacja
y_tar <- c(1,1,0,1,0,1,0,0,0,0) # factor
y_hat <- length(y_tar):1 / length(y_tar)

table( y_tar, y_tar )
Mat <- table( y_tar, 
              y_hat = ifelse( y_hat <= 
                                0.5, 0, 1 ) )

Jakosc <- sum( diag( Mat ) ) / sum( Mat )

install.packages( "pROC" )
library( "pROC" )

# R2 − n2(n2+1)/2
# n2n1

roc( y_tar, y_hat )
plot( roc( y_tar, y_hat ) )
auc( roc( y_tar, y_hat ) ) 

auc_Umww <- function( y_t, y_h ){
  y_tarLogic <- as.logical( y_t )
  n2 <- sum( y_tarLogic )
  n1 <- sum( !y_tarLogic )
  R2 <- sum( rank( y_h )[ y_tarLogic ] )
  licznik <- R2 - ( n2 * (n2+1) / 2 )
  mianownik <- n2 * n1
  return( licznik / mianownik )
}
auc_Umww( y_tar, y_hat )

# Kroswalidacja
kFold <- 4
set.seed( 123 )

# (1-1/k)%-(1/k)%
indxT <- sample( x = 1:length(y_tar), 
                 size = (1-1/kFold)*length(y_tar),
                 replace = F )
y_tar[ indxT ] #trening
y_tar[ -indxT ] # walidacja

# Tunowanie parametrow
parTune <- expand.grid( k = 1:5 )
parTune

parTune <- expand.grid( depth = 3:5, 
                        Nobs = c( 5, 10, 15 ) )
parTune

parTune <- expand.grid( kFold = 1:kFold,
                        depth = 3:5, 
                        Nobs = c( 5, 10, 15 ) )
View( parTune )

parTune <- data.frame( parTune, 
                       MSEt = 0, MAEt = 0,
                       MSEv = 0, MAEv = 0)
parTune