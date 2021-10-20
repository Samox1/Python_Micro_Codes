# Przetwarzanie równoległe
library( microbenchmark )
library( parallel )
# library( snow )
# library( doSnow )
# library( doParallel )

n_cores <- detectCores()
klaster <- makeCluster( n_cores )

microbenchmark(
  Seq = lapply( 1:1000, function(x){ c(x^2,x^3,x^4) }),
  Par = parLapply( klaster, 1:1000, function(x){ c(x^2,x^3,x^4) } )  
)
microbenchmark(
  Seq = lapply( 1:1000000, function(x){ c(x^2,x^3,x^4) }),
  Par = parLapply( klaster, 1:1000000, function(x){ c(x^2,x^3,x^4) } )  
)
1000000 / 12

parLapply( klaster, 1:12, function(x){ repeat{} } ) 

stopCluster( klaster )

klaster <- makeCluster( n_cores )

wartosc <- 4
clusterExport( cl = klaster, varlist = c("wartosc") )
parLapply( klaster, 1:1000000, function(x){ x ^ wartosc } ) 
stopCluster( klaster )

library( doParallel )

klaster <- makeCluster( n_cores )
registerDoParallel( klaster )

do.call( "rbind", list(1, 2) )
foreach( i = 1:12, .combine = "rbind" ) %dopar% {
  c( i^2, i^3 )
}
foreach( i = 1:12, .combine = "rbind" ) %dopar% {
  repeat{}
}
stopCluster( klaster )

# Regresja
y_tar <- rnorm(10)
y_hat <- y_tar + 0.1

MAE <- function( y_tar, y_hat ){
  return( mean(abs(y_tar - y_hat)) )
}
MAE( y_tar, y_tar )
MAE( y_tar, y_hat )

# Klasyfikacja
y_tar <- c(1,1,0,1,0,1,0,0,0,0)
y_hat <- (length(y_tar):1)/length(y_tar)

table( y_tar, y_tar )
Mat <- table( y_tar, y_hat = ifelse( y_hat <= 0.5, 0, 1 ) )
sum( diag(Mat) ) / sum(Mat)

library(pROC)
auc_pakiet <- auc( roc( y_tar, y_hat ) )
plot( roc( y_tar, y_hat ) )

auc_Uwmw <- function( y_tar, y_hat ){
  
  y_tarlogic <- as.logical( y_tar )
  
  n2 <- sum( y_tarlogic )
  n1 <- sum( !y_tarlogic )
  
  R2 <- sum( rank( y_hat )[ y_tarlogic ] )
  
  licznik <- R2 - n2 * (n2 + 1) / 2
  mianownik <- n2 * n1
  
  return( licznik / mianownik )
  
}

auc_Uwmw( y_tar, y_hat )
auc_pakiet

# Sprawdzain krzyżowy
kFold <- 10
set.seed(123)

indxT <- sample( x = 1:length(y_tar), size = (1-1/kFold) * length(y_tar) , replace = F )
indV <- (1:length(y_tar))[-indxT]

# Tunowanie parametrów

parTune <- expand.grid( k = 1:5 )
parTune <- expand.grid( kFold = 1:3, k = 1:5 )

wynik <- data.frame( parTune, MAE = 0, MSE = 0, MAPE = 0 )
wynik <- data.frame( parTune, MAEt = 0, MSEt = 0, MAPEt = 0, MAEv = 0, MSEv = 0, MAPEv = 0 )
