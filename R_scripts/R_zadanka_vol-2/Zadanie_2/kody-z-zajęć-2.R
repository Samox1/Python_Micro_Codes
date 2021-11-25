install.packages( "microbenchmark" )
install.packages( "parallel" )

library( microbenchmark )
library( parallel )

n_cores <- detectCores()
klaster <- makeCluster( n_cores )

microbenchmark(
  Seq = lapply( 1:1000, function(x){ c(x^2,x^3,x^4) } ),
  Par = parLapply( klaster, 1:1000, function(x){ c(x^2,x^3,x^4) } )
)

microbenchmark(
  Seq = lapply( 1:1000000, function(x){ c(x^2,x^3,x^4) } ),
  Par = parLapply( klaster, 1:1000000, function(x){ c(x^2,x^3,x^4) } ),
  times = 50
)
1000000 / 12

parLapply( klaster, 1:12, function(x){ repeat{} } )

stopCluster( klaster )

klaster <- makeCluster( n_cores )
p <- 3
parLapply( klaster, 1:12, function(x){ x ^ p } )

clusterExport( klaster, c("p") )
wynik <- parLapply( klaster, 1:12, function(x){ x ^ p } )

do.call( "rbind", wynik )
stopCluster( klaster )

install.packages( "doParallel" )
library( "doParallel" )

klaster <- makeCluster( n_cores )
registerDoParallel( klaster )

for( i in 1:12){ c(i^2,i^2) }

wynikForEach <- foreach( i = 1:12, .combine = "rbind" ) %dopar% {
  c(i^2,i^3)
}
wynikForEach

wynikForEach <- foreach( i = 1:12, .combine = "rbind" ) %dopar% {
  repeat{}
}

# Regresja

set.seed( 123 )
y_tar <- rnorm( 10 )
y_hat <- y_tar + 0.1

MAE <- function( y_t, y_h ){
  return( mean( abs( y_t - y_h ) ) )
}

MAE( y_tar, y_tar )
MAE( y_tar, y_hat )

# Klasyfikacja

y_tar <- c( 1,1,0,1,0,1,0,0,0,0 )# factor
y_hat <- (length(y_tar):1)/length(y_tar)

table( y_tar, y_tar )
Mat <- table( y_tar, 
              y_hat = ifelse( y_hat<=0.5, 0, 1 ) )#which.max

sum( diag(Mat) ) / sum( Mat )

install.packages("pROC")
library("pROC")

plot( roc( y_tar, y_hat ) )
auc( roc( y_tar, y_hat ) )

auc_Umww <- function( y_t, y_h ){
  y_tarLogic <- as.logical( y_t )
  n2 <- sum( y_tarLogic )
  n1 <- sum( !y_tarLogic )
  R2 <- rank( y_h )[ y_tarLogic ]
  licznik <- sum( R2 ) - ( n2 * (n2+1) ) / 2
  mianownik <- n1 * n2
  wynik <- licznik / mianownik
  return( wynik )
}

auc( roc( y_tar, y_hat ) )
auc_Umww( y_tar, y_hat )

# Kross-walidacja
kFold <- 4

set.seed(123)
(1-1/4);(1/4)
(1-1/10);(1/10)
(1-1/5);(1/5)

# (1-1/k)%-(1/k)%
indxT <- sample( x = 1:length(y_tar), replace = F,
                 size = (1-1/kFold) * length(y_tar) )
y_tar[ indxT ]
y_tar[ -indxT ]


library(caret)

# Tunowanie parametrow
parTune <- expand.grid( k = 1:5 )
parTune <- expand.grid( kFold = 1:kFold, depth = 3:5, 
                        nObs = c(5,10,15) )

wyniki <- data.frame( parTune, MAE = 0, MSE = 0, MAPE = 0 )
wyniki <- data.frame( parTune, 
                      MAEt = 0, MSEt = 0, MAPEt = 0,
                      MAEv = 0, MSEv = 0, MAPEv = 0)

