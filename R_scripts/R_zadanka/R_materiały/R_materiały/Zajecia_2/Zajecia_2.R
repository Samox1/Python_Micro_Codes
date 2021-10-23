# Przetwarzanie równoległe
library(microbenchmark)
install.packages("parallel")
library("parallel")

power <- function( x ){
  c( x^2, x^3 )
}

nCores <- detectCores()
klaster <- makeCluster( nCores )

microbenchmark(
  Seq = lapply( 1:1000, power ),
  Par = parLapply( klaster, 1:1000, power )
)
microbenchmark(
  Seq = lapply( 1:1000000, power ),
  Par = parLapply( klaster, 1:1000000, power )
)

parLapply( klaster, 1:12, function( x ){ repeat{} } )

stopCluster( klaster )

klaster <- makeCluster( nCores )

p <- 3
power <- function( x ){
  c( x ^ p )
}
power(1:5)

parLapply( klaster, 1:12, power )

clusterExport( klaster, c("p") )
parLapply( klaster, 1:12, power )
stopCluster( klaster )

install.packages("doParallel")
library("doParallel")
install.packages("doSnow")
library("doSnow")

klaster <- makeCluster( nCores )

for( i in 1:12 ){ print(i) }

registerDoParallel( klaster )
foreach( i = 1:12 ) %dopar% { i }
wynik <- foreach( i = 1:12 ) %dopar% { i }

do.call( "c", as.list(1:5) )
do.call( "rbind", as.list(1:5) )
wynik <- foreach( i = 1:12, .combine = "rbind" ) %dopar% { i }

# Large memory

data(trees)
model <- log(Volume) ~ log(Height) + log(Girth)

install.packages(biglm)
library(biglm)

dane1 <- trees[1:10,]
dane2 <- trees[11:20,]
dane3 <- trees[21:31,]
#for( i in 1:3 ){ read.table() }

model_biglm <- biglm( model, dane1 )
summary(model_biglm)

model_biglm <- update( model_biglm, dane2 )
model_biglm <- update( model_biglm, dane3 )
summary(model_biglm)

model_lm <- lm( model, trees )
rbind( coef(model_biglm), coef(model_lm) )

install.packages(data.table)
library(data.table)

n <- 1000000
k <- c(200,500)
p <- 3 

T1 <- sapply( k, function(x){ as.character(sample(1:n, n, replace = T)) })
str(T1)
T2 <- sapply(1:p, function(x){ rnorm(n) } )
str(T2)
TT <- data.frame( T1, T2 )
str(TT)

TT[ TT$X1 == "2",]

TT_dt <- data.table( T1, T2 )
str(TT_dt)

microbenchmark( filter( TT_dt, TT_dt$V1 == "2" ), TT[ TT$X1 == "2",] )
microbenchmark( TT[ TT$X1 == "2",], TT_dt[ V1 == "2"] )

install.packages("bigmemory")
library(bigmemory)
library(pryr)

big_tabela <- matrix( runif( 10^4 * 10^4 ), 10^4, 10^4 )
object_size( big_tabela )

small_tabela <- as.big.matrix( big_tabela )
object_size( small_tabela )

str( small_tabela )
small_tabela
small_tabela[1:10, 1:10]

Big = big_tabela[ big_tabela[,1] > 0.5, ]


microbenchmark( Big = big_tabela[ big_tabela[,1] > 0.5, ],
                Small = small_tabela[ small_tabela[,1] > 0.5, ] )

