
# Regresja

CrossValidTune(dane_2, 4, 4, 123)

X = dane_2[1:50,-9]
y_tar = dane_2[1:20,9]
X[,order(colnames(X))]
zbior(X)

KNNmodel_1 = KNNtrain( X[,order(colnames(X))], y_tar, 4, 0, 1)
KNNpred(KNNmodel_1, X)

y_hat <- KNNpred(KNNmodel_1, X)

ModelOcena <- function(y_tar, y_hat)

X <- dane_2[1:100,]
X <- sapply(X[,-c(3,11)], MinMax)
y_min <- min(X[,11])
y_tar <- sapply( X[ , 11, drop = F ], MinMax )
siec_regresja <- trainNN( X, y_tar, h = c(5,5), lr = 0.01, iter = 100000, seed = 123 )
summary(X) 

# Klasyfikacja binarna

CrossValidTune(dane_3, 4, 4, 123)

zbior(dane_3)
X = dane_3[1:50,-6]
y_tar = dane_3[1:10,6]
X[,order(colnames(X))]
KNNmodel_2 = KNNtrain( X[,order(colnames(X))], y_tar, 4, 0, 1)
KNNpred(KNNmodel_2, X)

y_hat <- KNNpred(KNNmodel_2, X)

ModelOcena <- function(y_tar, y_hat)
  
zbior(dane_3)
X <- dane_3[,c(2:3)]
y_tar <- matrix(dane_3$Ceasarian)
X <- sapply( X, MinMax )
summary(X)
siec_binarne <- trainNN( X, y_tar, h = c(5,5), lr = 0.01, iter = 5000, seed = 123 )
table( y_tar, ifelse( predNN( X, siec ) < 0.5, 0, 1 ) )

X <- dane_3[,1:2]
y_tar <- ifelse( dane_3$Ceasarian == 0, -1, 1)

model <- trainSVM( as.matrix(X), y_tar, C = 15, lr = 0.001, maxiter = 5000 )
table( tar = y_tar, pred = predSVM( as.matrix(X), model3$Theta, model3$Theta0 ) )

plot( X$Age, X$Delivery.number, col = ifelse( y_tar == -1, "green", "blue" ) )
x1x2 <- seq( 1, 7, by = 0.5 )

decision <- outer( x1x2, x1x2, function( x1, x2 ){ Decision( cbind(x1,x2), model3$Theta, model3$Theta0 ) } )
contour( x1x2, x1x2, decision, add = T, lwd = 2, levels = c(-1,0,1), col = c("red","blue","red"), lty = c(2,1,2) )
points( X$Petal.Length[model3$SP], X$Petal.Width[model3$SP], lwd = 3, 
        col = ifelse( y_tar[model3$SP] == -1, "red", "blue") )

zb_1 = as.data.frame(dane[dane['Rings'] == 10 | dane['Rings'] == 5])
zb_1 <- dane[ dane$Rings == c(5,10),]
zb_1 <- zb_1[c(4,5,9)]

Drzewko_2 <- Tree( "Rings", c("Height"), zb_1, 5, 1 )
print( Drzewko_2, "Count","Prob","Leaf")

# Klasyfikacja wieloklasowa

CrossValidTune(dane_1, 4, 4, 123)

X = dane_1[1:10,-9]
zbior(dane_1)
y_tar = dane_1[1:10,9]
KNNmodel = KNNtrain( X[,order(colnames(X))], y_tar, 4, 0, 1)
KNNpred(KNNmodel, X)

y_hat <- KNNpred(KNNmodel, X)

ModelOcena <- function(y_tar, y_hat)
  
X <- dane_1
X$Rings <- sub("^", "ring_", X_2$Rings )
y_tar <- model.matrix(~ X$Rings -1 )
X <- as.matrix( X[,-c(1,9)] )
siec_wieloklasowe <- trainNN(X, y_tar, h = c(5,5), lr = 0.01, iter = 500, seed = 123 )
head(siec_wieloklasowe$y_hat)
rowSums(siec_wieloklasowe$y_hat)
