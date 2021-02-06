library(caret)
library(rpart)
library(nnet)
library(neuralnet)
library(kernlab)
library(e1071)
#DANE REGRESJA
library(readxl)
Regresja <- read_excel("C:/Users/Karolina/Desktop/MGR/3 SEMESTR/Techniki eksploracji danych/Projekt/Regresja/Concrete_Data.xls")
View(Regresja)
colnames(Regresja)<-c("Cement","Zuzel","Popiol","Woda","Superplastyfikator","Krusz_grube","Krusz_drobne","Wiek","Wytrzymalosc")
#######################################################k-najblizszych sasiadow wbudowana 1

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
Regresja_norm <- as.data.frame(lapply(Regresja[,c(1,2,3,4,5,6,7,8)], nor))

summary(Regresja_norm)
test<-1:100
##extract training set
Regresja_train <- Regresja[1:as.integer(0.7*nrow(Regresja)),]
Regresja_train <- Regresja[-test,]
##extract testing set
Regresja_test <- Regresja[as.integer(0.7*nrow(Regresja)+1):nrow(Regresja),]
Regresja_test <- Regresja[test,]
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
Regresja_target_category <- Regresja$Wytrzymalosc[-test]
##extract 5th column if test dataset to measure the accuracy
Regresja_test_category <- Regresja$Wytrzymalosc[test]
##load the package class
library(class)
##run knn function
pr <- knn(Regresja_train,Regresja_test,cl=Regresja_target_category,k=5)

##create confusion matrix
tab <- table(pr,Regresja_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
#k-najbliższych sąsiadów wbudowana 2
#obliczanie K ze wzoru (pierwiastek z N)
set.seed(2)
K <- round(sqrt(nrow(Regresja)),0)
dim(Regresja)
summary(Regresja)
#podzial zbioru na 70:30, 
Regresja_train <- Regresja[1:as.integer(0.7*nrow(Regresja)),]
Regresja_test <- Regresja[as.integer(0.7*nrow(Regresja)+1):nrow(Regresja),]
#obliczenie odleglosci Euklidesowej

euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

#funkcja KNN
knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  #empty pred vector 
  #LOOP-1
  for(i in c(1:nrow(test_data))){   #looping over each record of test data
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    good = 0              #good & bad variable initialization with 0 value
    bad = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      
      #adding class variable of training data in eu_char
      eu_char <- c(eu_char, as.character(train_data[j,][[6]]))
    }
    
    eu <- data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    
    eu <- eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu <- eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"eu_char"]) == "g"){
        good = good + 1
      }
      else
        bad = bad + 1
    }
    
    # Compares the no. of neighbors with class label good or bad
    if(good > bad){          #if majority of neighbors are good then put "g" in pred vector
      
      pred <- c(pred, "g")
    }
    else if(good < bad){
      #if majority of neighbors are bad then put "b" in pred vector
      pred <- c(pred, "b")
    }
    
  }
  return(pred) #return pred vector
}

#dokladnosc
accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,9] == test_data[i,10]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}
#predykcja
predictions <- knn_predict(Regresja_test, Regresja_train, 5) #calling knn_predict()

Regresja_test[,10] <- predictions #Adding predictions in test data as 7th column
print(accuracy(Regresja_test))

#drzewo decyzyjne funkcja wbudowana
library(tree)
Regresja.ltr <- tree(Wytrzymalosc ~Cement+Zuzel+Popiol+Woda+Superplastyfikator+Krusz_grube+Krusz_drobne+Wiek, Regresja)
Regresja.ltr

#drzewo decyzyjne kod wlasny
shuffle_index <- sample(1:nrow(Regresja))
head(shuffle_index)

titanic <- titanic[shuffle_index, ]
head(titanic)

#maszynę wektorów nośnych funkcja wbudowana
set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
library(e1071)
dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, dat)
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}
xgrid = make.grid(x)
xgrid[1:10,]
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
beta = drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

#K-NAJBLIZSZYCH SASIADOW KOD WLASNY (ZAJECIA 3)
# Normalizacja

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}

set.seed( 123 )
y <- runif( 100 )

summary( MinMax( y ) )
summary( MinMax( y, 10, 25 ) )

summary( MinMax( Regresja ) )
summary( MinMax( y, 10, 25 ) )

# Miary odległości

d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

set.seed(123)
zbiorD <- data.frame( x1 = c( rep(1,5), rep(2,5) ), x2 = runif(10), x3 = rnorm(10)  )
zbiorD

d_euklides( zbiorD[1,], zbiorD[2,] )
d_euklides( zbiorD[1,], zbiorD[1,] )

d_euklides( Regresja[1,], Regresja[2,] )
d_euklides( Regresja[1,], Regresja[1,] )

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

dEuklides(Regresja)
dist(Regresja)
# K-NN

KNNtrain <- function( X, y_tar, k = 5 ){
  # k-d tree
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k
  return( knn )
}

KNNreg <- KNNtrain( Regresja[,-9], Regresja[,9], 5 )

KKNpred <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[ i, j ] <- d_euklides( model$X[i,], Xnew[j,] )
    }
  }
  pred <- double( nPred )
  for( i in 1:nPred ){
    kNaj <- order( odl[,i] )
    kNaj <- kNaj[1:model$k]
    y_hat <- mean( model$y[kNaj] )
    pred[i] <- y_hat
  }
  return( pred )
}

KKNpred(KNNreg, Regresja[,-9])

KNNreg_pakiet <- knnreg( Regresja[,-9], Regresja[,9], K )
str(KNNreg_pakiet)

predict( KNNreg_pakiet, Regresja[,-9] )
KKNpred( KNNreg, Regresja[,-9] )
