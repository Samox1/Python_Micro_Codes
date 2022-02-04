#wczytywanie danych
#klasyfikacja binarna
dane_bin <- read.table("C:\\Users\\Karolina\\Desktop\\TED\\projekt\\binarna.txt", header = T, sep = "\t")
dane_bin$y<-as.factor(dane_bin$y)
dane_bin$x1<-as.numeric(dane_bin$x1)
dane_bin$x4<-as.numeric(dane_bin$x4)
dane_bin$x5<-as.numeric(dane_bin$x5)
dane_bin$x6<-as.numeric(dane_bin$x6)
dane_bin$x7<-as.numeric(dane_bin$x7)
dane_bin$x8<-as.numeric(dane_bin$x8)
dane_bin$x9<-as.numeric(dane_bin$x9)
dane_bin$x10<-as.numeric(dane_bin$x10)
dane_bin$x11<-as.numeric(dane_bin$x11)
dane_bin$x12<-as.numeric(dane_bin$x12)
dane_bin$x13<-as.numeric(dane_bin$x13)
dane_bin$x14<-as.numeric(dane_bin$x14)
dane_bin$x15<-as.numeric(dane_bin$x15)
str(dane_bin)
#regresja
dane_reg<- read.table("C:\\Users\\Karolina\\Desktop\\TED\\projekt\\regresja.txt", header = T, sep = "\t")

str(dane_reg)
#klasyfikacja wiloklasowa
dane_wielo<- read.table("C:\\Users\\Karolina\\Desktop\\TED\\projekt\\iris.txt", header = T, sep = "\t")
dane_wielo$y<-as.factor(dane_wilo$y)
str(dane_wilo)
#podzia na zbiór testowy i treningowy klacyfinacja binarna
indxT_bin <- sample(c(rep(0, 0.7 * nrow(dane_bin)), rep(1, 0.3 * nrow(dane_bin))))


dane_bin_train<- dane_bin[ indxT_bin==0, ]
dane_bin_test<- dane_bin[indxT_bin==1, ]

#podzia na zbiór testowy i treningowy regresja
indxT_reg <- sample(c(rep(0, 0.7 * nrow(dane_reg)), rep(1, 0.3 * nrow(dane_reg))))


dane_reg_train<- dane_reg[ indxT_reg==0, ]
dane_reg_test<- dane_reg[indxT_reg==1, ]
#podzia na zbiór testowy i treningowy klasyfikacja welokalsowa
indxT_wielo <- sample(c(rep(0, 0.7 * nrow(dane_wielo)), rep(1, 0.3 * nrow(dane_wielo))))


dane_wielo_train<- dane_wielo[ indxT_wielo==0, ]
dane_wielo_test<- dane_wielo[indxT_wielo==1, ]
#2) Rozwi¹zuj¹ Pañstwo 3 powy¿sze problemy wykorzystuj¹c opracowane algorytmy:
  #   a) Dla klasyfikacji binarnej: k-najbli¿szych s¹siadów, drzewa decyzyjne, sieci neuronowe.
  #   b) Dla klasyfikacji wieloklasowej: k-najbli¿szych s¹siadów, drzewa decyzyjne, sieci neuronowe.
#     c) Dla regresji: k-najbli¿szych s¹siadów, drzewa decyzyjne, sieci neuronowe.
  # 

#   
#k-najbli¿szych s¹siadów regresja
KNN_train_reg<-KNN_train( dane_reg_train[,-7],dane_reg_train[,7], k = 5,1,100)
KNN_pred(KNN_train_reg,dane_reg_test[,-7])
#k-najbli¿szych s¹siadów klasyfikacja wiloklasowa
KNN_train_wielo<-KNN_train( dane_wielo_train[,-5],dane_wielo_train[,5], k = 5,1,100)
KNN_pred(KNN_train_wielo,dane_wielo_test[,-5])
#k-najbli¿szych s¹siadów klasyfikacja binarna
KNN_train_bin<-KNN_train( dane_bin_train[,-17],dane_bin_train[,17], k = 5,1,100)
KNN_pred(KNN_train_bin,dane_bin_test[,-17])

#drzewa decyzyjne regresja

tree_regesja<-Tree(Y="y", X=c("x1","x2","x3","x4","x5","x6"), data=dane_reg_train, type="SS", depth=6, minobs=1, overfit="prune", cf=0.3)
print(tree_regesja, "Count", "Leaf", "Depth")
PredictTree(tree_regesja, dane_reg_train[,7])
#drzewa decyzyjne klasyfikacja binarna                               
tree_binarna<-Tree(Y="y", X=c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16"), data=dane_bin_train, type="Entropy", depth=5, minobs=1, overfit="none", cf=0.3)
print(tree_binarna, "Count", "Class", "Prob", "Leaf", "Depth")
PredictTree(tree_binarna, dane_bin_train[,-17])
#drzewa decyzyjne klasyfikacja wieloklasowa                               
tree_wielo<-Tree(Y="y", X=c("x1","x2","x3","x4"), data=dane_wielo_train, type="Entropy", depth=5, minobs=1, overfit="none", cf=0.3)
print(tree_wielo, "Count", "Class", "Prob", "Leaf", "Depth")
PredictTree(tree_wielo, dane_wielo_train[,-5])
#sieci neuronowe regresja
reg_minmax <- dane_reg_train[ , ]

reg_minmax <- sapply( reg_minmax, MinMax )
summary(reg_minmax)

Xnames <- colnames(iris_minmax)[-5]
Yname <- colnames(iris_minmax)[5]

Siec_reg <- trainNN_NOWE_K(Yname = Yname, Xnames = Xnames, data = reg_minmax, h = c(3,7), lr =  0.01, Maxiter = 500, seed = 525)    
reg_test_minmax <- dane_reg_test[ , ]

reg_test_minmax <- sapply( reg_test_minmax, MinMax )
x_pred <- reg_test_minmax[,]
y_pred <- predNN_NOWE_K( x_pred[,Xnames], Siec_reg)
y_pred 

table(x_pred[,Yname], ifelse(y_pred >= 0.5, 1, 0))


#sieci neuronowe klasyfjkacja wiloklasowa
iris_minmax <- dane_wielo_train[ dane_wielo_train$y !="Iris-setosa", ]
iris_minmax$y <- ifelse( iris_minmax$y == "Iris-versicolor", 1, 0 )
iris_minmax <- sapply( iris_minmax, MinMax )
summary(iris_minmax)

Xnames <- colnames(iris_minmax)[-5]
Yname <- colnames(iris_minmax)[5]

Siec <- trainNN_NOWE_K(Yname = Yname, Xnames = Xnames, data = iris_minmax, h = c(3,7), lr =  0.01, Maxiter = 500, seed = 525)    
iris_minmax <- dane_wielo_test[ dane_wielo_test$y !="Iris-setosa", ]
iris_minmax$y <- ifelse( iris_minmax$y == "Iris-versicolor", 1, 0 )
iris_minmax <- sapply( iris_minmax, MinMax )
x_pred <- iris_minmax[,]
y_pred <- predNN_NOWE_K( x_pred[,Xnames], Siec)

table(x_pred[,Yname], ifelse(y_pred >= 0.5, 1, 0))


#sieci neuronowe binarane 
bin_minmax <- dane_bin_train[ , ]
bin_minmax$y <- ifelse( bin_minmax$y == "1", 1, 0 )
bin_minmax <- sapply( bin_minmax, MinMax )
summary(bin_minmax)

Xnames <- colnames(bin_minmax)[-5]
Yname <- colnames(bin_minmax)[5]

Siec_bin <- trainNN_NOWE_K(Yname = Yname, Xnames = Xnames, data = bin_minmax, h = c(3,7), lr =  0.01, Maxiter = 500, seed = 525)    
bin_test_minmax <- dane_bin_test[ , ]
bin_test_minmax$y <- ifelse( bin_test_minmax$y == "1", 1, 0 )
bin_test_minmax <- sapply( bin_test_minmax, MinMax )
x_pred <- bin_test_minmax[,]
y_pred <- predNN_NOWE_K( x_pred[,Xnames], Siec_bin)
y_pred 

table(x_pred[,Yname], ifelse(y_pred >= 0.5, 1, 0))

#3) Porównuj¹ Pañstwo wyniki otrzymane dla w³asnych algorymtów z wynikami otrzymanymi dla algorytmów pochodz¹cych z innych 
#    pakietów w R, np: caret, rpart, nnet, neuralnet.
install.packages( "caret" )
library( "caret" )
#k-najbli¿szych s¹siadów regresja
knnFit_reg <- train(y ~ .,
                data = dane_reg_train, 
                method = "knn")
regresja <- predict(knnFit_reg, dane_reg_test)

#k-najbli¿szych s¹siadów klasyfikacja wielkoasowa
knnFit_wielo <- train(y ~ .,
                    data = dane_wielo_train, 
                    method = "knn")
knn_klasyfikacja_wiloklasowa <- predict(knnFit_wielo , dane_wielo_test,'prob')
#k-najbli¿szych s¹siadów klasyfikacja binarna
knnFit_bin <- train(y ~ .,
                      data = dane_bin_train, 
                      method = "knn")
knn_klasyfikacja_wiloklasowa <- predict(knnFit_bin , dane_bin_test,'prob')
#drzewa decyzyjne regresja
install.packages( "rpart" )
library( "rpart" )
tree_reg <- rpart(y~., data =dane_reg_train)
predict(tree_reg, dane_reg_test)
#drzewa decyzyjne klasyfikacja binarna
install.packages( "rpart" )
library( "rpart" )
str(dane_reg_train)
tree_bin <- rpart(y~., data =dane_bin_train)
predict(tree_bin, dane_bin_test, type = 'prob')
#drzewa decyzyjne klasyfikacja wieloklasowa
install.packages( "rpart" )
library( "rpart" )
str(dane_reg_train)
tree_wielo <- rpart(y~., data =dane_wielo_train)
predict(tree_wielo, dane_wielo_test, type = 'prob')
#siec neuronowa regresja
install.packages( "neuralnet" )
library("neuralnet")
siec_reg<- neuralnet(y ~., 
                 data=dane_reg_train, hidden=2,linear.output = FALSE)
#siec neuronowa klasyfikacja binarna
install.packages( "neuralnet" )
library("neuralnet")
siec_bin<- neuralnet(y ~., 
                 data=dane_bin_train, hidden=2,linear.output = FALSE)
#siec neuronowa klasyfikacja wieloklasowa
install.packages( "neuralnet" )
library("neuralnet")
siec_wielo<- neuralnet(y ~., 
                     data=dane_wielo_train, hidden=2,linear.output = FALSE)