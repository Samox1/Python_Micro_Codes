source("funkcje.R")


### Wczytywanie danych

# Klasyfikacja Binarna
dane_bin <- read.table("binarna.txt", header = T, sep = "\t")
dane_bin$y <-as.factor(dane_bin$y)
dane_bin[,-length(dane_bin)] <- sapply(dane_bin[,-length(dane_bin)], as.numeric)
dane_bin_Y <- "y"
dane_bin_X <- colnames(dane_bin)[-(which(colnames(dane_bin) == dane_bin_Y))]
print(summary(dane_bin))


# Klasyfikacja Wieloklasowa
dane_wielo <- read.table("iris.txt", header = T, sep = "\t")
dane_wielo$y <- as.factor(dane_wielo$y)
dane_wielo_Y <- "y"
dane_wielo_X <- colnames(dane_wielo)[-(which(colnames(dane_wielo) == dane_wielo_Y))]
print(summary(dane_wielo))


# Regresja
dane_reg <- read.table("regresja.txt", header = T, sep = "\t")
dane_reg_Y <- "y"
dane_reg_X <- colnames(dane_reg)[-(which(colnames(dane_reg) == dane_reg_Y))]
print(summary(dane_reg))



# Obliczenia

# KNN - bin
KNN_bin_parametry <- expand.grid(k=c(3,6,9,12,15,20))  
KNN_bin_kroswalidacja <- Kroswalidacja(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, KNN_bin_parametry2, algorytm="KNN", seed = 428)

KNN_bin_kroswalidacja_2 <- KNN_bin_kroswalidacja
KNN_bin_kroswalidacja_2[is.na(KNN_bin_kroswalidacja_2)] <- 0
KNN_bin_kroswalidacja_grupowana <- as.data.frame(KNN_bin_kroswalidacja_2 %>% group_by(k) %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
KNN_bin_najlepsze_T <- KNN_bin_kroswalidacja_grupowana[which.max(KNN_bin_kroswalidacja_grupowana$JakoscT),]
KNN_bin_najlepsze_W <- KNN_bin_kroswalidacja_grupowana[which.max(KNN_bin_kroswalidacja_grupowana$JakoscW),]

print(KNN_bin_kroswalidacja_grupowana)
print(KNN_bin_najlepsze_T)
print(KNN_bin_najlepsze_W)


# KNN - wielo
KNN_wielo_parametry <- expand.grid(k=c(3,6,9,12,15,20))  
KNN_wielo_kroswalidacja <- Kroswalidacja(dane_wielo, dane_wielo_X, dane_wielo_Y, kFold = 8, KNN_wielo_parametry, algorytm="KNN", seed = 428)

KNN_wielo_kroswalidacja_grupowana <- as.data.frame(KNN_wielo_kroswalidacja %>% group_by(k) %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
KNN_wielo_najlepsze_T <- KNN_wielo_kroswalidacja_grupowana[which.max(KNN_wielo_kroswalidacja_grupowana$ACCT),]
KNN_wielo_najlepsze_W <- KNN_wielo_kroswalidacja_grupowana[which.max(KNN_wielo_kroswalidacja_grupowana$ACCW),]

print(KNN_wielo_kroswalidacja_grupowana)
print(KNN_wielo_najlepsze_T)
print(KNN_wielo_najlepsze_W)


# KNN - reg
KNN_reg_parametry <- expand.grid(k=c(3,6,9,12,15,20))  
KNN_reg_kroswalidacja <- Kroswalidacja(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, KNN_reg_parametry, algorytm="KNN", seed = 428)

KNN_reg_kroswalidacja_grupowana <- as.data.frame(KNN_reg_kroswalidacja %>% group_by(k) %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
KNN_reg_najlepsze_T <- KNN_reg_kroswalidacja_grupowana[which.min(KNN_reg_kroswalidacja_grupowana$MAET),]
KNN_reg_najlepsze_W <- KNN_reg_kroswalidacja_grupowana[which.min(KNN_reg_kroswalidacja_grupowana$MAEW),]

print(KNN_reg_kroswalidacja_grupowana)
print(KNN_reg_najlepsze_T)
print(KNN_reg_najlepsze_W)




# Drzewa - bin
Tree_bin_parametry <- expand.grid(depth=c(2,4,6), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.05, 0.15))
Tree_bin_kroswalidacja <- Kroswalidacja(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, Tree_bin_parametry, algorytm="Drzewa", seed = 428)

Tree_bin_kroswalidacja[is.na(Tree_bin_kroswalidacja)] <- 0
Tree_bin_kroswalidacja_grupowana <- as.data.frame(Tree_bin_kroswalidacja %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Tree_bin_najlepsze_T <- Tree_bin_kroswalidacja_grupowana[which.max(Tree_bin_kroswalidacja_grupowana$JakoscT),]
Tree_bin_najlepsze_W <- Tree_bin_kroswalidacja_grupowana[which.max(Tree_bin_kroswalidacja_grupowana$JakoscW),]

print(Tree_bin_kroswalidacja_grupowana)
print(Tree_bin_najlepsze_T)
print(Tree_bin_najlepsze_W)


# Drzewa - wielo
Tree_wielo_parametry <- expand.grid(depth=c(2,4,6), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.05, 0.15))
Tree_wielo_kroswalidacja <- Kroswalidacja(dane_wielo, dane_wielo_X, dane_wielo_Y, kFold = 8, Tree_wielo_parametry, algorytm="Drzewa", seed = 428)

Tree_wielo_kroswalidacja[is.na(Tree_wielo_kroswalidacja)] <- 0
Tree_wielo_kroswalidacja_grupowana <- as.data.frame(Tree_wielo_kroswalidacja %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Tree_wielo_najlepsze_T <- Tree_wielo_kroswalidacja_grupowana[which.max(Tree_wielo_kroswalidacja_grupowana$ACCT),]
Tree_wielo_najlepsze_W <- Tree_wielo_kroswalidacja_grupowana[which.max(Tree_wielo_kroswalidacja_grupowana$ACCW),]

print(Tree_wielo_kroswalidacja_grupowana)
print(Tree_wielo_najlepsze_T)
print(Tree_wielo_najlepsze_W)


# Drzewa - reg
Tree_reg_parametry <- expand.grid(depth=c(2,4,6), minobs=c(2,4,6), type=c('SS'), overfit = c('none'), cf=c(0.1))
Tree_reg_kroswalidacja <- Kroswalidacja(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, Tree_reg_parametry, algorytm="Drzewa", seed = 428)

Tree_reg_kroswalidacja[is.na(Tree_reg_kroswalidacja)] <- 0
Tree_reg_kroswalidacja_grupowana <- as.data.frame(Tree_reg_kroswalidacja %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Tree_reg_najlepsze_T <- Tree_reg_kroswalidacja_grupowana[which.min(Tree_reg_kroswalidacja_grupowana$MAET),]
Tree_reg_najlepsze_W <- Tree_reg_kroswalidacja_grupowana[which.min(Tree_reg_kroswalidacja_grupowana$MAEW),]

print(Tree_reg_kroswalidacja_grupowana)
print(Tree_reg_najlepsze_T)
print(Tree_reg_najlepsze_W)





# Sieci - bin
Sieci_bin_parametry <- expand.grid(h=list(c(3), c(5), c(6), c(3,7), c(4,6), c(5,5)), lr = c(0.01), iter = c(20000, 80000))
Sieci_bin_kroswalidacja <- Kroswalidacja(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, Sieci_bin_parametry, algorytm="Sieci", seed = 428)

Sieci_bin_kroswalidacja_wynik <- Sieci_bin_kroswalidacja
Sieci_bin_kroswalidacja_wynik[is.na(Sieci_bin_kroswalidacja_wynik)] <- 0
Sieci_bin_kroswalidacja_wynik$h <- as.character(Sieci_bin_kroswalidacja_wynik$h)
Sieci_bin_kroswalidacja_wynik$h <- str_remove(Sieci_bin_kroswalidacja_wynik$h, pattern = "c")
Sieci_bin_kroswalidacja_wynik_grupowana <- as.data.frame(Sieci_bin_kroswalidacja_wynik %>% group_by( h, lr, iter) %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Sieci_bin_najlepsze_T <- Sieci_bin_kroswalidacja_wynik_grupowana[which.max(Sieci_bin_kroswalidacja_wynik_grupowana$JakoscT),]
Sieci_bin_najlepsze_W <- Sieci_bin_kroswalidacja_wynik_grupowana[which.max(Sieci_bin_kroswalidacja_wynik_grupowana$JakoscW),]

print(Sieci_bin_kroswalidacja_wynik_grupowana)
print(Sieci_bin_najlepsze_T)
print(Sieci_bin_najlepsze_W)


# Sieci - wielo
Sieci_wielo_parametry <- expand.grid(h=list(c(3), c(5), c(6), c(3,7), c(4,6), c(5,5)), lr = c(0.01), iter = c(20000, 80000))
Sieci_wielo_kroswalidacja <- Kroswalidacja(dane_wielo, dane_wielo_X, dane_wielo_Y, kFold = 8, Sieci_wielo_parametry, algorytm="Sieci", seed = 428)

Sieci_wielo_kroswalidacja_wynik <- Sieci_wielo_kroswalidacja
Sieci_wielo_kroswalidacja_wynik[is.na(Sieci_wielo_kroswalidacja_wynik)] <- 0
Sieci_wielo_kroswalidacja_wynik$h <- as.character(Sieci_wielo_kroswalidacja_wynik$h)
Sieci_wielo_kroswalidacja_wynik$h <- str_remove(Sieci_wielo_kroswalidacja_wynik$h, pattern = "c")
Sieci_wielo_kroswalidacja_wynik_grupowana <- as.data.frame(Sieci_wielo_kroswalidacja_wynik %>% group_by( h, lr, iter) %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Sieci_wielo_najlepsze_T <- Sieci_wielo_kroswalidacja_wynik_grupowana[which.max(Sieci_wielo_kroswalidacja_wynik_grupowana$ACCT),]
Sieci_wielo_najlepsze_W <- Sieci_wielo_kroswalidacja_wynik_grupowana[which.max(Sieci_wielo_kroswalidacja_wynik_grupowana$ACCW),]

print(Sieci_wielo_kroswalidacja_wynik_grupowana)
print(Sieci_wielo_najlepsze_T)
print(Sieci_wielo_najlepsze_W)


# Sieci - reg
Sieci_reg_parametry <- expand.grid(h=list(c(3,7), c(4,6), c(5,5)), lr = c(0.01), iter = c(20000, 80000))
Sieci_reg_kroswalidacja <- Kroswalidacja(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, Sieci_reg_parametry, algorytm="Sieci", seed = 428)

Sieci_reg_kroswalidacja_wynik <- Sieci_reg_kroswalidacja
Sieci_reg_kroswalidacja_wynik[is.na(Sieci_reg_kroswalidacja_wynik)] <- 0
Sieci_reg_kroswalidacja_wynik$h <- as.character(Sieci_reg_kroswalidacja_wynik$h)
Sieci_reg_kroswalidacja_wynik$h <- str_remove(Sieci_reg_kroswalidacja_wynik$h, pattern = "c")
Sieci_reg_kroswalidacja_wynik_grupowana <- as.data.frame(Sieci_reg_kroswalidacja_wynik %>% group_by( h, lr, iter) %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Sieci_reg_najlepsze_T <- Sieci_reg_kroswalidacja_wynik_grupowana[which.min(Sieci_reg_kroswalidacja_wynik_grupowana$MAET),]
Sieci_reg_najlepsze_W <- Sieci_reg_kroswalidacja_wynik_grupowana[which.min(Sieci_reg_kroswalidacja_wynik_grupowana$MAEW),]

print(Sieci_reg_kroswalidacja_wynik_grupowana)
print(Sieci_reg_najlepsze_T)
print(Sieci_reg_najlepsze_W)






### Modele z bibliotek R 
# indxT_bin <- sample(c(rep(0, 0.9 * nrow(dane_bin)), rep(1, 0.1 * nrow(dane_bin))))
# dane_bin_train <- dane_bin[indxT_bin==0, ]
# dane_bin_test <- dane_bin[indxT_bin==1, ]
# 
# indxT_wielo <- sample(c(rep(0, 0.9 * nrow(dane_wielo)), rep(1, 0.1 * nrow(dane_wielo))))
# dane_wielo_train<- dane_wielo[ indxT_wielo==0, ]
# dane_wielo_test<- dane_wielo[indxT_wielo==1, ]
# 
# indxT_reg <- sample(c(rep(0, 0.9 * nrow(dane_reg)), rep(1, 0.1 * nrow(dane_reg))))
# dane_reg_train<- dane_reg[ indxT_reg==0, ]
# dane_reg_test<- dane_reg[indxT_reg==1, ]


# KNN - caret

knn_bin_cv <- c()
for (i in 1:8) {
  indxT_bin <- sample(c(rep(0, 0.9 * nrow(dane_bin)), rep(1, 0.1 * nrow(dane_bin))))
  dane_bin_train <- dane_bin[indxT_bin==0, ]
  dane_bin_test <- dane_bin[indxT_bin==1, ]
  
  knnFit_bin <- train(y ~ .,data = dane_bin_train, method = "knn")
  knn_klasyfikacja_binarna <- predict(knnFit_bin , dane_bin_test,'prob')
  knn_bin_cv <- rbind(knn_bin_cv, Model_Ocena(dane_bin_test$y , 1-knn_klasyfikacja_binarna[,1]))
}
knn_bin_cv


knn_wielo_cv <- c()
for (i in 1:8) {
  indxT_wielo <- sample(c(rep(0, 0.9 * nrow(dane_wielo)), rep(1, 0.1 * nrow(dane_wielo))))
  dane_wielo_train<- dane_wielo[ indxT_wielo==0, ]
  dane_wielo_test<- dane_wielo[indxT_wielo==1, ]
  
  knnFit_wielo <- train(y ~ .,data = dane_wielo_train, method = "knn")
  knn_klasyfikacja_wieloklasowa <- predict(knnFit_wielo , dane_wielo_test,'prob')
  knn_wielo_cv <- rbind(knn_wielo_cv, Model_Ocena(dane_wielo_test$y , knn_klasyfikacja_wieloklasowa[,1]))
}
knn_wielo_cv


knn_reg_cv <- c()
for (i in 1:8) {
  indxT_reg <- sample(c(rep(0, 0.9 * nrow(dane_reg)), rep(1, 0.1 * nrow(dane_reg))))
  dane_reg_train<- dane_reg[ indxT_reg==0, ]
  dane_reg_test<- dane_reg[indxT_reg==1, ]
  
  knnFit_reg <- train(y ~ .,data = dane_reg_train, method = "knn")
  knn_regresja <- predict(knnFit_reg, dane_reg_test)
  knn_reg_cv <- rbind(knn_reg_cv, Model_Ocena(dane_reg_test$y , knn_regresja))
}
knn_reg_cv




# Drzewa - Rpart

tree_bin_cv <- c()
for (i in 1:250) {
  indxT_bin <- sample(c(rep(0, 0.9 * nrow(dane_bin)), rep(1, 0.1 * nrow(dane_bin))))
  dane_bin_train <- dane_bin[indxT_bin==0, ]
  dane_bin_test <- dane_bin[indxT_bin==1, ]
  
  tree_bin <- rpart(y~., data =dane_bin_train)
  tree_bin_pred <- predict(tree_bin, dane_bin_test, type = 'prob')
  if(any(is.na(Model_Ocena(dane_bin_test$y , tree_bin_pred[,1]))) == FALSE)
  {
    tree_bin_cv <- rbind(tree_bin_cv, Model_Ocena(dane_bin_test$y , tree_bin_pred[,1]))
    if(nrow(tree_bin_cv) == 8) break
  }
  
}
tree_bin_cv


tree_wielo_cv <- c()
for (i in 1:8) {
  indxT_wielo <- sample(c(rep(0, 0.9 * nrow(dane_wielo)), rep(1, 0.1 * nrow(dane_wielo))))
  dane_wielo_train<- dane_wielo[ indxT_wielo==0, ]
  dane_wielo_test<- dane_wielo[indxT_wielo==1, ]
  
  tree_wielo <- rpart(y~., data =dane_wielo_train)
  tree_wielo_pred <- predict(tree_wielo, dane_wielo_test, type = 'prob')
  tree_wielo_cv <- rbind(tree_wielo_cv, Model_Ocena(dane_wielo_test$y , tree_wielo_pred))
}
tree_wielo_cv


tree_reg_cv <- c()
for (i in 1:8) {
  indxT_reg <- sample(c(rep(0, 0.9 * nrow(dane_reg)), rep(1, 0.1 * nrow(dane_reg))))
  dane_reg_train<- dane_reg[ indxT_reg==0, ]
  dane_reg_test<- dane_reg[indxT_reg==1, ]
  
  tree_reg <- rpart(y~., data =dane_reg_train)
  tree_reg_pred <- predict(tree_reg, dane_reg_test)
  tree_reg_cv <- rbind(tree_reg_cv, Model_Ocena(dane_reg_test$y , tree_reg_pred))
}
tree_reg_cv


# tree_bin <- rpart(y~., data =dane_bin_train)
# tree_bin_pred <- predict(tree_bin, dane_bin_test, type = 'prob')
# Model_Ocena(dane_bin_test$y , tree_bin_pred[,1])
# 
# tree_wielo <- rpart(y~., data =dane_wielo_train)
# tree_wielo_pred <- predict(tree_wielo, dane_wielo_test, type = 'prob')
# Model_Ocena(dane_wielo_test$y , tree_wielo_pred)
# 
# tree_reg <- rpart(y~., data =dane_reg_train)
# tree_reg_pred <- predict(tree_reg, dane_reg_test)
# Model_Ocena(dane_reg_test$y , tree_reg_pred)


siec_bin_cv <- c()
for (i in 1:8) {
  indxT_bin <- sample(c(rep(0, 0.8 * nrow(dane_bin)), rep(1, 0.2 * nrow(dane_bin))))
  dane_bin_train <- dane_bin[indxT_bin==0, ]
  dane_bin_test <- dane_bin[indxT_bin==1, ]
  
  siec_bin<- neuralnet(y ~., data=dane_bin_train, hidden=2,linear.output = FALSE)
  siec_bin_pred <- predict(siec_bin, dane_bin_test, type = 'prob')
  if(any(is.na(Model_Ocena(dane_bin_test$y , siec_bin_pred[,1]))) == FALSE)
  {
    siec_bin_cv <- rbind(siec_bin_cv, Model_Ocena(dane_bin_test$y , siec_bin_pred[,1]))
    if(nrow(siec_bin_cv) == 8) break
  }
}
siec_bin_cv


siec_wielo_cv <- c()
for (i in 1:8) {
  indxT_wielo <- sample(c(rep(0, 0.8 * nrow(dane_wielo)), rep(1, 0.2 * nrow(dane_wielo))))
  dane_wielo_train<- dane_wielo[ indxT_wielo==0, ]
  dane_wielo_test<- dane_wielo[indxT_wielo==1, ]
  
  siec_wielo<- neuralnet(y ~., data=dane_wielo_train, hidden=2,linear.output = FALSE)
  siec_wielo_pred <- predict(siec_wielo, dane_wielo_test, type = 'prob')
  siec_wielo_cv <- rbind(siec_wielo_cv, Model_Ocena(dane_wielo_test$y , siec_wielo_pred))
}
siec_wielo_cv


siec_reg_cv <- c()
for (i in 1:8) {
  indxT_reg <- sample(c(rep(0, 0.9 * nrow(dane_reg)), rep(1, 0.1 * nrow(dane_reg))))
  dane_reg_train<- dane_reg[ indxT_reg==0, ]
  dane_reg_test<- dane_reg[indxT_reg==1, ]
  
  siec_reg<- neuralnet(y ~., data=dane_reg_train, hidden=2,linear.output = FALSE)
  siec_reg_pred <- predict(siec_reg, dane_reg_test)
  siec_reg_cv <- rbind(siec_reg_cv, Model_Ocena(dane_reg_test$y , siec_reg_pred))
}
siec_reg_cv


# siec_bin<- neuralnet(y ~., data=dane_bin_train, hidden=2,linear.output = FALSE)
# siec_bin_pred <- predict(siec_bin, dane_bin_test, type = 'prob')
# Model_Ocena(dane_bin_test$y , siec_bin_pred[,1])
# 
# siec_wielo<- neuralnet(y ~., data=dane_wielo_train, hidden=2,linear.output = FALSE)
# siec_wielo_pred <- predict(siec_wielo, dane_wielo_test, type = 'prob')
# Model_Ocena(dane_wielo_test$y , siec_wielo_pred)
# 
# siec_reg<- neuralnet(y ~., data=dane_reg_train, hidden=2,linear.output = FALSE)
# siec_reg_pred <- predict(siec_reg, dane_reg_test)
# Model_Ocena(dane_reg_test$y , siec_reg_pred)

