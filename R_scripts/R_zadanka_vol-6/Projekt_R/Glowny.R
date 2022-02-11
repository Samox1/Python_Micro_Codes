source('Funkcje.R')


  ##  DANE  ##

dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=FALSE, sep = ",")
dane_bin <- dane_bin[,-1]    
dane_bin_X <- colnames(dane_bin)[-1]
dane_bin_Y <- colnames(dane_bin)[1]
dane_bin[,1] <- as.factor(dane_bin[,1])
print(summary(dane_bin))


dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header = FALSE, sep = ",")
dane_multi[,1] <- as.character(dane_multi[,1])
dane_multi[,2] <- as.character(dane_multi[,2])
dane_multi[,3] <- as.character(dane_multi[,3])
dane_multi[,4] <- as.character(dane_multi[,4])
dane_multi[,5] <- as.character(dane_multi[,5])
dane_multi[,6] <- as.character(dane_multi[,6])
dane_multi <- przeksztalcenie_multi(dane_multi)
# dane_multi[,1] <- as.factor(dane_multi[,1])
# dane_multi[,2] <- as.factor(dane_multi[,2])
# dane_multi[,3] <- as.factor(dane_multi[,3])
# dane_multi[,4] <- as.factor(dane_multi[,4])
# dane_multi[,5] <- as.factor(dane_multi[,5])
# dane_multi[,6] <- as.factor(dane_multi[,6])
dane_multi[,1] <- as.numeric(dane_multi[,1])
dane_multi[,2] <- as.numeric(dane_multi[,2])
dane_multi[,3] <- as.numeric(dane_multi[,3])
dane_multi[,4] <- as.numeric(dane_multi[,4])
dane_multi[,5] <- as.numeric(dane_multi[,5])
dane_multi[,6] <- as.numeric(dane_multi[,6])
dane_multi[,7] <- as.factor(dane_multi[,7])
dane_multi_X <- colnames(dane_multi)[-7]
dane_multi_Y <- colnames(dane_multi)[7]
print(summary(dane_multi))


dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv", header = TRUE, sep = ",")
dane_reg[,3] <- as.character(dane_reg[,3])
dane_reg[,4] <- as.character(dane_reg[,4])
dane_reg <- przeksztalcenie_reg(dane_reg)
dane_reg[,3] <- as.numeric(dane_reg[,3])
dane_reg[,4] <- as.numeric(dane_reg[,4])
dane_reg[,13] <- as.numeric(dane_reg[,13])            
dane_reg_X <- colnames(dane_reg)[-13]
dane_reg_Y <- colnames(dane_reg)[13]
print(summary(dane_reg))



  ##  Kroswalidacja wlasnych algorytmow  ##

### KNN ###
parTune_KNN_bin <- expand.grid(k=c(2,4,6,8,10))                                                                                     
KNN_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_KNN_bin, algorytm="KNN", seed = 264)

parTune_KNN_multi <- expand.grid(k=c(2,4,6,8,10))  
KNN_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_KNN_multi, algorytm="KNN", seed = 264)

parTune_KNN_reg <- expand.grid(k=c(2,4,6,8,10))
KNN_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_KNN_reg, algorytm="KNN", seed = 264)


### TREE ###
parTune_Tree_bin <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.15))
Tree_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_Tree_bin, algorytm="Tree", seed = 264)

parTune_Tree_multi <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.15))
Tree_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_Tree_multi, algorytm="Tree", seed = 264)

parTune_Tree_reg <- expand.grid(depth=c(3,5,7), minobs=c(2,4,6), type=c('SS'), overfit = c('none'), cf=c(0.15))
Tree_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_Tree_reg, algorytm="Tree", seed = 264)


### NN ###
parTune_NN_bin <- expand.grid(h=list(c(2,2), c(4,4), c(6,6)), lr = c(0.001), iter = c(20000, 100000))
NN_bin_CV <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 8, parTune_NN_bin, algorytm="NN", seed = 264)

parTune_NN_multi <- expand.grid(h=list(c(2,2), c(4,4), c(6,6)), lr = c(0.001), iter = c(20000, 100000))
NN_multi_CV <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 8, parTune_NN_multi, algorytm="NN", seed = 264)

parTune_NN_reg <- expand.grid(h=list(c(2,2), c(4,4), c(6,6)), lr = c(0.001), iter = c(20000, 100000))
NN_reg_CV <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 8, parTune_NN_reg, algorytm="NN", seed = 264)


  ##  Wybor najlepszego modelu dla funkcji wlasnych  ##

### KNN ###

### TREE ###

### NN ###


  ##  Kroswalidacja wbudowanych algorytmow  ##

cv_R <- trainControl(method="cv", number=8)

### KNN ###
R_KNN_parTune_bin = expand.grid(k=2:20)
R_KNN_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=R_KNN_parTune_bin, method='knn', metric='Accuracy', trControl=cv_R)
R_KNN_bin_R_Result = R_KNN_bin_R$results
print(paste("Najlepszy KNN w R - Binarny: k = ", R_KNN_bin_R$finalModel$k, " | Accuracy = " ,R_KNN_bin_R_Result$Accuracy[R_KNN_bin_R_Result$k == R_KNN_bin_R$finalModel$k]))

R_KNN_parTune_multi = expand.grid(k=2:20)
R_KNN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=R_KNN_parTune_multi, method='knn', metric='Accuracy', trControl=cv_R)
R_KNN_multi_R_Result = R_KNN_multi_R$results
print(paste("Najlepszy KNN w R - Multi: k = ", R_KNN_multi_R$finalModel$k, " | Accuracy = " ,R_KNN_multi_R_Result$Accuracy[R_KNN_multi_R_Result$k == R_KNN_multi_R$finalModel$k]))

R_KNN_parTune_reg = expand.grid(k=2:20)
R_KNN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=R_KNN_parTune_reg, method='knn', metric='MAE', trControl=cv_R)
R_KNN_reg_R_Result = R_KNN_reg_R$results
print(paste("Najlepszy KNN w R - Regresja: k = ", R_KNN_reg_R$finalModel$k, " | MAE = " ,R_KNN_reg_R_Result$MAE[R_KNN_reg_R_Result$k == R_KNN_reg_R$finalModel$k]))



### TREE ###
R_TREE_parTune_bin = expand.grid(maxdepth=2:20)
R_TREE_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=R_TREE_parTune_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
R_TREE_bin_R_Result = R_TREE_bin_R$results
print(paste("Najlepszy Tree w R - Binarny: Max Depth = ", R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_bin_R_Result$Accuracy[R_TREE_bin_R_Result$maxdepth == R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

R_TREE_parTune_multi = expand.grid(maxdepth=2:20)
R_TREE_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=R_TREE_parTune_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
R_TREE_multi_R_Result = R_TREE_multi_R$results
print(paste("Najlepszy Tree w R - Wieloklasowy: Max Depth = ", R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_multi_R_Result$Accuracy[R_TREE_multi_R_Result$maxdepth == R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

R_TREE_parTune_reg = expand.grid(maxdepth=2:20)
R_TREE_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=R_TREE_parTune_reg, method='rpart2', metric='MAE', trControl=cv_R)
R_TREE_reg_R_Result = R_TREE_reg_R$results
print(paste("Najlepszy Tree w R - Regresja: Max Depth = ", R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , R_TREE_reg_R_Result$MAE[R_TREE_reg_R_Result$maxdepth == R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))



### NN ###
dane_bin_norm <- MinMax(dane_bin[,dane_bin_X])
R_NN_parTune_bin = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_bin_R = train(x=dane_bin_norm, y=dane_bin[,dane_bin_Y], tuneGrid=R_NN_parTune_bin, method='nnet', metric='Accuracy', trControl=cv_R)
R_NN_bin_R_Result = R_NN_bin_R$results
# print(paste("Najlepszy NN w R - Binarny: h = ", R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_bin_R_Result$Accuracy[R_NN_bin_R_Result$size == R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))

dane_multi_norm <- MinMax(dane_multi[,dane_multi_X])
R_NN_parTune_multi = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_multi_R = train(x=dane_multi_norm, y=dane_multi[,dane_multi_Y], tuneGrid=R_NN_parTune_multi, method='nnet', metric='Accuracy', trControl=cv_R)
R_NN_multi_R_Result = R_NN_multi_R$results
# print(paste("Najlepszy NN w R - Wieloklasowy: h = ", R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_multi_R_Result$Accuracy[R_NN_multi_R_Result$size == R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))

dane_reg_norm <- MinMax(dane_reg)
R_NN_parTune_reg = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_reg_R = train(x=dane_reg_norm[,dane_reg_X], y=dane_reg_norm[,dane_reg_Y], tuneGrid=R_NN_parTune_reg, method='nnet', metric='MAE', trControl=cv_R)
R_NN_reg_R_Result = R_NN_reg_R$results
# print(paste0("Najlepszy NN w R - Regresja: h = ", R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , R_NN_reg_R_Result$MAE[R_NN_reg_R_Result$size == R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))



  ##  Wybor najlepszego modelu dla funkcji wbudowanych  ##


### KNN ###
print(paste("KNN w R - bin: k = ", R_KNN_bin_R$finalModel$k, " | Accuracy = " ,R_KNN_bin_R_Result$Accuracy[R_KNN_bin_R_Result$k == R_KNN_bin_R$finalModel$k]))
print(paste("KNN w R - multi: k = ", R_KNN_multi_R$finalModel$k, " | Accuracy = " ,R_KNN_multi_R_Result$Accuracy[R_KNN_multi_R_Result$k == R_KNN_multi_R$finalModel$k]))
print(paste("KNN w R - reg: k = ", R_KNN_reg_R$finalModel$k, " | MAE = " ,R_KNN_reg_R_Result$MAE[R_KNN_reg_R_Result$k == R_KNN_reg_R$finalModel$k]))

### TREE ###
print(paste("Tree w R - bin: Max Depth = ", R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_bin_R_Result$Accuracy[R_TREE_bin_R_Result$maxdepth == R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Tree w R - multi: Max Depth = ", R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_multi_R_Result$Accuracy[R_TREE_multi_R_Result$maxdepth == R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Tree w R - reg: Max Depth = ", R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , R_TREE_reg_R_Result$MAE[R_TREE_reg_R_Result$maxdepth == R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

### NN ###
print(paste("NN w R - bin: h = ", R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_bin_R_Result$Accuracy[R_NN_bin_R_Result$size == R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("NN w R - multi: h = ", R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_multi_R_Result$Accuracy[R_NN_multi_R_Result$size == R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("NN w R - reg: h = ", R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , R_NN_reg_R_Result$MAE[R_NN_reg_R_Result$size == R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))


  ##  Porownanie wynikow z wlasnych funckji z wynikami funkcji wbudowanych  ##

  ##  Wplyw hiper-parametrow na jakosc opracowanych modeli  ##



