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
Wlasne_KNN_bin_Tablica_wynikow <- KNN_bin_CV
Wlasne_KNN_bin_Tablica_wynikow[is.na(Wlasne_KNN_bin_Tablica_wynikow)] <- 0.0
Wlasne_KNN_bin_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_KNN_bin_Tablica_wynikow %>% group_by(k) %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Wlasne_KNN_bin_najlepsze_T <- Wlasne_KNN_bin_Tablica_wynikow_grupowana[which.max(Wlasne_KNN_bin_Tablica_wynikow_grupowana$JakoscT),]
Wlasne_KNN_bin_najlepsze_W <- Wlasne_KNN_bin_Tablica_wynikow_grupowana[which.max(Wlasne_KNN_bin_Tablica_wynikow_grupowana$JakoscW),]
print("Wlasne KNN - bin - tabela wynikow i najlepsze modele: ")
print(Wlasne_KNN_bin_Tablica_wynikow_grupowana)
print(Wlasne_KNN_bin_najlepsze_T)
print(Wlasne_KNN_bin_najlepsze_W)

Wlasne_KNN_multi_Tablica_wynikow <- KNN_multi_CV
Wlasne_KNN_multi_Tablica_wynikow[is.na(Wlasne_KNN_multi_Tablica_wynikow)] <- 0.0
Wlasne_KNN_multi_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_KNN_multi_Tablica_wynikow %>% group_by(k) %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Wlasne_KNN_multi_najlepsze_T <- Wlasne_KNN_multi_Tablica_wynikow_grupowana[which.max(Wlasne_KNN_multi_Tablica_wynikow_grupowana$ACCT),]
Wlasne_KNN_multi_najlepsze_W <- Wlasne_KNN_multi_Tablica_wynikow_grupowana[which.max(Wlasne_KNN_multi_Tablica_wynikow_grupowana$ACCW),]
print("Wlasne KNN - multi - tabela wynikow i najlepsze modele: ")
print(Wlasne_KNN_multi_Tablica_wynikow_grupowana)
print(Wlasne_KNN_multi_najlepsze_T)
print(Wlasne_KNN_multi_najlepsze_W)

Wlasne_KNN_reg_Tablica_wynikow <- KNN_reg_CV
Wlasne_KNN_reg_Tablica_wynikow[is.na(Wlasne_KNN_reg_Tablica_wynikow)] <- 0.0
Wlasne_KNN_reg_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_KNN_reg_Tablica_wynikow %>% group_by(k) %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Wlasne_KNN_reg_najlepsze_T <- Wlasne_KNN_reg_Tablica_wynikow_grupowana[which.min(Wlasne_KNN_reg_Tablica_wynikow_grupowana$MAET),]
Wlasne_KNN_reg_najlepsze_W <- Wlasne_KNN_reg_Tablica_wynikow_grupowana[which.min(Wlasne_KNN_reg_Tablica_wynikow_grupowana$MAEW),]
print("Wlasne KNN - reg - tabela wynikow i najlepsze modele: ")
print(Wlasne_KNN_reg_Tablica_wynikow_grupowana)
print(Wlasne_KNN_reg_najlepsze_T)
print(Wlasne_KNN_reg_najlepsze_W)


### TREE ###
Wlasne_Tree_bin_Tablica_wynikow <- Tree_bin_CV
Wlasne_Tree_bin_Tablica_wynikow[is.na(Wlasne_Tree_bin_Tablica_wynikow)] <- 0.0
Wlasne_Tree_bin_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_Tree_bin_Tablica_wynikow %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Wlasne_Tree_bin_najlepsze_T <- Wlasne_Tree_bin_Tablica_wynikow_grupowana[which.max(Wlasne_Tree_bin_Tablica_wynikow_grupowana$JakoscT),]
Wlasne_Tree_bin_najlepsze_W <- Wlasne_Tree_bin_Tablica_wynikow_grupowana[which.max(Wlasne_Tree_bin_Tablica_wynikow_grupowana$JakoscW),]
print("Wlasne Tree - bin - tabela wynikow i najlepsze modele: ")
print(Wlasne_Tree_bin_Tablica_wynikow_grupowana)
print(Wlasne_Tree_bin_najlepsze_T)
print(Wlasne_Tree_bin_najlepsze_W)

Wlasne_Tree_multi_Tablica_wynikow <- Tree_multi_CV
Wlasne_Tree_multi_Tablica_wynikow[is.na(Wlasne_Tree_multi_Tablica_wynikow)] <- 0.0
Wlasne_Tree_multi_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_Tree_multi_Tablica_wynikow %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Wlasne_Tree_multi_najlepsze_T <- Wlasne_Tree_multi_Tablica_wynikow_grupowana[which.max(Wlasne_Tree_multi_Tablica_wynikow_grupowana$ACCT),]
Wlasne_Tree_multi_najlepsze_W <- Wlasne_Tree_multi_Tablica_wynikow_grupowana[which.max(Wlasne_Tree_multi_Tablica_wynikow_grupowana$ACCW),]
print("Wlasne Tree - multi - tabela wynikow i najlepsze modele: ")
print(Wlasne_Tree_multi_Tablica_wynikow_grupowana)
print(Wlasne_Tree_multi_najlepsze_T)
print(Wlasne_Tree_multi_najlepsze_W)

Wlasne_Tree_reg_Tablica_wynikow <- Tree_reg_CV
Wlasne_Tree_reg_Tablica_wynikow[is.na(Wlasne_Tree_reg_Tablica_wynikow)] <- 0.0
Wlasne_Tree_reg_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_Tree_reg_Tablica_wynikow %>% group_by(depth, minobs, type, overfit, cf) %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Wlasne_Tree_reg_najlepsze_T <- Wlasne_Tree_reg_Tablica_wynikow_grupowana[which.min(Wlasne_Tree_reg_Tablica_wynikow_grupowana$MAET),]
Wlasne_Tree_reg_najlepsze_W <- Wlasne_Tree_reg_Tablica_wynikow_grupowana[which.min(Wlasne_Tree_reg_Tablica_wynikow_grupowana$MAEW),]
print("Wlasne Tree - reg - tabela wynikow i najlepsze modele: ")
print(Wlasne_Tree_reg_Tablica_wynikow_grupowana)
print(Wlasne_Tree_reg_najlepsze_T)
print(Wlasne_Tree_reg_najlepsze_W)


### NN ###
Wlasne_NN_bin_Tablica_wynikow <- NN_bin_CV
Wlasne_NN_bin_Tablica_wynikow[is.na(Wlasne_NN_bin_Tablica_wynikow)] <- 0
Wlasne_NN_bin_Tablica_wynikow$h <- as.character(Wlasne_NN_bin_Tablica_wynikow$h)
Wlasne_NN_bin_Tablica_wynikow$h <- str_remove(Wlasne_NN_bin_Tablica_wynikow$h, pattern = "c")
Wlasne_NN_bin_Tablica_wynikow <- Wlasne_NN_bin_Tablica_wynikow %>% group_by( h, lr, iter)
Wlasne_NN_bin_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_NN_bin_Tablica_wynikow %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Wlasne_NN_bin_Tablica_wynikow_grupowana <- Wlasne_NN_bin_Tablica_wynikow_grupowana %>% arrange(str_length(h), h)
Wlasne_NN_bin_najlepsze_T <- Wlasne_NN_bin_Tablica_wynikow_grupowana[which.max(Wlasne_NN_bin_Tablica_wynikow_grupowana$JakoscT),]
Wlasne_NN_bin_najlepsze_W <- Wlasne_NN_bin_Tablica_wynikow_grupowana[which.max(Wlasne_NN_bin_Tablica_wynikow_grupowana$JakoscW),]
print("Wlasne NN - bin - tabela wynikow i najlepsze modele: ")
print(Wlasne_NN_bin_Tablica_wynikow_grupowana)
print(Wlasne_NN_bin_najlepsze_T)
print(Wlasne_NN_bin_najlepsze_W)

Wlasne_NN_multi_Tablica_wynikow <- NN_multi_CV
Wlasne_NN_multi_Tablica_wynikow[is.na(Wlasne_NN_multi_Tablica_wynikow)] <- 0
Wlasne_NN_multi_Tablica_wynikow$h <- as.character(Wlasne_NN_multi_Tablica_wynikow$h)
Wlasne_NN_multi_Tablica_wynikow$h <- str_remove(Wlasne_NN_multi_Tablica_wynikow$h, pattern = "c")
Wlasne_NN_multi_Tablica_wynikow <- Wlasne_NN_multi_Tablica_wynikow %>% group_by( h, lr, iter)
Wlasne_NN_multi_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_NN_multi_Tablica_wynikow %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Wlasne_NN_multi_Tablica_wynikow_grupowana <- Wlasne_NN_multi_Tablica_wynikow_grupowana %>% arrange(str_length(h), h)
Wlasne_NN_multi_najlepsze_T <- Wlasne_NN_multi_Tablica_wynikow_grupowana[which.max(Wlasne_NN_multi_Tablica_wynikow_grupowana$ACCT),]
Wlasne_NN_multi_najlepsze_W <- Wlasne_NN_multi_Tablica_wynikow_grupowana[which.max(Wlasne_NN_multi_Tablica_wynikow_grupowana$ACCW),]
print("Wlasne NN - multi - tabela wynikow i najlepsze modele: ")
print(Wlasne_NN_multi_Tablica_wynikow_grupowana)
print(Wlasne_NN_multi_najlepsze_T)
print(Wlasne_NN_multi_najlepsze_W)

Wlasne_NN_reg_Tablica_wynikow <- NN_reg_CV
Wlasne_NN_reg_Tablica_wynikow[is.na(Wlasne_NN_reg_Tablica_wynikow)] <- 0
Wlasne_NN_reg_Tablica_wynikow$h <- as.character(Wlasne_NN_reg_Tablica_wynikow$h)
Wlasne_NN_reg_Tablica_wynikow$h <- str_remove(Wlasne_NN_reg_Tablica_wynikow$h, pattern = "c")
Wlasne_NN_reg_Tablica_wynikow <- Wlasne_NN_reg_Tablica_wynikow %>% group_by( h, lr, iter)
Wlasne_NN_reg_Tablica_wynikow_grupowana <- as.data.frame(Wlasne_NN_reg_Tablica_wynikow %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Wlasne_NN_reg_Tablica_wynikow_grupowana <- Wlasne_NN_reg_Tablica_wynikow_grupowana %>% arrange(str_length(h), h)
Wlasne_NN_reg_najlepsze_T <- Wlasne_NN_reg_Tablica_wynikow_grupowana[which.max(Wlasne_NN_reg_Tablica_wynikow_grupowana$MAET),]
Wlasne_NN_reg_najlepsze_W <- Wlasne_NN_reg_Tablica_wynikow_grupowana[which.max(Wlasne_NN_reg_Tablica_wynikow_grupowana$MAEW),]
print("Wlasne NN - reg - tabela wynikow i najlepsze modele: ")
print(Wlasne_NN_reg_Tablica_wynikow_grupowana)
print(Wlasne_NN_reg_najlepsze_T)
print(Wlasne_NN_reg_najlepsze_W)



  ##  Kroswalidacja wbudowanych algorytmow  ##

R_CV_kFold <- trainControl(method="cv", number=8)

### KNN ###
R_KNN_parTune_bin = expand.grid(k=2:20)
R_KNN_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=R_KNN_parTune_bin, method='knn', metric='Accuracy', trControl=R_CV_kFold)
R_KNN_bin_R_Wyniki = R_KNN_bin_R$results

R_KNN_parTune_multi = expand.grid(k=2:20)
R_KNN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=R_KNN_parTune_multi, method='knn', metric='Accuracy', trControl=R_CV_kFold)
R_KNN_multi_R_Wyniki = R_KNN_multi_R$results

R_KNN_parTune_reg = expand.grid(k=2:20)
R_KNN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=R_KNN_parTune_reg, method='knn', metric='MAE', trControl=R_CV_kFold)
R_KNN_reg_R_Wyniki = R_KNN_reg_R$results

### TREE ###
R_TREE_parTune_bin = expand.grid(maxdepth=2:20)
R_TREE_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=R_TREE_parTune_bin, method='rpart2', metric='Accuracy', trControl=R_CV_kFold)
R_TREE_bin_R_Wyniki = R_TREE_bin_R$results

R_TREE_parTune_multi = expand.grid(maxdepth=2:20)
R_TREE_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=R_TREE_parTune_multi, method='rpart2', metric='Accuracy', trControl=R_CV_kFold)
R_TREE_multi_R_Wyniki = R_TREE_multi_R$results

R_TREE_parTune_reg = expand.grid(maxdepth=2:20)
R_TREE_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=R_TREE_parTune_reg, method='rpart2', metric='MAE', trControl=R_CV_kFold)
R_TREE_reg_R_Wyniki = R_TREE_reg_R$results

### NN ###
dane_bin_norm <- MinMax(dane_bin[,dane_bin_X])
R_NN_parTune_bin = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_bin_R = train(x=dane_bin_norm, y=dane_bin[,dane_bin_Y], tuneGrid=R_NN_parTune_bin, method='nnet', metric='Accuracy', trControl=R_CV_kFold)
R_NN_bin_R_Wyniki = R_NN_bin_R$results

dane_multi_norm <- MinMax(dane_multi[,dane_multi_X])
R_NN_parTune_multi = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_multi_R = train(x=dane_multi_norm, y=dane_multi[,dane_multi_Y], tuneGrid=R_NN_parTune_multi, method='nnet', metric='Accuracy', trControl=R_CV_kFold)
R_NN_multi_R_Wyniki = R_NN_multi_R$results

dane_reg_norm <- MinMax(dane_reg)
R_NN_parTune_reg = expand.grid(size=2:15, decay = c(0.0001, 0.01))
R_NN_reg_R = train(x=dane_reg_norm[,dane_reg_X], y=dane_reg_norm[,dane_reg_Y], tuneGrid=R_NN_parTune_reg, method='nnet', metric='MAE', trControl=R_CV_kFold)
R_NN_reg_R_Wyniki = R_NN_reg_R$results


  ##  Wybor najlepszego modelu dla funkcji wbudowanych  ##

### KNN ###
print(paste("KNN w R - bin: k = ", R_KNN_bin_R$finalModel$k, " | Accuracy = " ,R_KNN_bin_R_Wyniki$Accuracy[R_KNN_bin_R_Wyniki$k == R_KNN_bin_R$finalModel$k]))
print(paste("KNN w R - multi: k = ", R_KNN_multi_R$finalModel$k, " | Accuracy = " ,R_KNN_multi_R_Wyniki$Accuracy[R_KNN_multi_R_Wyniki$k == R_KNN_multi_R$finalModel$k]))
print(paste("KNN w R - reg: k = ", R_KNN_reg_R$finalModel$k, " | MAE = " ,R_KNN_reg_R_Wyniki$MAE[R_KNN_reg_R_Wyniki$k == R_KNN_reg_R$finalModel$k]))

### TREE ###
print(paste("Tree w R - bin: Max Depth = ", R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_bin_R_Wyniki$Accuracy[R_TREE_bin_R_Wyniki$maxdepth == R_TREE_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Tree w R - multi: Max Depth = ", R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , R_TREE_multi_R_Wyniki$Accuracy[R_TREE_multi_R_Wyniki$maxdepth == R_TREE_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Tree w R - reg: Max Depth = ", R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , R_TREE_reg_R_Wyniki$MAE[R_TREE_reg_R_Wyniki$maxdepth == R_TREE_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

### NN ###
print(paste("NN w R - bin: h = ", R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_bin_R_Wyniki$Accuracy[R_NN_bin_R_Wyniki$size == R_NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("NN w R - multi: h = ", R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , R_NN_multi_R_Wyniki$Accuracy[R_NN_multi_R_Wyniki$size == R_NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("NN w R - reg: h = ", R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , R_NN_reg_R_Wyniki$MAE[R_NN_reg_R_Wyniki$size == R_NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))


  ##  Porownanie wynikow z wlasnych funckji z wynikami funkcji wbudowanych  ##

VS_KNN_bin <- data.frame(k = c(1:20))
VS_KNN_bin <- merge(VS_KNN_bin, Wlasne_KNN_bin_Tablica_wynikow_grupowana[,c('k', "JakoscW")], by = 'k')
VS_KNN_bin <- merge(VS_KNN_bin, R_KNN_bin_R_Wyniki[,c('k', "Accuracy")], by = 'k')
ggplot(VS_KNN_bin , aes(x=k)) + geom_line(aes(y = JakoscW, color='blue'), size=1, ) + 
                                geom_line(aes(y = Accuracy, color='red'), size=1,) + 
                                labs(title='KNN - bin', x='k', y='Dokladnosc') + 
                                scale_color_discrete(name = "Algorytm", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")


VS_KNN_multi <- data.frame(k = c(1:20))
VS_KNN_multi <- merge(VS_KNN_multi, Wlasne_KNN_multi_Tablica_wynikow_grupowana[,c('k', "ACCW")], by = 'k')
VS_KNN_multi <- merge(VS_KNN_multi, R_KNN_multi_R_Wyniki[,c('k', "Accuracy")], by = 'k')
ggplot(VS_KNN_multi , aes(x=k)) + geom_line(aes(y = ACCW, color='blue'), size=1, ) + 
                                  geom_line(aes(y = Accuracy, color='red'), size=1,) +
                                  labs(title='KNN - multi', x='k', y='Dokladnosc') + 
                                  scale_color_discrete(name = "Algorytm: ", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")

VS_KNN_reg <- data.frame(k = c(1:20))
VS_KNN_reg <- merge(VS_KNN_reg, Wlasne_KNN_reg_Tablica_wynikow_grupowana[,c('k', "MAEW")], by = 'k')
VS_KNN_reg <- merge(VS_KNN_reg, R_KNN_reg_R_Wyniki[,c('k', "MAE")], by = 'k')
ggplot(VS_KNN_reg , aes(x=k)) + geom_line(aes(y = MAEW, color='blue'), size=1, ) + 
                                geom_line(aes(y = MAE, color='red'), size=1,) +
                                labs(title='KNN - reg', x='k', y='MAE') + 
                                scale_color_discrete(name = "Algorytm: ", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")


  ## Statystyka wynikow w zaleznosci od modeli ##

# Tree
ggplot(Wlasne_Tree_bin_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_Tree_bin_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = AUCT, color = 'black'), size=1, ) + 
      geom_line(aes(y = JakoscT, color = 'green'), size=1,) + 
      geom_line(aes(y = AUCW, color = 'red'), size=1, ) + 
      geom_line(aes(y = JakoscW, color = 'blue'), size=1,) + 
      labs(title='Tree - bin', x='Nr modelu', y='AUC / Dokladnosc') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("AUCT", "JakoscW","JakoscT", "AUCW")) + theme(legend.position = "bottom")

ggplot(Wlasne_Tree_multi_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_Tree_multi_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = ACCT, color='blue'), size=1, ) + 
      geom_line(aes(y = ACCW, color='red'), size=1,) + 
      labs(title='Tree - multi', x='Nr modelu', y='Dokladnosc') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("JakoscT", "JakoscW")) + theme(legend.position = "bottom")

ggplot(Wlasne_Tree_reg_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_Tree_reg_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = MAET, color='blue'), size=1, ) + 
      geom_line(aes(y = MAEW, color='red'), size=1,) + 
      labs(title='Tree - reg', x='Nr modelu', y='MAE') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("MAET", "MAEW")) + theme(legend.position = "bottom")


# NN
ggplot(Wlasne_NN_bin_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_NN_bin_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = AUCT, color = 'black'), size=1, ) + 
      geom_line(aes(y = JakoscT, color = 'green'), size=1,) + 
      geom_line(aes(y = AUCW, color = 'red'), size=1, ) + 
      geom_line(aes(y = JakoscW, color = 'blue'), size=1,) + 
      labs(title='NN - bin', x='Nr modelu', y='AUC / Dokladnosc') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("AUCT", "JakoscW","JakoscT", "AUCW")) + theme(legend.position = "bottom")

ggplot(Wlasne_NN_multi_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_NN_multi_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = ACCT, color='blue'), size=1, ) + 
      geom_line(aes(y = ACCW, color='red'), size=1,) + 
      labs(title='NN - multi', x='Nr modelu', y='Dokladnosc') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("JakoscT", "JakoscW")) + theme(legend.position = "bottom")

ggplot(Wlasne_NN_reg_Tablica_wynikow_grupowana , aes(x=c(1:nrow(Wlasne_NN_reg_Tablica_wynikow_grupowana)), size = 1)) + 
      geom_line(aes(y = MAET, color='blue'), size=1, ) + 
      geom_line(aes(y = MAEW, color='red'), size=1,) + 
      labs(title='NN - reg', x='Nr modelu', y='MAE') + 
      scale_color_discrete(name = "Wyniki: ", labels = c("MAET", "MAEW")) + theme(legend.position = "bottom")



