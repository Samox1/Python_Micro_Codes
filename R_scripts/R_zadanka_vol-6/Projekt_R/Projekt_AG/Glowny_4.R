rm(list=ls())

library(openxlsx)

source("funkcje.R")


### DANE ###

# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Wholesale+customers         # Dane wybrane przez prowadzacego
# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/seeds
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Computer+Hardware


dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
dane_bin <- dane_bin[,-2]
bin_kolumny <- colnames(dane_bin)               # glupi blad - przy zebraniu nazwy "dane_bin[,1]" jest NULL
dane_bin_X <- bin_kolumny[-1]
dane_bin_Y <- bin_kolumny[1]
dane_bin[,1] <- as.factor(dane_bin[,1])
print("*** Dane - klasyfikacja binarna ***")
print(head(dane_bin))
print("*********************************")

dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", header = FALSE, sep = "\t")
dane_multi <- drop_na(dane_multi)
multi_kolumny <- colnames(dane_multi)               
dane_multi_X <- multi_kolumny[-8]
dane_multi_Y <- multi_kolumny[8]
dane_multi[,8] <- as.factor(dane_multi[,8])
print("*** Dane - klasyfikacja multi ***")
print(head(dane_multi))
print("*********************************")


dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data", header = FALSE)
dane_reg <- dane_reg[,-c(2,10)]
dane_reg[,1] <- as.numeric(dane_reg[,1])
reg_kolumny <- colnames(dane_reg)               
dane_reg_X <- reg_kolumny[-8]
dane_reg_Y <- reg_kolumny[8]
print("*** Dane - regresja ***")
print(head(dane_reg))
print("*********************************")
cat(" \n")


print("//////////////////////////////////////////////////////////")
print("/////////////////////// OBLICZENIA ///////////////////////")
print("//////////////////////////////////////////////////////////")


### KNN ###

print("*** KNN - bin - kroswalidacja ***")
# parTune_KNN_bin <- expand.grid(k=c(2:15))       <-- SIATKA PARAMETROW DO OBLICZEN, zmniejszona w celu oszczedzenia czasu | To samo sie tyczy kFold = 10 (do obliczen)
parTune_KNN_bin <- expand.grid(k=c(3,6,9))   
KNN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 6, parTune_KNN_bin, algorytm="KNN", seed = 123)
# KNN_bin_CrossValid

KNN_bin_CrossValid_gr <- KNN_bin_CrossValid %>% group_by(k)
KNN_bin_CrossValid_gr <- as.data.frame(KNN_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(KNN_bin_CrossValid_gr)
KNN_bin_best_T <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscT),]
KNN_bin_best_W <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscW),]
print(KNN_bin_best_T)
print(KNN_bin_best_W)


print("*** KNN - multi - kroswalidacja ***")
# parTune_KNN_multi <- expand.grid(k=c(2:15))     <-- SIATKA PARAMETROW DO OBLICZEN, zmniejszona w celu oszczedzenia czasu | To samo sie tyczy kFold = 10 (do obliczen)
parTune_KNN_multi <- expand.grid(k=c(3,6,9))
KNN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 6, parTune_KNN_multi, algorytm="KNN", seed = 123)
# KNN_multi_CrossValid

KNN_multi_CrossValid_gr <- KNN_multi_CrossValid %>% group_by(k)
KNN_multi_CrossValid_gr <- as.data.frame(KNN_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(KNN_multi_CrossValid_gr)
KNN_multi_best_T <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCT),]
KNN_multi_best_W <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCW),]
print(KNN_multi_best_T)
print(KNN_multi_best_W)


print("*** KNN - reg - kroswalidacja ***")
# parTune_KNN_reg <- expand.grid(k=c(2:15))     <-- SIATKA PARAMETROW DO OBLICZEN, zmniejszona w celu oszczedzenia czasu | To samo sie tyczy kFold = 10 (do obliczen)
parTune_KNN_reg <- expand.grid(k=c(3,6,9))
KNN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 6, parTune_KNN_reg, algorytm="KNN", seed = 123)
# KNN_reg_CrossValid

KNN_reg_CrossValid_gr <- KNN_reg_CrossValid %>% group_by(k)
KNN_reg_CrossValid_gr <- as.data.frame(KNN_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
print(KNN_reg_CrossValid_gr)
KNN_reg_best_T <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAET),]
KNN_reg_best_W <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAEW),]
print(KNN_reg_best_T)
print(KNN_reg_best_W)



### Drzewa Decyzyjne ###

print("*** Tree - bin - kroswalidacja ***")
parTune_Tree_bin <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.1, 0.25 ))
Tree_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_Tree_bin, algorytm="Tree", seed = 123)
Tree_bin_CrossValid[is.na(Tree_bin_CrossValid)] <- 0
# Tree_bin_CrossValid

Tree_bin_CrossValid_gr <- Tree_bin_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_bin_CrossValid_gr <- as.data.frame(Tree_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(Tree_bin_CrossValid_gr)
Tree_bin_best_T <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscT),]
Tree_bin_best_W <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscW),]
print(Tree_bin_best_T)
print(Tree_bin_best_W)


print("*** Tree - multi - kroswalidacja ***")
parTune_Tree_multi <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.1, 0.25 ))
Tree_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_Tree_multi, algorytm="Tree", seed = 123)
Tree_multi_CrossValid[is.na(Tree_multi_CrossValid)] <- 0
# Tree_multi_CrossValid

Tree_multi_CrossValid_gr <- Tree_multi_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_multi_CrossValid_gr <- as.data.frame(Tree_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(Tree_multi_CrossValid_gr)
Tree_multi_best_T <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCT),]
Tree_multi_best_W <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCW),]
print(Tree_multi_best_T)
print(Tree_multi_best_W)


print("*** Tree - reg - kroswalidacja ***")
parTune_Tree_reg <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('SS'), overfit = c('none'), cf=0.2)
Tree_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_Tree_reg, algorytm="Tree", seed = 123)
Tree_reg_CrossValid[is.na(Tree_reg_CrossValid)] <- 0
# Tree_reg_CrossValid

Tree_reg_CrossValid_gr <- Tree_reg_CrossValid %>% group_by(depth, minobs)
Tree_reg_CrossValid_gr <- as.data.frame(Tree_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
print(Tree_reg_CrossValid_gr)
Tree_reg_best_T <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAET),]
Tree_reg_best_W <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAEW),]
print(Tree_reg_best_T)
print(Tree_reg_best_W)



### Sieci Neuronowe ###

### NN Bin ###

# dane_bin_NN <- dane_bin
# dane_bin_NN[,dane_bin_X] <- sapply(dane_bin_NN[,dane_bin_X], MinMax_nn)
# dane_bin_NN
# X_bin = as.matrix(dane_bin_NN[,dane_bin_X])
# Y_bin = model.matrix( ~ dane_bin_NN[,dane_bin_Y] - 1, dane_bin_NN )
# NN_model_bin <- trainNN( X_bin, Y_bin, h = c(4,4), lr = 0.001, iter = 400000, seed = 123, typ = 'bin')
# NN_predict_bin <- predNN( X_bin, NN_model_bin, typ = "bin")
# NN_predict_bin
# NN_pred_Klasy_bin <- as.numeric( levels(dane_bin_NN[,dane_bin_Y])[apply( NN_predict_bin, 1, which.max )] )
# NN_pred_Klasy_bin
# print(ModelOcena(dane_bin_NN[,dane_bin_Y], NN_predict_bin[,2]))

print("*** Sieci - bin - kroswalidacja ***")
parTune_NN_bin <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_NN_bin, algorytm="NN", seed = 123)
# NN_bin_CrossValid

NN_bin_CrossValid_wynik <- NN_bin_CrossValid
NN_bin_CrossValid_wynik[is.na(NN_bin_CrossValid_wynik)] <- 0
NN_bin_CrossValid_wynik$h <- as.character(NN_bin_CrossValid_wynik$h)
NN_bin_CrossValid_wynik$h <- str_remove(NN_bin_CrossValid_wynik$h, pattern = "c")

NN_bin_CrossValid_wynik <- NN_bin_CrossValid_wynik %>% group_by( h, lr, iter)
NN_bin_CrossValid_wynik_gr <- as.data.frame(NN_bin_CrossValid_wynik %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(NN_bin_CrossValid_wynik_gr)
NN_bin_best_T <- NN_bin_CrossValid_wynik_gr[which.max(NN_bin_CrossValid_wynik_gr$JakoscT),]
NN_bin_best_W <- NN_bin_CrossValid_wynik_gr[which.max(NN_bin_CrossValid_wynik_gr$JakoscW),]
print(NN_bin_best_T)
print(NN_bin_best_W)



### NN Multi ###

# dane_multi_NN <- dane_multi
# dane_multi_NN[,dane_multi_X] <- sapply(dane_multi_NN[,dane_multi_X], MinMax_nn)
# dane_multi_NN
# X_multi = as.matrix(dane_multi_NN[,dane_multi_X])
# Y_multi = model.matrix( ~ dane_multi_NN[,dane_multi_Y] - 1, dane_multi_NN )
# NN_model_Multi <- trainNN( X_multi, Y_multi, h = c(4,4), lr = 0.001, iter = 400000, seed = 123, typ = 'multi')
# NN_predict_Multi <- predNN( X_multi, NN_model_Multi, typ = "multi")
# NN_predict_Multi
# NN_pred_Klasy_multi <- as.numeric( levels(dane_multi_NN[,dane_multi_Y])[apply( NN_predict_Multi, 1, which.max )] )
# NN_pred_Klasy_multi
# print(ModelOcena(dane_multi_NN[,dane_multi_Y], NN_pred_Klasy_multi))

print("*** Sieci - multi - kroswalidacja ***")
parTune_NN_multi <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_NN_multi, algorytm="NN", seed = 123)
# NN_multi_CrossValid

NN_multi_CrossValid_wynik <- NN_multi_CrossValid
NN_multi_CrossValid_wynik[is.na(NN_multi_CrossValid_wynik)] <- 0
NN_multi_CrossValid_wynik$h <- as.character(NN_multi_CrossValid_wynik$h)
NN_multi_CrossValid_wynik$h <- str_remove(NN_multi_CrossValid_wynik$h, pattern = "c")

NN_multi_CrossValid_wynik <- NN_multi_CrossValid_wynik %>% group_by( h, lr, iter)
NN_multi_CrossValid_wynik_gr <- as.data.frame(NN_multi_CrossValid_wynik %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(NN_multi_CrossValid_wynik_gr)
NN_multi_best_T <- NN_multi_CrossValid_wynik_gr[which.max(NN_multi_CrossValid_wynik_gr$ACCT),]
NN_multi_best_W <- NN_multi_CrossValid_wynik_gr[which.max(NN_multi_CrossValid_wynik_gr$ACCW),]
print(NN_multi_best_T)
print(NN_multi_best_W)



### NN Reg ###

# dane_reg_NN <- dane_reg
# dane_reg_NN <- sapply(dane_reg_NN, MinMax_nn)
# dane_reg_NN
# X_reg = as.matrix(dane_reg_NN[,dane_reg_X])
# Y_reg = as.matrix(dane_reg_NN[,dane_reg_Y])
# NN_model_reg <- trainNN( X_reg, Y_reg, h = c(4,4), lr = 0.001, iter = 400000, seed = 123, typ = 'reg')
# NN_predict_reg <- predNN( X_reg, NN_model_reg, typ = "reg")
# NN_predict_reg
# NN_predict_reg_real <- (MinMaxOdwrot(NN_predict_reg[,1], y_min = min(dane_reg[,dane_reg_Y]), y_max = max(dane_reg[,dane_reg_Y])))
# NN_predict_reg_real
# print(ModelOcena(dane_reg[,dane_reg_Y], NN_predict_reg_real))

print("*** Sieci - reg - kroswalidacja ***")
parTune_NN_reg <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_NN_reg, algorytm="NN", seed = 123)
NN_reg_CrossValid

NN_reg_CrossValid_wynik <- NN_reg_CrossValid
NN_reg_CrossValid_wynik[is.na(NN_reg_CrossValid_wynik)] <- 0
NN_reg_CrossValid_wynik$h <- as.character(NN_reg_CrossValid_wynik$h)
NN_reg_CrossValid_wynik$h <- str_remove(NN_reg_CrossValid_wynik$h, pattern = "c")

NN_reg_CrossValid_wynik <- NN_reg_CrossValid_wynik %>% group_by( h, lr, iter)
NN_reg_CrossValid_wynik_gr <- as.data.frame(NN_reg_CrossValid_wynik %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
print(NN_reg_CrossValid_wynik_gr)
NN_reg_best_T <- NN_reg_CrossValid_wynik_gr[which.max(NN_reg_CrossValid_wynik_gr$MAPET),]
NN_reg_best_W <- NN_reg_CrossValid_wynik_gr[which.max(NN_reg_CrossValid_wynik_gr$MAPEW),]
print(NN_reg_best_T)
print(NN_reg_best_W)




#################################################################################################
#################################### Funkcje z bibliotek R ######################################
#################################################################################################

print("/// --- Obliczenia dla funkcji wbudowanych --- ///")

cv_R <- trainControl(method="cv", number=10)


### KNN ###

print("KNN - R - bin")
knn_grid_bin = expand.grid(k=2:50)
KNN_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results
print(paste("Najlepszy KNN w R - Binarny: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))

print("KNN - R - multi")
knn_grid_multi = expand.grid(k=2:50)
KNN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=knn_grid_multi, method='knn', metric='Accuracy', trControl=cv_R)
KNN_multi_R_Wynik = KNN_multi_R$results
print(paste("Najlepszy KNN w R - Multi: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))

print("KNN - R - reg")
knn_grid_reg = expand.grid(k=2:50)
KNN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=knn_grid_reg, method='knn', metric='MAE', trControl=cv_R)
KNN_reg_R_Wynik = KNN_reg_R$results
print(paste("Najlepszy KNN w R - Regresja: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))


### TREE ###

print("TREE - R - bin")
tree_grid_bin = expand.grid(maxdepth=2:15)
Tree_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=tree_grid_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_bin_R_Wynik = Tree_bin_R$results
print(paste("Najlepszy Tree w R - Binarny: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

print("TREE - R - multi")
tree_grid_multi = expand.grid(maxdepth=2:15)
Tree_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=tree_grid_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_multi_R_Wynik = Tree_multi_R$results
print(paste("Najlepszy Tree w R - Wieloklasowy: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

print("TREE - R - reg")
tree_grid_reg = expand.grid(maxdepth=2:15)
Tree_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=tree_grid_reg, method='rpart2', metric='MAE', trControl=cv_R)
Tree_reg_R_Wynik = Tree_reg_R$results
print(paste("Najlepszy Tree w R - Regresja: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))


### NN ###

print("Neural Network - R - bin")
dane_bin_norm <- MinMax_nn(dane_bin[,dane_bin_X])
nn_grid_bin = expand.grid(size=3:10, decay = c(0.00001, 0.001))
NN_bin_R = train(x=dane_bin_norm, y=dane_bin[,dane_bin_Y], tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results
print(paste("Najlepszy NN w R - Binarny: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))


print("Neural Network - R - multi")
dane_multi_norm <- MinMax_nn(dane_multi[,dane_multi_X])
nn_grid_multi = expand.grid(size=3:10, decay = c(0.00001, 0.001))
NN_multi_R = train(x=dane_multi_norm, y=dane_multi[,dane_multi_Y], tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results
print(paste("Najlepszy NN w R - Wieloklasowy: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))


print("Neural Network - R - reg")
dane_reg_norm <- MinMax_nn(dane_reg)
nn_grid_reg = expand.grid(size=3:10, decay = c(0.00001, 0.001))
NN_reg_R = train(x=dane_reg_norm[,dane_reg_X], y=dane_reg_norm[,dane_reg_Y], tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results
print(paste0("Najlepszy NN w R - Regresja: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))





#### PODSUMOWANIE WYNIKOW I POROWNANIE ####

# print(KNN_bin_best_W)
# print(KNN_multi_best_W)
# print(KNN_reg_best_W)
# 
# print(Tree_bin_best_W)
# print(Tree_multi_best_W)
# print(Tree_reg_best_W)
# 
# print(NN_bin_best_W)
# print(NN_multi_best_W)
# print(NN_reg_best_W)
# 
# R - KNN
# print(paste("Najlepszy KNN w R - Binarny: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))
# print(paste("Najlepszy KNN w R - Multi: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))
# print(paste("Najlepszy KNN w R - Regresja: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))
# 
# R - Tree
# print(paste("Najlepszy Tree w R - Binarny: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
# print(paste("Najlepszy Tree w R - Wieloklasowy: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
# print(paste("Najlepszy Tree w R - Regresja: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
# 
# R - NN
# print(paste("Najlepszy NN w R - Binarny: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))
# print(paste("Najlepszy NN w R - Wieloklasowy: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))
# print(paste("Najlepszy NN w R - Regresja: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))



# Klasyfikacja BINARNA - Porownanie
print("*** Porownanie modeli - Klasyfikacja BINARNA ***")
print("Najlepsze KNN - wlasne: ")
print(KNN_bin_best_W)
print(paste("Najlepszy KNN w R: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))
print("Najlepsze Drzewo Decyzyjne - wlasne: ")
print(Tree_bin_best_W)
print(paste("Najlepszy Tree w R: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print("Najlepsze NN - wlasne: ")
print(NN_bin_best_W)
print(paste("Najlepszy NN w R: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))


# Klasyfikacja MULTI - Porownanie
print("*** Porownanie modeli - Klasyfikacja MULTI ***")
print("Najlepsze KNN - wlasne: ")
print(KNN_multi_best_W)
print(paste("Najlepszy KNN w R: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))
print("Najlepsze Drzewo Decyzyjne - wlasne: ")
print(Tree_multi_best_W)
print(paste("Najlepszy Tree w R: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print("Najlepsze NN - wlasne: ")
print(NN_multi_best_W)
print(paste("Najlepszy NN w R: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))


# Regresja - Porownanie
print("*** Porownanie modeli - Regresja ***")
print("Najlepsze KNN - wlasne: ")
print(KNN_reg_best_W)
print(paste("Najlepszy KNN w R - Regresja: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))
print("Najlepsze Drzewo Decyzyjne - wlasne: ")
print(Tree_reg_best_W)
print(paste("Najlepszy Tree w R - Regresja: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print("Najlepsze NN - wlasne: ")
print(NN_reg_best_W)
print(paste0("Najlepszy NN w R - Regresja: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))




### Wykresy ###

# Porownanie KNN wlasnych z bibliotekami w R

Porownanie_KNN_Bin <- data.frame(k = c(1:20))
Porownanie_KNN_Bin <- merge(Porownanie_KNN_Bin, KNN_bin_CrossValid_gr[,c('k', "JakoscW")], by = 'k')
Porownanie_KNN_Bin <- merge(Porownanie_KNN_Bin, KNN_bin_R_Wynik[,c('k', "Accuracy")], by = 'k')
print(Porownanie_KNN_Bin)

ggplot(Porownanie_KNN_Bin , aes(x=k)) +
  geom_line(aes(y = JakoscW, color='blue'), size=1, ) +
  geom_line(aes(y = Accuracy, color='red'), size=1,) +
  labs(title='KNN - Klas. Binarna: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")


Porownanie_KNN_Multi <- data.frame(k = c(1:20))
Porownanie_KNN_Multi <- merge(Porownanie_KNN_Multi, KNN_multi_CrossValid_gr[,c('k', "ACCW")], by = 'k')
Porownanie_KNN_Multi <- merge(Porownanie_KNN_Multi, KNN_multi_R_Wynik[,c('k', "Accuracy")], by = 'k')
print(Porownanie_KNN_Multi)

ggplot(Porownanie_KNN_Multi , aes(x=k)) +
  geom_line(aes(y = ACCW, color='blue'), size=1, ) +
  geom_line(aes(y = Accuracy, color='red'), size=1,) +
  labs(title='KNN - Klas. Wieloklasowa: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")


Porownanie_KNN_Reg <- data.frame(k = c(1:20))
Porownanie_KNN_Reg <- merge(Porownanie_KNN_Reg, KNN_reg_CrossValid_gr[,c('k', "MAEW")], by = 'k')
Porownanie_KNN_Reg <- merge(Porownanie_KNN_Reg, KNN_reg_R_Wynik[,c('k', "MAE")], by = 'k')
print(Porownanie_KNN_Reg)

ggplot(Porownanie_KNN_Reg , aes(x=k)) +
  geom_line(aes(y = MAEW, color='blue'), size=1, ) +
  geom_line(aes(y = MAE, color='red'), size=1,) +
  labs(title='KNN - Regresja: MAE od k', x='k', y='MAE') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")


### Drzewa Decyzyjne ###
### Zaleznosc wynikow od parametrow drzewa - BIN ###

# Zaleznosc parametru PRUNE ('prune' / 'none') - ucinajacego drzewo
Tree_bin_CrossValid_gr_prune <- Tree_bin_CrossValid %>% group_by(depth, minobs,overfit)
Tree_bin_CrossValid_gr_prune <- as.data.frame(Tree_bin_CrossValid_gr_prune %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(Tree_bin_CrossValid_gr_prune)

bin_prune_yes <- Tree_bin_CrossValid_gr_prune[Tree_bin_CrossValid_gr_prune$overfit == 'prune',c("depth","minobs", "JakoscW")]
bin_prune_no <- Tree_bin_CrossValid_gr_prune[Tree_bin_CrossValid_gr_prune$overfit == 'none',c("depth","minobs", "JakoscW")]
Tree_bin_Porownanie_Prune <- cbind("nr_wariantu" = c(1:nrow(bin_prune_yes)), bin_prune_yes, "JakoscW_none" = bin_prune_no$JakoscW)
Tree_bin_Porownanie_Prune

ggplot(Tree_bin_Porownanie_Prune , aes(x=nr_wariantu)) +
  geom_line(aes(y = JakoscW, color='blue'), size=1, ) +
  geom_line(aes(y = JakoscW_none, color='red'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Binarna - porownanie Prune - prune i none', x='nr wariantu', y='Accuracy') +
  scale_color_discrete(name = "Prune", labels = c("prune", "none")) +
  theme(legend.position = "bottom")


# Zaleznosc parametru TYPE ('Entropy' / 'Gini')
Tree_bin_CrossValid_gr_type <- Tree_bin_CrossValid %>% group_by(depth, minobs, type)
Tree_bin_CrossValid_gr_type <- as.data.frame(Tree_bin_CrossValid_gr_type %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(Tree_bin_CrossValid_gr_type)

bin_type_entropy <- Tree_bin_CrossValid_gr_type[Tree_bin_CrossValid_gr_type$type == 'Entropy',c("depth","minobs", "JakoscW")]
bin_type_gini <- Tree_bin_CrossValid_gr_type[Tree_bin_CrossValid_gr_type$type == 'Gini',c("depth","minobs", "JakoscW")]
Tree_bin_Porownanie_Type <- cbind("nr_wariantu" = c(1:nrow(bin_type_entropy)), bin_type_entropy, "JakoscW_gini" = bin_type_gini$JakoscW)
Tree_bin_Porownanie_Type

ggplot(Tree_bin_Porownanie_Type , aes(x=nr_wariantu)) +
  geom_line(aes(y = JakoscW, color='blue'), size=1, ) +
  geom_line(aes(y = JakoscW_gini, color='red'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Binarna - porownanie Typu - Entropy i Gini', x='nr wariantu', y='Accuracy') +
  scale_color_discrete(name = "Typ", labels = c("Entropy", "Gini")) +
  theme(legend.position = "bottom")


# Usrednione wyniki w zaleznosci od MINOBS dla kazdej glebokosci drzewa
Tree_bin_CrossValid_gr_wykres_minobs <- Tree_bin_CrossValid %>% group_by(depth, minobs)
Tree_bin_CrossValid_gr_wykres_minobs <- as.data.frame(Tree_bin_CrossValid_gr_wykres_minobs %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(Tree_bin_CrossValid_gr_wykres_minobs)

Tree_bin_Porownanie_Minobs <- data.frame(cbind("minobs" = Tree_bin_CrossValid_gr_wykres_minobs$minobs, 
                                    "JakoscW_3" = Tree_bin_CrossValid_gr_wykres_minobs[Tree_bin_CrossValid_gr_wykres_minobs$depth == 3, "JakoscW"],
                                    "JakoscW_4" = Tree_bin_CrossValid_gr_wykres_minobs[Tree_bin_CrossValid_gr_wykres_minobs$depth == 4, "JakoscW"],
                                    "JakoscW_5" = Tree_bin_CrossValid_gr_wykres_minobs[Tree_bin_CrossValid_gr_wykres_minobs$depth == 5, "JakoscW"],
                                    "JakoscW_6" = Tree_bin_CrossValid_gr_wykres_minobs[Tree_bin_CrossValid_gr_wykres_minobs$depth == 6, "JakoscW"]))
Tree_bin_Porownanie_Minobs

ggplot(Tree_bin_Porownanie_Minobs , aes(x=minobs)) +
  geom_line(aes(y = JakoscW_3, color='blue'), size=1, ) +
  geom_line(aes(y = JakoscW_4, color='red'), size=1,) +
  geom_line(aes(y = JakoscW_5, color='green'), size=1,) +
  geom_line(aes(y = JakoscW_6, color='black'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Binarna - porownanie Acc od MinObs dla kazdej glebokosci', x='MinObs', y='Accuracy') +
  scale_color_discrete(name = "Depth", labels = c("D_3", "D_4", "D_5", "D_6")) +
  theme(legend.position = "bottom")




### Zaleznosc wynikow od parametrow drzewa - MULTI ###

# Zaleznosc parametru PRUNE ('prune' / 'none') - ucinajacego drzewo
Tree_multi_CrossValid_gr_prune <- Tree_multi_CrossValid %>% group_by(depth, minobs, overfit)
Tree_multi_CrossValid_gr_prune <- as.data.frame(Tree_multi_CrossValid_gr_prune %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(Tree_multi_CrossValid_gr_prune)

multi_prune_yes <- Tree_multi_CrossValid_gr_prune[Tree_multi_CrossValid_gr_prune$overfit == 'prune',c("depth","minobs", "ACCW")]
multi_prune_no <- Tree_multi_CrossValid_gr_prune[Tree_multi_CrossValid_gr_prune$overfit == 'none',c("depth","minobs", "ACCW")]
Tree_multi_Porownanie_Prune <- cbind("nr_wariantu" = c(1:nrow(multi_prune_yes)), multi_prune_yes, "ACCW_none" = multi_prune_no$ACCW)
Tree_multi_Porownanie_Prune

ggplot(Tree_multi_Porownanie_Prune , aes(x=nr_wariantu)) +
  geom_line(aes(y = ACCW, color='blue'), size=1, ) +
  geom_line(aes(y = ACCW_none, color='red'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Wieloklasowa - porownanie Prune - prune i none', x='nr wariantu', y='Accuracy') +
  scale_color_discrete(name = "Prune", labels = c("prune", "none")) +
  theme(legend.position = "bottom")


# Zaleznosc parametru TYPE ('Entropy' / 'Gini')
Tree_multi_CrossValid_gr_type <- Tree_multi_CrossValid %>% group_by(depth, minobs, type)
Tree_multi_CrossValid_gr_type <- as.data.frame(Tree_multi_CrossValid_gr_type %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(Tree_multi_CrossValid_gr_type)

multi_type_entropy <- Tree_multi_CrossValid_gr_type[Tree_multi_CrossValid_gr_type$type == 'Entropy',c("depth","minobs", "ACCW")]
multi_type_gini <- Tree_multi_CrossValid_gr_type[Tree_multi_CrossValid_gr_type$type == 'Gini',c("depth","minobs", "ACCW")]
Tree_multi_Porownanie_Prune <- cbind("nr_wariantu" = c(1:nrow(multi_type_entropy)), multi_type_entropy, "ACCW_gini" = multi_type_gini$ACCW)
Tree_multi_Porownanie_Prune

ggplot(Tree_multi_Porownanie_Prune , aes(x=nr_wariantu)) +
  geom_line(aes(y = ACCW, color='blue'), size=1, ) +
  geom_line(aes(y = ACCW_gini, color='red'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Wieloklasowa - porownanie Typu - Entropy i Gini', x='nr wariantu', y='Accuracy') +
  scale_color_discrete(name = "Typ", labels = c("Entropy", "Gini")) +
  theme(legend.position = "bottom")


# Usrednione wyniki w zaleznosci od MINOBS dla kazdej glebokosci drzewa
Tree_multi_CrossValid_gr_wykres_minobs <- Tree_multi_CrossValid %>% group_by(depth, minobs)
Tree_multi_CrossValid_gr_wykres_minobs <- as.data.frame(Tree_multi_CrossValid_gr_wykres_minobs %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(Tree_multi_CrossValid_gr_wykres_minobs)

Tree_multi_Porownanie_Minobs <- data.frame(cmultid("minobs" = Tree_multi_CrossValid_gr_wykres_minobs$minobs, 
                                               "ACCW_3" = Tree_multi_CrossValid_gr_wykres_minobs[Tree_multi_CrossValid_gr_wykres_minobs$depth == 3, "ACCW"],
                                               "ACCW_4" = Tree_multi_CrossValid_gr_wykres_minobs[Tree_multi_CrossValid_gr_wykres_minobs$depth == 4, "ACCW"],
                                               "ACCW_5" = Tree_multi_CrossValid_gr_wykres_minobs[Tree_multi_CrossValid_gr_wykres_minobs$depth == 5, "ACCW"],
                                               "ACCW_6" = Tree_multi_CrossValid_gr_wykres_minobs[Tree_multi_CrossValid_gr_wykres_minobs$depth == 6, "ACCW"]))
Tree_multi_Porownanie_Minobs

ggplot(Tree_multi_Porownanie_Minobs , aes(x=minobs)) +
  geom_line(aes(y = ACCW_3, color='blue'), size=1, ) +
  geom_line(aes(y = ACCW_4, color='red'), size=1,) +
  geom_line(aes(y = ACCW_5, color='green'), size=1,) +
  geom_line(aes(y = ACCW_6, color='black'), size=1,) +
  labs(title='Drzwa Decyzyjne - Klas. Wieloklasowa - porownanie MAE od MinObs dla kazdej glebokosci', x='MinObs', y='Accuracy') +
  scale_color_discrete(name = "Depth", labels = c("D_3", "D_4", "D_5", "D_6")) +
  theme(legend.position = "bottom")



### Zaleznosc wynikow od parametrow drzewa - REG ###

# Usrednione wyniki w zaleznosci od MINOBS dla kazdej glebokosci drzewa
Tree_reg_CrossValid_gr_wykres_minobs <- Tree_reg_CrossValid %>% group_by(depth, minobs)
Tree_reg_CrossValid_gr_wykres_minobs <- as.data.frame(Tree_reg_CrossValid_gr_wykres_minobs %>% summarise(MAET = mean(MAET), MAEW = mean(MAEW)))
print(Tree_reg_CrossValid_gr_wykres_minobs)

Tree_reg_Porownanie_Minobs <- data.frame(cbind("minobs" = Tree_reg_CrossValid_gr_wykres_minobs$minobs, 
                                                   "MAEW_3" = Tree_reg_CrossValid_gr_wykres_minobs[Tree_reg_CrossValid_gr_wykres_minobs$depth == 3, "MAEW"],
                                                   "MAEW_4" = Tree_reg_CrossValid_gr_wykres_minobs[Tree_reg_CrossValid_gr_wykres_minobs$depth == 4, "MAEW"],
                                                   "MAEW_5" = Tree_reg_CrossValid_gr_wykres_minobs[Tree_reg_CrossValid_gr_wykres_minobs$depth == 5, "MAEW"],
                                                   "MAEW_6" = Tree_reg_CrossValid_gr_wykres_minobs[Tree_reg_CrossValid_gr_wykres_minobs$depth == 6, "MAEW"]))
Tree_reg_Porownanie_Minobs

ggplot(Tree_reg_Porownanie_Minobs , aes(x=minobs)) +
  geom_line(aes(y = MAEW_3, color='blue'), size=1, ) +
  geom_line(aes(y = MAEW_4, color='red'), size=1,) +
  geom_line(aes(y = MAEW_5, color='green'), size=1,) +
  geom_line(aes(y = MAEW_6, color='black'), size=1,) +
  labs(title='Drzwa Decyzyjne - Regresja - porownanie MinObs dla kazdej glebokosci', x='MinObs', y='MAE') +
  scale_color_discrete(name = "Depth", labels = c("D_3", "D_4", "D_5", "D_6")) +
  theme(legend.position = "bottom")




### Sieci Neuronowe ###

### Zaleznosc wynikow od parametrow Sieci Neuronowej - BIN ###

# print(NN_bin_CrossValid_wynik)

# Zaleznosc parametru ITER i kombinacji neuronow w warstwach
NN_bin_CrossValid_gr_iter <- NN_bin_CrossValid_wynik %>% group_by(h, iter)
NN_bin_CrossValid_gr_iter <- as.data.frame(NN_bin_CrossValid_gr_iter %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(NN_bin_CrossValid_gr_iter)

bin_iter_1 <- NN_bin_CrossValid_gr_iter[NN_bin_CrossValid_gr_iter$iter == unique(NN_bin_CrossValid_gr_iter$iter)[1], c("h", "JakoscW")]
bin_iter_2 <- NN_bin_CrossValid_gr_iter[NN_bin_CrossValid_gr_iter$iter == unique(NN_bin_CrossValid_gr_iter$iter)[2], c("h", "JakoscW")]
NN_bin_Porownanie_iter <- cbind("nr_wariantu" = c(1:nrow(bin_iter_1)), bin_iter_1, "JakoscW_2" = bin_iter_2$JakoscW)
NN_bin_Porownanie_iter

ggplot(NN_bin_Porownanie_iter , aes(x=nr_wariantu)) +
  geom_line(aes(y = JakoscW, color='blue'), size=1, ) +
  geom_line(aes(y = JakoscW_2, color='red'), size=1,) +
  labs(title='Sieci Neuronowe - Klas. Binarna - porownanie wynikow dla roznych ITER i modeli', x='Wariant warstw', y='Accuracy') +
  scale_color_discrete(name = "Iter", labels = c(as.character(unique(NN_bin_CrossValid_gr_iter$iter)[1]), as.character(unique(NN_bin_CrossValid_gr_iter$iter)[2]))) +
  theme(legend.position = "bottom")



### Zaleznosc wynikow od parametrow Sieci Neuronowej - MULTI ###

# print(NN_multi_CrossValid_wynik)

# Zaleznosc parametru ITER i kombinacji neuronow w warstwach
NN_multi_CrossValid_gr_iter <- NN_multi_CrossValid_wynik %>% group_by(h, iter)
NN_multi_CrossValid_gr_iter <- as.data.frame(NN_multi_CrossValid_gr_iter %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
print(NN_multi_CrossValid_gr_iter)

multi_iter_1 <- NN_multi_CrossValid_gr_iter[NN_multi_CrossValid_gr_iter$iter == unique(NN_multi_CrossValid_gr_iter$iter)[1], c("h", "ACCW")]
multi_iter_2 <- NN_multi_CrossValid_gr_iter[NN_multi_CrossValid_gr_iter$iter == unique(NN_multi_CrossValid_gr_iter$iter)[2], c("h", "ACCW")]
NN_multi_Porownanie_iter <- cbind("nr_wariantu" = c(1:nrow(multi_iter_1)), multi_iter_1, "ACCW_2" = multi_iter_2$ACCW)
NN_multi_Porownanie_iter

ggplot(NN_multi_Porownanie_iter , aes(x=nr_wariantu)) +
  geom_line(aes(y = ACCW, color='blue'), size=1, ) +
  geom_line(aes(y = ACCW_2, color='red'), size=1,) +
  labs(title='Sieci Neuronowe - Klas. Wieloklasowa - porownanie wynikow dla roznych ITER i modeli', x='Wariant warstw', y='Accuracy') +
  scale_color_discrete(name = "Iter", labels = c(as.character(unique(NN_multi_CrossValid_gr_iter$iter)[1]), as.character(unique(NN_multi_CrossValid_gr_iter$iter)[2]))) +
  theme(legend.position = "bottom")



### Zaleznosc wynikow od parametrow Sieci Neuronowej - REG ###

# print(NN_reg_CrossValid_wynik)

# Zaleznosc parametru ITER i kombinacji neuronow w warstwach
NN_reg_CrossValid_gr_iter <- NN_reg_CrossValid_wynik %>% group_by(h, iter)
NN_reg_CrossValid_gr_iter <- as.data.frame(NN_reg_CrossValid_gr_iter %>% summarise(MAET = mean(MAET), MAEW = mean(MAEW)))
print(NN_reg_CrossValid_gr_iter)

reg_iter_1 <- NN_reg_CrossValid_gr_iter[NN_reg_CrossValid_gr_iter$iter == unique(NN_reg_CrossValid_gr_iter$iter)[1], c("h", "MAEW")]
reg_iter_2 <- NN_reg_CrossValid_gr_iter[NN_reg_CrossValid_gr_iter$iter == unique(NN_reg_CrossValid_gr_iter$iter)[2], c("h", "MAEW")]
NN_reg_Porownanie_iter <- cbind("nr_wariantu" = c(1:nrow(reg_iter_1)), reg_iter_1, "MAEW_2" = reg_iter_2$MAEW)
NN_reg_Porownanie_iter

ggplot(NN_reg_Porownanie_iter , aes(x=nr_wariantu)) +
  geom_line(aes(y = MAEW, color='blue'), size=1, ) +
  geom_line(aes(y = MAEW_2, color='red'), size=1,) +
  labs(title='Sieci Neuronowe - Regresja - porownanie wynikow dla roznych ITER i modeli', x='Wariant warstw', y='MAE') +
  scale_color_discrete(name = "Iter", labels = c(as.character(unique(NN_reg_CrossValid_gr_iter$iter)[1]), as.character(unique(NN_reg_CrossValid_gr_iter$iter)[2]))) +
  theme(legend.position = "bottom")



