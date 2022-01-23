rm(list=ls())

library(openxlsx)

source("funkcje.R")


### DANE ###

# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Wholesale+customers         # Dane przez prowadzacego
# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/seeds
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Computer+Hardware


dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
dane_bin <- dane_bin[,-2]
bin_kolumny <- colnames(dane_bin)               # glupi blad - przy zebraniu nazwy "dane_bin[,1]" jest NULL
dane_bin_X <- bin_kolumny[-1]
dane_bin_Y <- bin_kolumny[1]
dane_bin[,1] <- as.factor(dane_bin[,1])
print(head(dane_bin))


dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", header = FALSE, sep = "\t")
dane_multi <- drop_na(dane_multi)
multi_kolumny <- colnames(dane_multi)               
dane_multi_X <- multi_kolumny[-8]
dane_multi_Y <- multi_kolumny[8]
dane_multi[,8] <- as.factor(dane_multi[,8])
print(head(dane_multi))


dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data", header = FALSE)
dane_reg <- dane_reg[,-c(2,10)]
dane_reg[,1] <- as.numeric(dane_reg[,1])
reg_kolumny <- colnames(dane_reg)               
dane_reg_X <- reg_kolumny[-8]
dane_reg_Y <- reg_kolumny[8]
print(head(dane_reg))



### KNN ###

parTune_KNN_bin <- expand.grid(k=c(2:15))
KNN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_KNN_bin, algorytm="KNN", seed = 123)
KNN_bin_CrossValid

KNN_bin_CrossValid_gr <- KNN_bin_CrossValid %>% group_by(k)
KNN_bin_CrossValid_gr <- as.data.frame(KNN_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
KNN_bin_CrossValid_gr
KNN_bin_best_T <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscT),]
KNN_bin_best_W <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscW),]
KNN_bin_best_T
KNN_bin_best_W


parTune_KNN_multi <- expand.grid(k=c(2:15))
KNN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_KNN_multi, algorytm="KNN", seed = 123)
KNN_multi_CrossValid

KNN_multi_CrossValid_gr <- KNN_multi_CrossValid %>% group_by(k)
KNN_multi_CrossValid_gr <- as.data.frame(KNN_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
KNN_multi_CrossValid_gr
KNN_multi_best_T <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCT),]
KNN_multi_best_W <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCW),]
KNN_multi_best_T
KNN_multi_best_W


parTune_KNN_reg <- expand.grid(k=c(2:15))
KNN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_KNN_reg, algorytm="KNN", seed = 123)
KNN_reg_CrossValid

KNN_reg_CrossValid_gr <- KNN_reg_CrossValid %>% group_by(k)
KNN_reg_CrossValid_gr <- as.data.frame(KNN_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
KNN_reg_CrossValid_gr
KNN_reg_best_T <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAET),]
KNN_reg_best_W <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAEW),]
KNN_reg_best_T
KNN_reg_best_W



### Drzewa Decyzyjne ###

parTune_Tree_bin <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.1, 0.25 ))
Tree_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_Tree_bin, algorytm="Tree", seed = 123)
Tree_bin_CrossValid

Tree_bin_CrossValid_gr <- Tree_bin_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_bin_CrossValid_gr <- as.data.frame(Tree_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Tree_bin_CrossValid_gr
Tree_bin_best_T <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscT),]
Tree_bin_best_W <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscW),]
Tree_bin_best_T
Tree_bin_best_W


parTune_Tree_multi <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.1, 0.25 ))
Tree_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_Tree_multi, algorytm="Tree", seed = 123)
Tree_multi_CrossValid

Tree_multi_CrossValid_gr <- Tree_multi_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_multi_CrossValid_gr <- as.data.frame(Tree_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Tree_multi_CrossValid_gr
Tree_multi_best_T <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCT),]
Tree_multi_best_W <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCW),]
Tree_multi_best_T
Tree_multi_best_W


parTune_Tree_reg <- expand.grid(depth=c(3:6), minobs=c(2:5), type=c('SS'), overfit = c('none'), cf=0.2)
Tree_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_Tree_reg, algorytm="Tree", seed = 123)
Tree_reg_CrossValid

Tree_reg_CrossValid_gr <- Tree_reg_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_reg_CrossValid_gr <- as.data.frame(Tree_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Tree_reg_CrossValid_gr
Tree_reg_best_T <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAPET),]
Tree_reg_best_W <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAPEW),]
Tree_reg_best_T
Tree_reg_best_W



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

parTune_NN_bin <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_NN_bin, algorytm="NN", seed = 123)
NN_bin_CrossValid

NN_bin_CrossValid_gr <- NN_bin_CrossValid %>% group_by(h, lr, iter)
NN_bin_CrossValid_gr <- as.data.frame(NN_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
NN_bin_CrossValid_gr
NN_bin_best_T <- NN_bin_CrossValid_gr[which.max(NN_bin_CrossValid_gr$JakoscT),]
NN_bin_best_W <- NN_bin_CrossValid_gr[which.max(NN_bin_CrossValid_gr$JakoscW),]
NN_bin_best_T
NN_bin_best_W


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

parTune_NN_multi <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_NN_multi, algorytm="NN", seed = 123)
NN_multi_CrossValid

NN_multi_CrossValid_gr <- NN_multi_CrossValid %>% group_by(h, lr, iter)
NN_multi_CrossValid_gr <- as.data.frame(NN_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
NN_multi_CrossValid_gr
NN_multi_best_T <- NN_multi_CrossValid_gr[which.max(NN_multi_CrossValid_gr$ACCT),]
NN_multi_best_W <- NN_multi_CrossValid_gr[which.max(NN_multi_CrossValid_gr$ACCW),]
NN_multi_best_T
NN_multi_best_W


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

parTune_NN_reg <- expand.grid(h=list(c(3,4), c(4,4), c(5,5), c(6,6)), lr = c(0.001), iter = c(200000, 100000))
NN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_NN_reg, algorytm="NN", seed = 123)
NN_reg_CrossValid

NN_reg_CrossValid_gr <- NN_reg_CrossValid %>% group_by(h, lr, iter)
NN_reg_CrossValid_gr <- as.data.frame(NN_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
NN_reg_CrossValid_gr
NN_reg_best_T <- NN_reg_CrossValid_gr[which.min(NN_reg_CrossValid_gr$MAPET),]
NN_reg_best_W <- NN_reg_CrossValid_gr[which.min(NN_reg_CrossValid_gr$MAPEW),]
NN_reg_best_T
NN_reg_best_W





#################################### Funkcje z bibliotek R ######################################

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
print(paste("Najlepszy KNN w R - Binarny: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))

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
nn_grid_bin = expand.grid(size=3:10, decay=0.00001)
NN_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results
print(paste("Najlepszy NN w R - Binarny: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))

print("Neural Network - R - multi")
nn_grid_multi = expand.grid(size=3:10, decay=0.00001)
NN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results
print(paste("Najlepszy NN w R - Wieloklasowy: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))

print("Neural Network - R - reg")
nn_grid_reg = expand.grid(size=3:10, decay = 0.00001)
NN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results
print(paste("Najlepszy NN w R - Regresja: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]]))



