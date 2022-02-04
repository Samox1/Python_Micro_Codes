source("funkcje.R")

# Wczytanie danych

# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Wholesale+customers

dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
dane_bin <- dane_bin[,-2]
bin_kolumny <- colnames(dane_bin) 
dane_bin_X <- bin_kolumny[-1]
dane_bin_Y <- bin_kolumny[1]
dane_bin[,1] <- as.factor(dane_bin[,1])
print(summary(dane_bin))



# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/Balance+Scale                # Dane wybrane przez prowadzacego

dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/balance-scale/balance-scale.data", header = FALSE)
multi_kolumny <- colnames(dane_multi)               
dane_multi_X <- multi_kolumny[-1]
dane_multi_Y <- multi_kolumny[1]
dane_multi[,1] <- as.factor(dane_multi[,1])
print(summary(dane_multi))


# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Computer+Hardware

dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data", header = FALSE)
dane_reg <- dane_reg[,-c(2,10)]
dane_reg[,1] <- as.numeric(dane_reg[,1])
reg_kolumny <- colnames(dane_reg)               
dane_reg_X <- reg_kolumny[-8]
dane_reg_Y <- reg_kolumny[8]
print(summary(dane_reg))



print("### Obliczenia - implementacje wlasne ###")


# KNN 

print("KNN - bin")
parTune_KNN_bin <- expand.grid(k=c(2:20))
KNN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_KNN_bin, algorytm="KNN", seed = 631)
KNN_bin_CrossValid_gr <- KNN_bin_CrossValid %>% group_by(k)
KNN_bin_CrossValid_gr[is.na(KNN_bin_CrossValid_gr)] <- 0
KNN_bin_CrossValid_gr <- as.data.frame(KNN_bin_CrossValid_gr %>% summarise(AUCW = mean(AUCW), SensitivityW = mean(SensitivityW), SpecificityW = mean(SpecificityW), AccuracyW = mean(AccuracyW)))
print(KNN_bin_CrossValid_gr)
KNN_bin_best_W <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$AccuracyW),]
print(KNN_bin_best_W)


print("KNN - multi")
parTune_KNN_multi <- expand.grid(k=c(2:20))
KNN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_KNN_multi, algorytm="KNN", seed = 631)
KNN_multi_CrossValid_gr <- KNN_multi_CrossValid %>% group_by(k)
KNN_multi_CrossValid_gr[is.na(KNN_multi_CrossValid_gr)] <- 0
KNN_multi_CrossValid_gr <- as.data.frame(KNN_multi_CrossValid_gr %>% summarise(ACCW = mean(ACCW)))
print(KNN_multi_CrossValid_gr)
KNN_multi_best_W <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCW),]
print(KNN_multi_best_W)


print("KNN - reg")
parTune_KNN_reg <- expand.grid(k=c(2:20))
KNN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_KNN_reg, algorytm="KNN", seed = 631)
KNN_reg_CrossValid_gr <- KNN_reg_CrossValid %>% group_by(k)
KNN_reg_CrossValid_gr[is.na(KNN_reg_CrossValid_gr)] <- 0
KNN_reg_CrossValid_gr <- as.data.frame(KNN_reg_CrossValid_gr %>% summarise(MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
print(KNN_reg_CrossValid_gr)
KNN_reg_best_W <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAEW),]
print(KNN_reg_best_W)





# Drzewa Decyzyjne

print("Tree - bin")
parTune_Tree_bin <- expand.grid(depth=c(3,5,7), minobs=c(2,4,7), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.08, 0.2))
Tree_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_Tree_bin, algorytm="Tree", seed = 631)
Tree_bin_CrossValid[is.na(Tree_bin_CrossValid)] <- 0
Tree_bin_CrossValid_gr <- Tree_bin_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_bin_CrossValid_gr <- as.data.frame(Tree_bin_CrossValid_gr %>% summarise(AUCW = mean(AUCW), SensitivityW = mean(SensitivityW), SpecificityW = mean(SpecificityW), AccuracyW = mean(AccuracyW), ))
print(Tree_bin_CrossValid_gr)
Tree_bin_best_W <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$AccuracyW),]
print(Tree_bin_best_W)


print("Tree - multi")
parTune_Tree_multi <- expand.grid(depth=c(3,5,7), minobs=c(2,4,7), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.08,0.2))
Tree_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_Tree_multi, algorytm="Tree", seed = 631)
Tree_multi_CrossValid[is.na(Tree_multi_CrossValid)] <- 0
Tree_multi_CrossValid_gr <- Tree_multi_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_multi_CrossValid_gr <- as.data.frame(Tree_multi_CrossValid_gr %>% summarise(ACCW = mean(ACCW)))
print(Tree_multi_CrossValid_gr)
Tree_multi_best_W <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCW),]
print(Tree_multi_best_W)


print("Tree - reg")
parTune_Tree_reg <- expand.grid(depth=c(3,5,7), minobs=c(2,4,7), type=c('SS'), overfit = c('none'), cf=0.2)
Tree_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_Tree_reg, algorytm="Tree", seed = 631)
Tree_reg_CrossValid[is.na(Tree_reg_CrossValid)] <- 0
Tree_reg_CrossValid_gr <- Tree_reg_CrossValid %>% group_by(depth, minobs)
Tree_reg_CrossValid_gr <- as.data.frame(Tree_reg_CrossValid_gr %>% summarise(MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
print(Tree_reg_CrossValid_gr)
Tree_reg_best_W <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAEW),]
print(Tree_reg_best_W)





# Sieci Neuronowe

print("Sieci - bin")
parTune_NN_bin <- expand.grid(h=list(c(4), c(8), c(4,6), c(5,10), c(6,6,6), c(4,6,8,10)), lr = c(0.01), iter = c(20000, 100000))
NN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 10, parTune_NN_bin, algorytm="NN", seed = 631)
NN_bin_CrossValid_wynik <- NN_bin_CrossValid
NN_bin_CrossValid_wynik[is.na(NN_bin_CrossValid_wynik)] <- 0
NN_bin_CrossValid_wynik$h <- as.character(NN_bin_CrossValid_wynik$h)
NN_bin_CrossValid_wynik$h <- str_remove(NN_bin_CrossValid_wynik$h, pattern = "c")
NN_bin_CrossValid_wynik <- NN_bin_CrossValid_wynik %>% group_by( h, lr, iter)
NN_bin_CrossValid_wynik_gr <- as.data.frame(NN_bin_CrossValid_wynik %>% summarise(AUCW = mean(AUCW), SensitivityW = mean(SensitivityW), SpecificityW = mean(SpecificityW), AccuracyW = mean(AccuracyW)))
NN_bin_CrossValid_wynik_gr <- NN_bin_CrossValid_wynik_gr %>% arrange(str_length(h), h)
print(NN_bin_CrossValid_wynik_gr)
NN_bin_best_W <- NN_bin_CrossValid_wynik_gr[which.max(NN_bin_CrossValid_wynik_gr$AccuracyW),]
print(NN_bin_best_W)



print("Sieci - multi")
parTune_NN_multi <- expand.grid(h=list(c(4), c(8), c(4,6), c(5,10), c(6,6,6), c(4,6,8,10)), lr = c(0.01), iter = c(20000, 100000))
NN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_NN_multi, algorytm="NN", seed = 631)
NN_multi_CrossValid_wynik <- NN_multi_CrossValid
NN_multi_CrossValid_wynik[is.na(NN_multi_CrossValid_wynik)] <- 0
NN_multi_CrossValid_wynik$h <- as.character(NN_multi_CrossValid_wynik$h)
NN_multi_CrossValid_wynik$h <- str_remove(NN_multi_CrossValid_wynik$h, pattern = "c")
NN_multi_CrossValid_wynik <- NN_multi_CrossValid_wynik %>% group_by( h, lr, iter)
NN_multi_CrossValid_wynik_gr <- as.data.frame(NN_multi_CrossValid_wynik %>% summarise(ACCW = mean(ACCW)))
NN_multi_CrossValid_wynik_gr <- NN_multi_CrossValid_wynik_gr %>% arrange(str_length(h), h)
print(NN_multi_CrossValid_wynik_gr)
NN_multi_best_W <- NN_multi_CrossValid_wynik_gr[which.max(NN_multi_CrossValid_wynik_gr$ACCW),]
print(NN_multi_best_W)



print("Sieci - reg")
parTune_NN_reg <- expand.grid(h=list(c(8), c(4,6), c(6,6,6), c(4,6,8,10)), lr = c(0.01), iter = c(20000, 100000))
NN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_NN_reg, algorytm="NN", seed = 631)
NN_reg_CrossValid_wynik <- NN_reg_CrossValid
NN_reg_CrossValid_wynik[is.na(NN_reg_CrossValid_wynik)] <- 0
NN_reg_CrossValid_wynik$h <- as.character(NN_reg_CrossValid_wynik$h)
NN_reg_CrossValid_wynik$h <- str_remove(NN_reg_CrossValid_wynik$h, pattern = "c")
NN_reg_CrossValid_wynik <- NN_reg_CrossValid_wynik %>% group_by( h, lr, iter)
NN_reg_CrossValid_wynik_gr <- as.data.frame(NN_reg_CrossValid_wynik %>% summarise(MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
NN_reg_CrossValid_wynik_gr <- NN_reg_CrossValid_wynik_gr %>% arrange(str_length(h), h)
print(NN_reg_CrossValid_wynik_gr %>% arrange(str_length(h), h))
NN_reg_best_W <- NN_reg_CrossValid_wynik_gr[which.min(NN_reg_CrossValid_wynik_gr$MAPEW),]
print(NN_reg_best_W)






print("### Obliczenia - R ###")

cv_R <- trainControl(method="cv", number=10)


# KNN

knn_grid_bin = expand.grid(k=2:20)
KNN_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results
print(paste("KNN w R - bin: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))

knn_grid_multi = expand.grid(k=2:20)
KNN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=knn_grid_multi, method='knn', metric='Accuracy', trControl=cv_R)
KNN_multi_R_Wynik = KNN_multi_R$results
print(paste("KNN w R - multi: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))

knn_grid_reg = expand.grid(k=2:20)
KNN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=knn_grid_reg, method='knn', metric='MAE', trControl=cv_R)
KNN_reg_R_Wynik = KNN_reg_R$results
print(paste("KNN w R - reg: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))


# Drzewa decyzyjne

tree_grid_bin = expand.grid(maxdepth=2:8)
Tree_bin_R = train(x=dane_bin[,dane_bin_X], y=dane_bin[,dane_bin_Y], tuneGrid=tree_grid_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_bin_R_Wynik = Tree_bin_R$results
print(paste("Drzewo w R - bin: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

tree_grid_multi = expand.grid(maxdepth=2:8)
Tree_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=tree_grid_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_multi_R_Wynik = Tree_multi_R$results
print(paste("Drzewo w R - multi: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

tree_grid_reg = expand.grid(maxdepth=2:8)
Tree_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=tree_grid_reg, method='rpart2', metric='MAE', trControl=cv_R)
Tree_reg_R_Wynik = Tree_reg_R$results
print(paste("Drzewo w R - reg: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))


# Sieci neuronowe

dane_bin_norm <- MinMax_NN(dane_bin[,dane_bin_X])
nn_grid_bin = expand.grid(size=2:8, decay = c(0.0001, 0.01))
NN_bin_R = train(x=dane_bin_norm, y=dane_bin[,dane_bin_Y], tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results
print(paste("Siec NN w R - bin: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))

dane_multi_norm <- MinMax_NN(dane_multi[,dane_multi_X])
nn_grid_multi = expand.grid(size=2:8, decay = c(0.0001, 0.01))
NN_multi_R = train(x=dane_multi_norm, y=dane_multi[,dane_multi_Y], tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results
print(paste("Siec NN w R - multi: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))

dane_reg_norm <- MinMax_NN(dane_reg)
nn_grid_reg = expand.grid(size=2:8, decay = c(0.0001, 0.01))
NN_reg_R = train(x=dane_reg_norm[,dane_reg_X], y=dane_reg_norm[,dane_reg_Y], tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results
print(paste0("Siec NN w R - reg: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))






# Porownanie KNN wlasnych z bibliotekami w R

VS_KNN_Bin <- data.frame(k = c(1:20))
VS_KNN_Bin <- merge(VS_KNN_Bin, KNN_bin_CrossValid_gr[,c('k', "AccuracyW")], by = 'k')
VS_KNN_Bin <- merge(VS_KNN_Bin, KNN_bin_R_Wynik[,c('k', "Accuracy")], by = 'k')
# print(VS_KNN_Bin)

ggplot(VS_KNN_Bin , aes(x=k)) + geom_line(aes(y = AccuracyW, color='blue'), size=1, ) + geom_line(aes(y = Accuracy, color='red'), size=1,) + 
  labs(title='KNN - bin', x='k', y='Accuracy') + scale_color_discrete(name = "Algorytm", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")


VS_KNN_Multi <- data.frame(k = c(1:20))
VS_KNN_Multi <- merge(VS_KNN_Multi, KNN_multi_CrossValid_gr[,c('k', "ACCW")], by = 'k')
VS_KNN_Multi <- merge(VS_KNN_Multi, KNN_multi_R_Wynik[,c('k', "Accuracy")], by = 'k')
# print(VS_KNN_Multi)

ggplot(VS_KNN_Multi , aes(x=k)) + geom_line(aes(y = ACCW, color='blue'), size=1, ) + geom_line(aes(y = Accuracy, color='red'), size=1,) +
  labs(title='KNN - multi', x='k', y='Accuracy') + scale_color_discrete(name = "Algorytm", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")


VS_KNN_Reg <- data.frame(k = c(1:20))
VS_KNN_Reg <- merge(VS_KNN_Reg, KNN_reg_CrossValid_gr[,c('k', "MAEW")], by = 'k')
VS_KNN_Reg <- merge(VS_KNN_Reg, KNN_reg_R_Wynik[,c('k', "MAE")], by = 'k')
# print(VS_KNN_Reg)

ggplot(VS_KNN_Reg , aes(x=k)) + geom_line(aes(y = MAEW, color='blue'), size=1, ) + geom_line(aes(y = MAE, color='red'), size=1,) +
  labs(title='KNN - reg', x='k', y='MAE') + scale_color_discrete(name = "Algorytm", labels = c("Wlasny", "Biblioteka R")) + theme(legend.position = "bottom")





