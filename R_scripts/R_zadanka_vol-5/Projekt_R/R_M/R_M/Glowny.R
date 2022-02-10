source("funkcje.R")


print("******************************************************")
print("*** Wczytywanie danych do regresji - Computer Hardware")                                         # https://archive.ics.uci.edu/ml/datasets/Computer+Hardware
regresja <- read.csv("machine.data", header = FALSE)[,-10]                                              # Ostatnia kolumna wedlug notki to estymacja wydajnosci (kolumny nr 9) z jakiegos artykulu
regresja[,1] <- as.factor(regresja[,1])
regresja <- regresja[,-2]
regresja_Y <- colnames(regresja)[ncol(regresja)]
regresja_X <- colnames(regresja)[-ncol(regresja)]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", anyNA(regresja))) 
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(regresja))


print("******************************************************")
print("*** Wczytywanie danych do klasyfikacji binarnej - Breast Cancer Wisconsin (Diagnostic)")         # http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
binarna <- read.csv("breast-cancer-wisconsin.data", header = FALSE) 
binarna[,11] <- as.factor(binarna[,11])                                                                 # Klasy zapisane jako wartosci numeryczne -> "2 for benign, 4 for malignant" (z notki)
binarna <- subset(binarna, V7 != '?')
binarna[,7] <- as.numeric(binarna[,7])
binarna_Y <- colnames(binarna)[11]
binarna_X <- colnames(binarna)[-11]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", anyNA(binarna)))   
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(binarna))


print("******************************************************")
print("** Wczytywanie danych do klasyfikacji wieloklasowej - Yeast")                                    # Dane od prowadzacego -> https://archive.ics.uci.edu/ml/datasets/Yeast
wieloklasowa <- read.csv("yeast.data", header = FALSE, sep = ";")[,-1]                                  # Dane wedlug notki maja 8 kolumn numerycznych i 1 z klasami docelowymi -> kolumna nr 1 to nic nie mowiaca nazwa
wieloklasowa[,9] <- as.factor(wieloklasowa[,9])
wieloklasowa_Y <- colnames(wieloklasowa)[9]
wieloklasowa_X <- colnames(wieloklasowa)[-9]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", anyNA(wieloklasowa)))        # Dane mialy specyficzny separator, ktory zostal zamieniony na srednik + upewniono sie ze nie ma zadnych problemow z separatorem
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(wieloklasowa))
print("Rozklad klas w danych: ")
print(sort(summary(wieloklasowa[,9])))




# Sieci Neuronowe

parTune_NN_reg <- expand.grid(h = list(c(8,8), c(8,8,8)), lr = c(0.0001, 0.001, 0.01), iter = c(20000, 80000))
CV_NN_reg <- CrossValidTune(regresja_Y, regresja_X, regresja, algo = 'NN', kFold = 10, parTune = parTune_NN_reg, seed = 333)

parTune_NN_bin <- expand.grid(h = list(c(8,8), c(8,8,8)), lr = c(0.0001, 0.001, 0.01), iter = c(20000, 80000))
CV_NN_bin <- CrossValidTune(binarna_Y, binarna_X, binarna, algo = 'NN', kFold = 10, parTune = parTune_NN_bin, seed = 333)

parTune_NN_multi <- expand.grid(h = list(c(8,8), c(8,8,8)), lr = c(0.01, 0.1, 1.0), iter = c(20000, 80000))
CV_NN_multi <- CrossValidTune(wieloklasowa_Y, wieloklasowa_X, wieloklasowa, algo = 'NN', kFold = 10, parTune = parTune_NN_multi, seed = 333)


# Drzewa Decyzyjne

parTune_Tree_reg <- expand.grid(type = c('SS'), depth = c(3,6,9), minobs = c(2,5,10), overfit = c('none'), cf = c(0.02))
CV_Tree_reg <- CrossValidTune(regresja_Y, regresja_X, regresja, algo = 'Tree', kFold = 10, parTune = parTune_Tree_reg, seed = 333)

parTune_Tree_bin <- expand.grid(type = c('Entropy', 'Gini'), depth = c(3,6,9), minobs = c(2,5,10), overfit = c('prune', 'none'), cf = c(0.02))
CV_Tree_bin <- CrossValidTune(binarna_Y, binarna_X, binarna, algo = 'Tree', kFold = 10, parTune = parTune_Tree_bin, seed = 333)

parTune_Tree_multi <- expand.grid(type = c('Entropy', 'Gini'), depth = c(3,6,9), minobs = c(2,5,10), overfit = c('prune', 'none'), cf = c(0.05, 0.022))
CV_Tree_multi <- CrossValidTune(wieloklasowa_Y, wieloklasowa_X, wieloklasowa, algo = 'Tree', kFold = 10, parTune = parTune_Tree_multi, seed = 333)


# KNN

parTune_KNN_reg <- expand.grid(k = c(2:15))
CV_KNN_reg <- CrossValidTune(regresja_Y, regresja_X, regresja, algo = 'KNN', kFold = 10, parTune = parTune_KNN_reg, seed = 333)

parTune_KNN_bin <- expand.grid(k = c(2:15))
CV_KNN_bin <- CrossValidTune(binarna_Y, binarna_X, binarna, algo = 'KNN', kFold = 10, parTune = parTune_KNN_bin, seed = 333)

parTune_KNN_multi <- expand.grid(k = c(2:15))
CV_KNN_multi <- CrossValidTune(wieloklasowa_Y, wieloklasowa_X, wieloklasowa, algo = 'KNN', kFold = 10, parTune = parTune_KNN_multi, seed = 333)





# Wykresy AUC, Jakosc / MSE, MAE, MAPE od parametrow 


# Sieci Neuronowe - grupowanie i wykresy w zaleznosci od parametrow

NN_reg_CV_wynik <- CV_NN_reg
NN_reg_CV_wynik[is.na(NN_reg_CV_wynik)] <- 0
NN_reg_CV_wynik$h <- as.character(NN_reg_CV_wynik$h)
NN_reg_CV_wynik$h <- str_remove(NN_reg_CV_wynik$h, pattern = "c")
NN_reg_CV_wynik <- NN_reg_CV_wynik %>% group_by( h, lr, iter)
NN_reg_CV_wynik_gr <- as.data.frame(NN_reg_CV_wynik %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
NN_reg_best_T <- NN_reg_CV_wynik_gr[which.min(NN_reg_CV_wynik_gr$MAPET),]
NN_reg_best_W <- NN_reg_CV_wynik_gr[which.min(NN_reg_CV_wynik_gr$MAPEW),]
print(NN_reg_CV_wynik_gr)
print(NN_reg_best_T)
print(NN_reg_best_W)

ggplot(NN_reg_CV_wynik_gr , aes(x=1:nrow(NN_reg_CV_wynik_gr))) +
  geom_line(aes(y = MAET, color='red'), size=1, ) +
  geom_line(aes(y = MAEW, color='blue'), size=1,) +
  labs(title='NN - Regresja: MAE dla kazdego modelu', x='ID modelu', y='MAE') +
  scale_color_discrete(name = "Predykcja na zbiorze", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")

ggplot(NN_reg_CV_wynik_gr , aes(x=1:nrow(NN_reg_CV_wynik_gr))) +
  geom_line(aes(y = MAPET, color='red'), size=1, ) +
  geom_line(aes(y = MAPEW, color='blue'), size=1,) +
  labs(title='NN - Regresja: MAPE dla kazdego modelu', x='ID modelu', y='MAPE') +
  scale_color_discrete(name = "Predykcja na zbiorze", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")



NN_bin_CrossValid_wynik <- CV_NN_bin
NN_bin_CrossValid_wynik[is.na(NN_bin_CrossValid_wynik)] <- 0
NN_bin_CrossValid_wynik$h <- as.character(NN_bin_CrossValid_wynik$h)
NN_bin_CrossValid_wynik$h <- str_remove(NN_bin_CrossValid_wynik$h, pattern = "c")
NN_bin_CrossValid_wynik <- NN_bin_CrossValid_wynik %>% group_by( h, lr, iter)
NN_bin_CrossValid_wynik_gr <- as.data.frame(NN_bin_CrossValid_wynik %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
                                                                                  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
NN_bin_best_T <- NN_bin_CrossValid_wynik_gr[which.max(NN_bin_CrossValid_wynik_gr$JakoscT),]
NN_bin_best_W <- NN_bin_CrossValid_wynik_gr[which.max(NN_bin_CrossValid_wynik_gr$JakoscW),]
print(NN_bin_CrossValid_wynik_gr)
print(NN_bin_best_T)
print(NN_bin_best_W)

NN_bin_CrossValid_gr_lr <- NN_bin_CrossValid_wynik %>% group_by(h, lr)
NN_bin_CrossValid_gr_lr <- as.data.frame(NN_bin_CrossValid_gr_lr %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
                                                                                   AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
print(NN_bin_CrossValid_gr_lr)
bin_lr_1 <- NN_bin_CrossValid_gr_lr[NN_bin_CrossValid_gr_lr$lr == unique(NN_bin_CrossValid_gr_lr$lr)[1], c("h", "JakoscW")]
bin_lr_2 <- NN_bin_CrossValid_gr_lr[NN_bin_CrossValid_gr_lr$lr == unique(NN_bin_CrossValid_gr_lr$lr)[2], c("h", "JakoscW")]
bin_lr_3 <- NN_bin_CrossValid_gr_lr[NN_bin_CrossValid_gr_lr$lr == unique(NN_bin_CrossValid_gr_lr$lr)[3], c("h", "JakoscW")]
NN_bin_Porownanie_lr <- cbind("nr_wariantu" = c(1:nrow(bin_lr_1)), bin_lr_1, "JakoscW_2" = bin_lr_2$JakoscW, "JakoscW_3" = bin_lr_3$JakoscW)

ggplot(NN_bin_Porownanie_lr , aes(x=nr_wariantu)) +
  geom_line(aes(y = JakoscW, color='green'), size=1, ) +
  geom_line(aes(y = JakoscW_2, color='blue'), size=1,) +
  geom_line(aes(y = JakoscW_3, color='red'), size=1,) +
  labs(title='NN - Klas. Binarna: porownanie wynikow dla roznych wspolczynnikow uczenia (lr)', x='Wariant warstw', y='Jakosc') +
  scale_color_discrete(name = "Learning Rate", labels = c(as.character(unique(NN_bin_CrossValid_gr_lr$lr)[2]), as.character(unique(NN_bin_CrossValid_gr_lr$lr)[1]), as.character(unique(NN_bin_CrossValid_gr_lr$lr)[3]))) +
  theme(legend.position = "bottom")



NN_multi_CrossValid_wynik <- CV_NN_multi
NN_multi_CrossValid_wynik[is.na(NN_multi_CrossValid_wynik)] <- 0
NN_multi_CrossValid_wynik$h <- as.character(NN_multi_CrossValid_wynik$h)
NN_multi_CrossValid_wynik$h <- str_remove(NN_multi_CrossValid_wynik$h, pattern = "c")
NN_multi_CrossValid_wynik <- NN_multi_CrossValid_wynik %>% group_by( h, lr, iter)
NN_multi_CrossValid_wynik_gr <- as.data.frame(NN_multi_CrossValid_wynik %>% summarise(JakoscT = mean(JakoscT), JakoscW = mean(JakoscW)))
NN_multi_best_T <- NN_multi_CrossValid_wynik_gr[which.max(NN_multi_CrossValid_wynik_gr$JakoscT),]
NN_multi_best_W <- NN_multi_CrossValid_wynik_gr[which.max(NN_multi_CrossValid_wynik_gr$JakoscW),]
print(NN_multi_CrossValid_wynik_gr)
print(NN_multi_best_T)
print(NN_multi_best_W)

NN_multi_CrossValid_gr_lr <- NN_multi_CrossValid_wynik %>% group_by(h, lr)
NN_multi_CrossValid_gr_lr <- as.data.frame(NN_multi_CrossValid_gr_lr %>% summarise(JakoscT = mean(JakoscT), JakoscW = mean(JakoscW), ))
print(NN_multi_CrossValid_gr_lr)
bin_lr_1 <- NN_multi_CrossValid_gr_lr[NN_multi_CrossValid_gr_lr$lr == unique(NN_multi_CrossValid_gr_lr$lr)[1], c("h", "JakoscW")]
bin_lr_2 <- NN_multi_CrossValid_gr_lr[NN_multi_CrossValid_gr_lr$lr == unique(NN_multi_CrossValid_gr_lr$lr)[2], c("h", "JakoscW")]
bin_lr_3 <- NN_multi_CrossValid_gr_lr[NN_multi_CrossValid_gr_lr$lr == unique(NN_multi_CrossValid_gr_lr$lr)[3], c("h", "JakoscW")]
NN_multi_Porownanie_lr <- cbind("nr_wariantu" = c(1:nrow(bin_lr_1)), bin_lr_1, "JakoscW_2" = bin_lr_2$JakoscW, "JakoscW_3" = bin_lr_3$JakoscW)

ggplot(NN_multi_Porownanie_lr , aes(x=nr_wariantu)) + 
  geom_line(aes(y = JakoscW, color='green'), size=1, ) + 
  geom_line(aes(y = JakoscW_2, color='blue'), size=1,) + 
  geom_line(aes(y = JakoscW_3, color='red'), size=1,) +
  labs(title='NN - Klas. Multi: porownanie wynikow dla roznych wspolczynnikow uczenia (lr)', x='Wariant warstw', y='Jakosc') +
  scale_color_discrete(name = "Learning Rate", labels = c(as.character(unique(NN_multi_CrossValid_gr_lr$lr)[2]), as.character(unique(NN_multi_CrossValid_gr_lr$lr)[1]), as.character(unique(NN_multi_CrossValid_gr_lr$lr)[3]))) +
  theme(legend.position = "bottom")



# Drzewa Decyzyjne - grupowanie wynikow i wykresy

Tree_reg_CrossValid_gr <- CV_Tree_reg %>% group_by(depth, minobs)
Tree_reg_CrossValid_gr[is.na(Tree_reg_CrossValid_gr)] <- 0
Tree_reg_CrossValid_gr <- as.data.frame(Tree_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Tree_reg_best_T <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAET),]
Tree_reg_best_W <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAEW),]
print(Tree_reg_CrossValid_gr)
print(Tree_reg_best_T)
print(Tree_reg_best_W)

ggplot(Tree_reg_CrossValid_gr , aes(x=1:nrow(Tree_reg_CrossValid_gr))) +
  geom_line(aes(y = MAET, color='red'), size=1, ) +
  geom_line(aes(y = MAEW, color='blue'), size=1,) +
  labs(title='Tree - Regresja: MAE dla kazdego modelu', x='ID modelu', y='MAE') +
  scale_color_discrete(name = "Predykcja na zbiorze: ", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")

ggplot(Tree_reg_CrossValid_gr , aes(x=1:nrow(Tree_reg_CrossValid_gr))) +
  geom_line(aes(y = MAPET, color='red'), size=1, ) +
  geom_line(aes(y = MAPEW, color='blue'), size=1,) +
  labs(title='Tree - Regresja: MAPE dla kazdego modelu', x='ID modelu', y='MAPE') +
  scale_color_discrete(name = "Predykcja na zbiorze: ", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")



Tree_bin_CrossValid_gr <- CV_Tree_bin %>% group_by(depth, minobs, type, overfit, cf)
Tree_bin_CrossValid_gr[is.na(Tree_bin_CrossValid_gr)] <- 0
Tree_bin_CrossValid_gr <- as.data.frame(Tree_bin_CrossValid_gr %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
                                                                             AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Tree_bin_best_T <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscT),]
Tree_bin_best_W <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscW),]
print(Tree_bin_CrossValid_gr)
print(Tree_bin_best_T)
print(Tree_bin_best_W)

ggplot(Tree_bin_CrossValid_gr , aes(x=1:nrow(Tree_bin_CrossValid_gr))) +
  geom_line(aes(y = JakoscT, color='red'), size=1, ) +
  geom_line(aes(y = JakoscW, color='blue'), size=1,) +
  labs(title='Tree - Klas. Binarna: Jakosc dla kazdego modelu', x='ID modelu', y='Jakosc') +
  scale_color_discrete(name = "Predykcja na zbiorze", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")


Tree_multi_CrossValid_gr <- CV_Tree_multi %>% group_by(depth, minobs, type, overfit, cf)
Tree_multi_CrossValid_gr[is.na(Tree_multi_CrossValid_gr)] <- 0
Tree_multi_CrossValid_gr <- as.data.frame(Tree_multi_CrossValid_gr %>% summarise(JakoscT = mean(JakoscT), JakoscW = mean(JakoscW)))
Tree_multi_best_T <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$JakoscT),]
Tree_multi_best_W <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$JakoscW),]
print(Tree_multi_CrossValid_gr)
print(Tree_multi_best_T)
print(Tree_multi_best_W)

ggplot(Tree_multi_CrossValid_gr , aes(x=1:nrow(Tree_multi_CrossValid_gr))) +
  geom_line(aes(y = JakoscT, color='red'), size=1, ) +
  geom_line(aes(y = JakoscW, color='blue'), size=1,) +
  labs(title='Tree - Klas. Multi: Jakosc dla kazdego modelu', x='ID modelu', y='Jakosc') +
  scale_color_discrete(name = "Predykcja na zbiorze", labels = c("Treningowym", "Walidacyjnym")) +
  theme(legend.position = "bottom")



# KNN - grupowanie wynikow

KNN_reg_CrossValid_gr <- CV_KNN_reg %>% group_by(k)
KNN_reg_CrossValid_gr[is.na(KNN_reg_CrossValid_gr)] <- 0
KNN_reg_CrossValid_gr <- as.data.frame(KNN_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
KNN_reg_best_T <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAET),]
KNN_reg_best_W <- KNN_reg_CrossValid_gr[which.min(KNN_reg_CrossValid_gr$MAEW),]
print(KNN_reg_CrossValid_gr)
print(KNN_reg_best_T)
print(KNN_reg_best_W)


KNN_bin_CrossValid_gr <- CV_KNN_bin %>% group_by(k)
KNN_bin_CrossValid_gr[is.na(KNN_bin_CrossValid_gr)] <- 0
KNN_bin_CrossValid_gr <- as.data.frame(KNN_bin_CrossValid_gr %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
                                                                           AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
KNN_bin_best_T <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscT),]
KNN_bin_best_W <- KNN_bin_CrossValid_gr[which.max(KNN_bin_CrossValid_gr$JakoscW),]
print(KNN_bin_CrossValid_gr)
print(KNN_bin_best_T)
print(KNN_bin_best_W)


KNN_multi_CrossValid_gr <- CV_KNN_multi %>% group_by(k)
KNN_multi_CrossValid_gr[is.na(KNN_multi_CrossValid_gr)] <- 0
KNN_multi_CrossValid_gr <- as.data.frame(KNN_multi_CrossValid_gr %>% summarise(JakoscT = mean(JakoscT), JakoscW = mean(JakoscW)))
KNN_multi_best_T <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$JakoscT),]
KNN_multi_best_W <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$JakoscW),]
print(KNN_multi_CrossValid_gr)
print(KNN_multi_best_T)
print(KNN_multi_best_W)





#######################################################################################################
# Biblioteki R - train na - nnet, rpart, caret


cv_R <- trainControl(method="cv", number=10)

# Sieci neuronowe

regresja_norm <- regresja
regresja_norm <- data.frame(sapply(regresja, as.numeric))
regresja_norm <- sapply(regresja_norm, MinMax)
summary(regresja_norm)
nn_grid_reg = expand.grid(size=2:10, decay = c(0.0001, 0.01))
NN_reg_R = train(x=regresja_norm[,regresja_X], y=regresja_norm[,regresja_Y], tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results
print(paste0("Siec NN w R - reg: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]][1]))

binarna_norm <- binarna
binarna_norm[binarna_X] <- data.frame(sapply(binarna[binarna_X], as.numeric))
binarna_norm[binarna_X] <- sapply(binarna_norm[binarna_X], MinMax)
summary(binarna_norm)
nn_grid_bin = expand.grid(size=2:10, decay = c(0.0001, 0.01))
NN_bin_R = train(x=binarna_norm[,binarna_X], y=binarna[,binarna_Y], tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results
print(paste0("Siec NN w R - bin: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))

wieloklasowa_norm <- wieloklasowa
wieloklasowa_norm[wieloklasowa_X] <- data.frame(sapply(wieloklasowa[wieloklasowa_X], as.numeric))
wieloklasowa_norm[wieloklasowa_X] <- sapply(wieloklasowa_norm[wieloklasowa_X], MinMax)
summary(wieloklasowa_norm)
nn_grid_multi = expand.grid(size=2:10, decay = c(0.0001, 0.01))
NN_multi_R = train(x=wieloklasowa_norm[,wieloklasowa_X], y=wieloklasowa[,wieloklasowa_Y], tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results
print(paste("Siec NN w R - multi: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))



# Drzewa decyzyjne

tree_grid_reg = expand.grid(maxdepth=3:9)
Tree_reg_R = train(x=regresja[,regresja_X], y=regresja[,regresja_Y], tuneGrid=tree_grid_reg, method='rpart2', metric='MAE', trControl=cv_R)
Tree_reg_R_Wynik = Tree_reg_R$results
print(paste("Drzewo w R - reg: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

tree_grid_bin = expand.grid(maxdepth=3:9)
Tree_bin_R = train(x=binarna[,binarna_X], y=binarna[,binarna_Y], tuneGrid=tree_grid_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_bin_R_Wynik = Tree_bin_R$results
print(paste("Drzewo w R - bin: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

tree_grid_multi = expand.grid(maxdepth=3:9)
Tree_multi_R = train(x=wieloklasowa[,wieloklasowa_X], y=wieloklasowa[,wieloklasowa_Y], tuneGrid=tree_grid_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_multi_R_Wynik = Tree_multi_R$results
print(paste("Drzewo w R - multi: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))



# KNN

regresja_R <- regresja
regresja_R[,1] <- as.numeric(regresja_R[,1])
knn_grid_reg = expand.grid(k=2:15)
KNN_reg_R = train(x=regresja_R[,regresja_X], y=regresja_R[,regresja_Y], tuneGrid=knn_grid_reg, method='knn', metric='MAE', trControl=cv_R)
KNN_reg_R_Wynik = KNN_reg_R$results
print(paste("KNN w R - reg: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))

binarna_R <- binarna
binarna_R[,7] <- as.numeric(as.character(as.numeric(binarna_R[,7])))
knn_grid_bin = expand.grid(k=2:15)
KNN_bin_R = train(x=binarna_R[,binarna_X], y=binarna_R[,binarna_Y], tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results
print(paste("KNN w R - bin: k = ", KNN_bin_R$finalModel$k, " | Jakosc = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))

knn_grid_multi = expand.grid(k=2:15)
KNN_multi_R = train(x=wieloklasowa[,wieloklasowa_X], y=wieloklasowa[,wieloklasowa_Y], tuneGrid=knn_grid_multi, method='knn', metric='Accuracy', trControl=cv_R)
KNN_multi_R_Wynik = KNN_multi_R$results
print(paste("KNN w R - multi: k = ", KNN_multi_R$finalModel$k, " | Jakosc = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))


# Porownanie KNN

VS_KNN_Reg <- data.frame(k = c(1:15))
VS_KNN_Reg <- merge(VS_KNN_Reg, KNN_reg_CrossValid_gr[,c('k', "MAET")], by = 'k')
VS_KNN_Reg <- merge(VS_KNN_Reg, KNN_reg_CrossValid_gr[,c('k', "MAEW")], by = 'k')
VS_KNN_Reg <- merge(VS_KNN_Reg, KNN_reg_R_Wynik[,c('k', "MAE")], by = 'k')
print(VS_KNN_Reg)

ggplot(VS_KNN_Reg , aes(x=k)) + geom_line(aes(y = MAET, color='green'), size=1, ) + geom_line(aes(y = MAEW, color='blue'), size=1, ) + geom_line(aes(y = MAE, color='red'), size=1,) +
  labs(title='KNN - Regresja: MAE od k', x='k', y='MAE') + scale_color_discrete(name = "Implementacja:", labels = c("Wlasna (W)", "Wlasna (T)","Biblioteka R")) + theme(legend.position = "bottom")


VS_KNN_Bin <- data.frame(k = c(1:15))
VS_KNN_Bin <- merge(VS_KNN_Bin, KNN_bin_CrossValid_gr[,c('k', "JakoscT")], by = 'k')
VS_KNN_Bin <- merge(VS_KNN_Bin, KNN_bin_CrossValid_gr[,c('k', "JakoscW")], by = 'k')
VS_KNN_Bin <- merge(VS_KNN_Bin, KNN_bin_R_Wynik[,c('k', "Accuracy")], by = 'k')
print(VS_KNN_Bin)

ggplot(VS_KNN_Bin , aes(x=k)) + geom_line(aes(y = JakoscT, color='green'), size=1, ) + geom_line(aes(y = JakoscW, color='blue'), size=1, ) + geom_line(aes(y = Accuracy, color='red'), size=1,) +
  labs(title='KNN - Klas. Binarna: Jakosc od k', x='k', y='Jakosc') + scale_color_discrete(name = "Implementacja:", labels = c("Wlasna (W)", "Wlasna (T)", "Biblioteka R")) + theme(legend.position = "bottom")


VS_KNN_Multi <- data.frame(k = c(1:15))
VS_KNN_Multi <- merge(VS_KNN_Multi, KNN_multi_CrossValid_gr[,c('k', "JakoscT")], by = 'k')
VS_KNN_Multi <- merge(VS_KNN_Multi, KNN_multi_CrossValid_gr[,c('k', "JakoscW")], by = 'k')
VS_KNN_Multi <- merge(VS_KNN_Multi, KNN_multi_R_Wynik[,c('k', "Accuracy")], by = 'k')
print(VS_KNN_Multi)

ggplot(VS_KNN_Multi , aes(x=k)) + geom_line(aes(y = JakoscT, color='green'), size=1, ) + geom_line(aes(y = JakoscW, color='blue'), size=1, ) + geom_line(aes(y = Accuracy, color='red'), size=1,) +
  labs(title='KNN - Klas. Wieloklasowa: Jakosc od k', x='k', y='Jakosc') + scale_color_discrete(name = "Implementacja:", labels = c("Wlasna (W)", "Wlasna (T)","Biblioteka R")) + theme(legend.position = "bottom")



