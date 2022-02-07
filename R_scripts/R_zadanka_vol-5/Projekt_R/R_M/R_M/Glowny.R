source("funkcje.R")

print("******************************************************")
print("*** Wczytywanie danych do klasyfikacji binarnej - Breast Cancer Wisconsin (Diagnostic)")         # http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29
binarna <- read.csv("breast-cancer-wisconsin.data", header = FALSE) 
binarna[,11] <- as.factor(binarna[,11])                                                                 # Klasy zapisane jako wartosci numeryczne -> "2 for benign, 4 for malignant" (z notki)
# Usunac znaki zapytania '?' z danych
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
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", anyNA(wieloklasowa)))        # Brak wartosci NA - dane mialy specyficzny separator, ktory zostal zamieniony na srednik + upewniono sie ze nie ma zadnych problemow z separatorem
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(wieloklasowa))
print("Rozklad klas w danych: ")
print(sort(summary(wieloklasowa[,9])))


print("******************************************************")
print("*** Wczytywanie danych do regresji - Computer Hardware")                                         # https://archive.ics.uci.edu/ml/datasets/Computer+Hardware
regresja <- read.csv("machine.data", header = FALSE)[,-10]                                              # Ostatnia kolumna wedlug notki to estymacja wydajnosci (kolumny nr 9) z jakiegos artykulu
regresja[,1] <- as.factor(regresja[,1])
regresja[,2] <- as.factor(regresja[,2])
regresja_Y <- colnames(regresja)[9]
regresja_X <- colnames(regresja)[-9]
print(paste0("Czy dane do klasyfikacji wieloklasowej maja warstosci NA: ", anyNA(regresja))) 
print("Podsumowanie danych w kazdej kolumnie:")
print(summary(regresja))



source("funkcje.R")

# Sieci Neuronowe

binarna_NN <- binarna
binarna_NN[,binarna_Y] <- as.numeric(binarna[,binarna_Y])
binarna_NN[,7] <- as.numeric(binarna_NN[,7])
binarna_NN <- sapply(binarna_NN, MinMax)
binarna_NN

# HELL <- trainNN(binarna_Y, binarna_X, binarna_NN, h = c(2,4), lr = 0.01, iter = 2000, seed = 300, type = 'bin')
# HELL$y_hat
# 
# table(binarna_NN[,binarna_Y], ifelse(HELL$y_hat >= 0.5, 1, 0))
# table(binarna_NN[,binarna_Y], ifelse(predNN(binarna_NN[,binarna_X], HELL) >= 0.5, 1, 0))
# ModelOcena(as.factor(binarna_NN[,binarna_Y]), predNN(binarna_NN[,binarna_X], HELL))


wieloklasowa_NN <- wieloklasowa
wieloklasowa_NN <- sapply(wieloklasowa_NN, as.numeric)
wieloklasowa_NN <- sapply(wieloklasowa_NN, MinMax_bez_przedzialu)
wieloklasowa_NN[,1] <- MinMax_bez_przedzialu(wieloklasowa_NN[,1])

HELL_2 <- trainNN(wieloklasowa_Y, wieloklasowa_X, wieloklasowa_NN, h = c(2,4), lr = 0.01, iter = 2000, seed = 300, type = 'multi')




# Drzewa Decyzyjne

# Tree_1 <- Tree(binarna_Y, binarna_X, binarna, type = "Entropy", depth = 6, minobs = 2, overfit = 'none', cf = 0.2)
# Tree_wynik <- PredictTree(Tree_1, binarna[,binarna_X])
# table(binarna[,binarna_Y], ifelse(Tree_wynik[,2] >= 0.5, 1, 0))
# ModelOcena(binarna[,binarna_Y], as.numeric(Tree_wynik[,2]))

# Tree_2 <- Tree(wieloklasowa_Y, wieloklasowa_X, wieloklasowa, type = "Entropy", depth = 6, minobs = 2, overfit = 'none', cf = 0.2)
# Tree_wynik_2 <- PredictTree(Tree_2, wieloklasowa[,wieloklasowa_X])
# ModelOcena(wieloklasowa[,wieloklasowa_Y], (Tree_wynik_2[,'Class']))

# Tree_3 <- Tree(regresja_Y, regresja_X, regresja, type = "SS", depth = 6, minobs = 2, overfit = 'none', cf = 0.2)
# Tree_wynik_3 <- PredictTree(Tree_3, regresja[,regresja_X])
# ModelOcena(regresja[,regresja_Y], (Tree_wynik_3))




# KNN

# KNN_Model <- KNNtrain(binarna[,binarna_X], binarna[,binarna_Y], k=2, 0, 1)
# KNN_wynik <- KNNpred(KNNmodel = KNN_Model, binarna[,binarna_X], Ncores = 10)
# ModelOcena(binarna[,binarna_Y], KNN_wynik[,3])
# 
# KNN_Model_multi <- KNNtrain(wieloklasowa[,wieloklasowa_X], wieloklasowa[,wieloklasowa_Y], k=2, 0, 1)
# KNN_wynik_multi <- KNNpred(KNNmodel = KNN_Model_multi, wieloklasowa[,wieloklasowa_X], Ncores = 20)
# ModelOcena(wieloklasowa[,wieloklasowa_Y], KNN_wynik_multi[,length(KNN_wynik_multi)])
# 
# KNN_Model_reg <- KNNtrain(regresja[,regresja_X], regresja[,regresja_Y], k=2, 0, 1)
# KNN_wynik_reg <- KNNpred(KNNmodel = KNN_Model_reg, regresja[,regresja_X], Ncores = 20)
# ModelOcena(regresja[,regresja_Y], KNN_wynik_reg)








# Wykresy AUC, Jakosc / MSE, MAE, MAPE od parametrow 






#######################################################################################################
# Kroswalidacja na modelach z bibliotek R                                   # ZROBIC Z TEGO FUNKCJE


cv_R <- trainControl(method="cv", number=10)

# KNN
binarna_R <- binarna
binarna_R[,7] <- as.numeric(as.character(as.numeric(binarna_R[,7])))
knn_grid_bin = expand.grid(k=2:20)
KNN_bin_R = train(x=binarna_R[,binarna_X], y=binarna_R[,binarna_Y], tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results
print(paste("KNN w R - bin: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))



# CrossValidTune_R <- function(data_Y_names, data_X_names, data, tuneGrid, method, metric, train_control, kFold, seed){
#   
#   set.seed(seed)
#   n = nrow(data)
#   lista <- data.frame(matrix(ncol = kFold, nrow = nrow(data)))
#   
#   
#   if(method == 'knn'){
#     KNN = train(x = data[,data_X_names], y = data[,data_Y_names], tuneGrid = tuneGrid, method = method, metric = metric, trControl = train_control)
# 
#     ocena_calosci <- list()
#     
#     for(i in 1:kFold)
#     { 
#       id_sample <- sample( 1:n, size = (1/kFold * n)-1, replace = F )
#       walidacja_Y <- data[id_sample,data_Y_names]
#       walidacja_X <- data[id_sample,data_X_names]
#       wynik_predykcji <- predict(KNN, walidacja_X, type = "prob")
#       
#       print(walidacja_Y)
#       print(wynik_predykcji)
#       
#       ocena <- ModelOcena(y_tar = walidacja_Y, y_hat = wynik_predykcji[,2])
#       ocena_calosci[[i]] <- ocena
#     }
#     print(ocena_calosci)
#     return(ocena_calosci)
#     return(list("Wyniki" = KNN$results, "Best_Model" = KNN$finalModel, "Best_Model_k" = KNN$finalModel$k, "Best_Model_statistic" = KNN$finalModel$Accuracy))
#   }
#   
#   # return(list("Wyniki" = KNN$results, "Best_Model" = KNN$finalModel))
# }

# kar <- CrossValidTune_R(binarna_Y, binarna_X, binarna_R, knn_grid_bin, 'knn', 'Accuracy',train_control = cv_R, kFold = 10, seed = 371)


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





