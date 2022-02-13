source("funkcje.R")


# Dane do Projektu:
# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Breast+Cancer         
# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/Abalone
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Automobile


bin_cancer <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer/breast-cancer.data", header = FALSE)
# bin_cancer <- read.csv("breast-cancer.data", header = FALSE)
bin_cancer_X <- colnames(bin_cancer)[-1]
bin_cancer_Y <- colnames(bin_cancer)[1]
bin_cancer[,bin_cancer_Y] <- as.factor(bin_cancer[,bin_cancer_Y])
summary(bin_cancer)
bin_cancer <- bin_cancer[! bin_cancer$V6 == "?",]       # Usuniecie wierszy z wartosciami '?'
bin_cancer$V6 <- as.factor(as.character(bin_cancer$V6))
bin_cancer <- bin_cancer[! bin_cancer$V9 == "?",]       # Usuniecie wierszy z wartosciami '?'
bin_cancer$V9 <- as.factor(as.character(bin_cancer$V9))
bin_cancer$V2 <- as.numeric(bin_cancer$V2)
bin_cancer$V3 <- as.numeric(bin_cancer$V3)
bin_cancer$V4 <- as.numeric(bin_cancer$V4)
bin_cancer$V5 <- as.numeric(bin_cancer$V5)
bin_cancer$V6 <- as.numeric(bin_cancer$V6)
bin_cancer$V7 <- as.numeric(bin_cancer$V7)
bin_cancer$V8 <- as.numeric(bin_cancer$V8)
bin_cancer$V9 <- as.numeric(bin_cancer$V9)
bin_cancer$V10 <- as.numeric(bin_cancer$V10)
summary(bin_cancer)


multi_abalone <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header = FALSE, sep = ",")
multi_abalone_X <- colnames(multi_abalone)[-1]
multi_abalone_Y <- colnames(multi_abalone)[1]
multi_abalone[,1] <- as.factor(as.numeric((multi_abalone[,1])))       # Dla pewnosci ze pierwsza kolumna - "Y" - jest factorem
summary(multi_abalone)


reg_automobile <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data", header = FALSE, sep = ",")
reg_automobile_X <- colnames(reg_automobile)[-26]
reg_automobile_Y <- colnames(reg_automobile)[26]
reg_automobile <- reg_automobile[! reg_automobile$V26 == "?",]          # Usuniecie wierszy z wartosciami '?'
reg_automobile <- as.data.frame(sapply(reg_automobile, as.numeric))     # Zamiana kolumn na numeryczne 
summary(reg_automobile)


# KNN

print("KNN - bin")
hiper_parametry_KNN_bin <- expand.grid(k=c(2:10))   
Kroswalidacja_KNN_bin <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 9, hiper_parametry_KNN_bin, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_bin)


print("KNN - multi")
hiper_parametry_KNN_multi <- expand.grid(k=c(2:8))
Kroswalidacja_KNN_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_KNN_multi, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_multi)


print("KNN - reg")
hiper_parametry_KNN_reg <- expand.grid(k=c(2:12))
Kroswalidacja_KNN_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_KNN_reg, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_reg)




# Tree

print("Tree - bin")
hiper_parametry_Tree_bin <- expand.grid(depth=c(3,5,10), minobs=c(2,10), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.4))
Kroswalidacja_Tree_bin <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 9, hiper_parametry_Tree_bin, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_bin)


print("Tree - multi")
hiper_parametry_Tree_multi <- expand.grid(depth=c(3,10), minobs=c(2,10), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.4))
Kroswalidacja_Tree_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_Tree_multi, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_multi)


print("Tree - reg")
hiper_parametry_Tree_reg <- expand.grid(depth=c(3,10), minobs=c(2,10), type=c('SS'), overfit = c('none'), cf=c(0.4))
Kroswalidacja_Tree_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_Tree_reg, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_reg)



# NN

print("Sieci NN - bin")
hiper_parametry_NN_bin <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_bin <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 9, hiper_parametry_NN_bin, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_bin)


print("Sieci NN - multi")
hiper_parametry_NN_multi <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_NN_multi, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_multi)


print("Sieci NN - reg")
hiper_parametry_NN_reg <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_NN_reg, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_reg)




# Wyniki dla modeli wbudowanych w biblioteki R

kontrola_kroswalidacji <- trainControl(method="cv", number=9)

print("KNN - R - bin")
R_GRID_KNN_bin = expand.grid(k=2:20)
R_CV_KNN_binarna = train(x=bin_cancer[,bin_cancer_X], y=bin_cancer[,bin_cancer_Y], tuneGrid=R_GRID_KNN_bin, method='knn', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_KNN_binarna_Wynik = R_CV_KNN_binarna$results


print("KNN - R - multi")
R_GRID_KNN_multi = expand.grid(k=2:20)
R_CV_KNN_wieloklasowa = train(x=multi_abalone[,multi_abalone_X], y=multi_abalone[,multi_abalone_Y], tuneGrid=R_GRID_KNN_multi, method='knn', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_KNN_wieloklasowa_Wynik = R_CV_KNN_wieloklasowa$results


print("KNN - R - reg")
R_GRID_KNN_reg = expand.grid(k=2:20)
R_CV_KNN_regresja = train(x=reg_automobile[,reg_automobile_X], y=reg_automobile[,reg_automobile_Y], tuneGrid=R_GRID_KNN_reg, method='knn', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_KNN_regresja_Wynik = R_CV_KNN_regresja$results



print("TREE - R - bin")
R_GRID_TREE_bin = expand.grid(maxdepth=2:20)
R_CV_TREE_binarna = train(x=bin_cancer[,bin_cancer_X], y=bin_cancer[,bin_cancer_Y], tuneGrid=R_GRID_TREE_bin, method='rpart2', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_TREE_binarna_Wynik = R_CV_TREE_binarna$results


print("TREE - R - multi")
R_GRID_TREE_wieloklasowa = expand.grid(maxdepth=2:20)
R_CV_TREE_wieloklasowa = train(x=multi_abalone[,multi_abalone_X], y=multi_abalone[,multi_abalone_Y], tuneGrid=R_GRID_TREE_wieloklasowa, method='rpart2', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_TREE_wieloklasowa_Wynik = R_CV_TREE_wieloklasowa$results


print("TREE - R - reg")
R_GRID_TREE_regresja = expand.grid(maxdepth=2:20)
R_CV_TREE_regresja = train(x=reg_automobile[,reg_automobile_X], y=reg_automobile[,reg_automobile_Y], tuneGrid=R_GRID_TREE_regresja, method='rpart2', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_TREE_regresja_Wynik = R_CV_TREE_regresja$results



print("Neural Network - R - bin")
bin_cancer_norm <- MinMax(bin_cancer[,bin_cancer_X])
R_GRID_NN_binarna = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_binarna = train(x=bin_cancer_norm, y=bin_cancer[,bin_cancer_Y], tuneGrid=R_GRID_NN_binarna, method='nnet', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_NN_binarna_Wynik = R_CV_NN_binarna$results


print("Neural Network - R - multi")
multi_abalone_norm <- MinMax(multi_abalone[,multi_abalone_X])
R_GRID_NN_wieloklasowa = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_wieloklasowa = train(x=multi_abalone_norm, y=multi_abalone[,multi_abalone_Y], tuneGrid=R_GRID_NN_wieloklasowa, method='nnet', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_NN_wieloklasowa_Wynik = R_CV_NN_wieloklasowa$results


print("Neural Network - R - reg")
reg_automobile_norm <- MinMax(reg_automobile)
R_GRID_NN_regresja = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_regresja = train(x=reg_automobile_norm[,reg_automobile_X], y=reg_automobile_norm[,reg_automobile_Y], tuneGrid=R_GRID_NN_regresja, method='nnet', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_NN_regresja_Wynik = R_CV_NN_regresja$results







