source("funkcje.R")


# Dane do Projektu:
# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Breast+Cancer         
# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/Abalone
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set


bin_cancer <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer/breast-cancer.data", header = FALSE)
bin_cancer <- read.csv("breast-cancer.data", header = FALSE)
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


# dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header = FALSE, sep = "\t")
# dane_multi <- drop_na(dane_multi)
# multi_kolumny <- colnames(dane_multi)               
# dane_multi_X <- multi_kolumny[-8]
# dane_multi_Y <- multi_kolumny[8]
# dane_multi[,8] <- as.factor(dane_multi[,8])



# dane_reg <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/cpu-performance/machine.data", header = FALSE)
# dane_reg <- dane_reg[,-c(2,10)]
# dane_reg[,1] <- as.numeric(dane_reg[,1])
# reg_kolumny <- colnames(dane_reg)               
# dane_reg_X <- reg_kolumny[-8]
# dane_reg_Y <- reg_kolumny[8]



# KNN

print("KNN - bin")
parTune_KNN_bin <- expand.grid(k=c(2:2))   
KNN_bin_CrossValid <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 10, parTune_KNN_bin, algorytm="KNN", seed = 123)
KNN_bin_CrossValid

KNN_Model <- KNNtrain(bin_cancer[,bin_cancer_X], bin_cancer[,bin_cancer_Y], 20, 0, 1)
KNN_pred_Trening <- KNNpred(KNN_Model, X=bin_cancer[,bin_cancer_X])
KNN_pred_Trening


print("KNN - multi")
parTune_KNN_multi <- expand.grid(k=c(2:50))
KNN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 6, parTune_KNN_multi, algorytm="KNN", seed = 123)
KNN_multi_CrossValid



print("KNN - reg")
parTune_KNN_reg <- expand.grid(k=c(3,6,9))
KNN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 6, parTune_KNN_reg, algorytm="KNN", seed = 123)
KNN_reg_CrossValid




# Tree

print("Tree - bin")
parTune_Tree_bin <- expand.grid(depth=c(2:8), minobs=c(2:8), type=c('Entropy', 'Gini'), overfit = c('none'), cf=c(0.0))
Tree_bin_CrossValid <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 10, parTune_Tree_bin, algorytm="Tree", seed = 123)
Tree_bin_CrossValid


print("Tree - multi")
parTune_Tree_multi <- expand.grid(depth=c(2:8), minobs=c(2:8), type=c('Entropy', 'Gini'), overfit = c('none'), cf=c(0.0))
Tree_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_Tree_multi, algorytm="Tree", seed = 123)
Tree_multi_CrossValid


print("Tree - reg")
parTune_Tree_reg <- expand.grid(depth=c(2:8), minobs=c(2:8), type=c('SS'), overfit = c('none'), cf=0.0)
Tree_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_Tree_reg, algorytm="Tree", seed = 123)
Tree_reg_CrossValid



# NN

print("Sieci NN - bin")
parTune_NN_bin <- expand.grid(h=list(c(3,5), c(4,6), c(2,7), c(3,4)), lr = c(0.01), iter = c(50000, 100000))
NN_bin_CrossValid <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 10, parTune_NN_bin, algorytm="NN", seed = 123)
NN_bin_CrossValid


print("Sieci NN - multi")
parTune_NN_multi <- expand.grid(h=list(c(3,5), c(4,6), c(2,7), c(3,4)), lr = c(0.01), iter = c(50000, 100000))
NN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 10, parTune_NN_multi, algorytm="NN", seed = 123)
NN_multi_CrossValid


print("Sieci NN - reg")
parTune_NN_reg <- expand.grid(h=list(c(3,5), c(4,6), c(2,7), c(3,4)), lr = c(0.01), iter = c(50000, 100000))
NN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 10, parTune_NN_reg, algorytm="NN", seed = 123)
NN_reg_CrossValid




cv_R <- trainControl(method="cv", number=10)

print("KNN - R - bin")
knn_grid_bin = expand.grid(k=2:50)
KNN_bin_R = train(x=bin_cancer[,bin_cancer_X], y=bin_cancer[,bin_cancer_Y], tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results

print("KNN - R - multi")
knn_grid_multi = expand.grid(k=2:50)
KNN_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=knn_grid_multi, method='knn', metric='Accuracy', trControl=cv_R)
KNN_multi_R_Wynik = KNN_multi_R$results

print("KNN - R - reg")
knn_grid_reg = expand.grid(k=2:50)
KNN_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=knn_grid_reg, method='knn', metric='MAE', trControl=cv_R)
KNN_reg_R_Wynik = KNN_reg_R$results


print("TREE - R - bin")
tree_grid_bin = expand.grid(maxdepth=2:15)
Tree_bin_R = train(x=bin_cancer[,bin_cancer_X], y=bin_cancer[,bin_cancer_Y], tuneGrid=tree_grid_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_bin_R_Wynik = Tree_bin_R$results

print("TREE - R - multi")
tree_grid_multi = expand.grid(maxdepth=2:15)
Tree_multi_R = train(x=dane_multi[,dane_multi_X], y=dane_multi[,dane_multi_Y], tuneGrid=tree_grid_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_multi_R_Wynik = Tree_multi_R$results

print("TREE - R - reg")
tree_grid_reg = expand.grid(maxdepth=2:15)
Tree_reg_R = train(x=dane_reg[,dane_reg_X], y=dane_reg[,dane_reg_Y], tuneGrid=tree_grid_reg, method='rpart2', metric='MAE', trControl=cv_R)
Tree_reg_R_Wynik = Tree_reg_R$results

print("Neural Network - R - bin")
bin_cancer_norm <- MinMax_nn(bin_cancer[,bin_cancer_X])
nn_grid_bin = expand.grid(size=3:15, decay = c(0.0001, 0.001))
NN_bin_R = train(x=bin_cancer_norm, y=bin_cancer[,bin_cancer_Y], tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results


print("Neural Network - R - multi")
dane_multi_norm <- MinMax_nn(dane_multi[,dane_multi_X])
nn_grid_multi = expand.grid(size=3:15, decay = c(0.0001, 0.001))
NN_multi_R = train(x=dane_multi_norm, y=dane_multi[,dane_multi_Y], tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results


print("Neural Network - R - reg")
dane_reg_norm <- MinMax_nn(dane_reg)
nn_grid_reg = expand.grid(size=3:15, decay = c(0.0001, 0.001))
NN_reg_R = train(x=dane_reg_norm[,dane_reg_X], y=dane_reg_norm[,dane_reg_Y], tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results







