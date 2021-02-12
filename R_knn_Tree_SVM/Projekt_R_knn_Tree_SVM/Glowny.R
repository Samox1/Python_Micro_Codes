rm(list=ls())

library(caret)
library(rpart)
library(rpart.plot)
library(DiagrammeR)
library(nnet)
library(neuralnet)
library(kernlab)
library(e1071)
library(data.tree)
library(pROC)
library(ROCit)
library(tidyverse)
library(readxl)
library(ggplot2)


source("funkcje.R")

set.seed(125)

### --------------- ###
### --- Binarna --- ###
### --------------- ###
print("### --------------- ###")
print("### --- Binarna --- ###")
print("### --------------- ###")

# Dane: Transfusion.csv
# Zmienne są następujące:
#R. Recency - miesiące od ostatniej darowizny
#F. Częstotliwość - całkowita liczba darowizn
#M. Monetary - całkowita oddana krew w cm3 (ml)
#T. Czas - miesiące od pierwszej darowizny
#y. zmienna binarna określająca, czy oddał krew w marcu 2007 r. (1 = tak; 0 = nie)

Transfusion_Bin <- as.data.frame(read.csv(file="http://archive.ics.uci.edu/ml/machine-learning-databases/blood-transfusion/transfusion.data"))
colnames(Transfusion_Bin) <- c("Recency", "Frequency","Monetary","Time","Y_out")
print(paste("Jakies wartosci ANY = " , any(is.na(Transfusion_Bin))))
Transfusion_Bin_Y <- Transfusion_Bin$Y_out
Transfusion_Bin$Y_out <- factor(Transfusion_Bin$Y_out)

# Krotki przeglad danych
print("Ilosc danych wejsciowych: ")
print(length(Transfusion_Bin[,1]))
print(summary(Transfusion_Bin))


training.samples <- Transfusion_Bin$Y_out %>% createDataPartition(p = 0.7, list = FALSE)
train.data  <- Transfusion_Bin[training.samples, ]
test.data <- Transfusion_Bin[-training.samples, ]


# --- Drzewko Binarne - rpart --- #
cat("\n")
print("### --- Tree - rpart --- ###")
# Drzewko_Bin_rpart = rpart( formula = Y_out~., data = train.data, minsplit = 1, maxdepth = 5)
# # rpart.plot(Drzewko_Bin_rpart, type = 1, extra = 1)
# pred_Drzewko_Bin_rpart_class <- predict(Drzewko_Bin_rpart, newdata = test.data, type="class")
# pred_Drzewko_Bin_rpart <- predict(Drzewko_Bin_rpart, newdata = test.data, type="prob")[,2]
# ROCit_Drzewko_Bin_rpart <- rocit(score=pred_Drzewko_Bin_rpart, class = test.data$Y_out)
# summary(ROCit_Drzewko_Bin_rpart)
# plot(ROCit_Drzewko_Bin_rpart)


print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5))

minsplit_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_minsplit_range = 1:50
for (minsplit in tree_minsplit_range) {
  #print(minsplit)
  #print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=minsplit, tree_maxdepth=7))
  minsplit_test <- rbind(minsplit_test, Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=minsplit, tree_maxdepth=5))
}
minsplit_test_Bin <- cbind(tree_minsplit_range, minsplit_test[-1,])
### WYKRES ###
minsplit_test_Bin <- as.data.frame(minsplit_test_Bin)
minsplit_test_Bin <- drop_na(minsplit_test_Bin)

p = ggplot() + geom_line(data = minsplit_test_Bin, aes(x = tree_minsplit_range, y = ACC_mean), color = "blue") + xlab('Minimum Split') + ylab('Accuracy') + ggtitle("Drzewo Decyzyjne - Binarna - Algorytm biblioteki: rpart")
print(p)

p = ggplot() + geom_line(data = minsplit_test_Bin, aes(x = tree_minsplit_range, y = AUC_mean), color = "blue") + xlab('Minimum Split') + ylab('AUC') + ggtitle("Drzewo Decyzyjne - Binarna - Algorytm biblioteki: rpart")
print(p)


maxdepth_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_maxdepth_range = 1:15
for (maxdep in tree_maxdepth_range) {
  #print(maxdep)
  #print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=maxdep))
  maxdepth_test <- rbind(maxdepth_test,Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=maxdep))
}
maxdepth_test_Bin <- cbind(tree_maxdepth_range, maxdepth_test[-1,])
### WYKRES ###
maxdepth_test_Bin <- as.data.frame(maxdepth_test_Bin)
maxdepth_test_Bin <- drop_na(maxdepth_test_Bin)

p = ggplot() + geom_line(data = maxdepth_test_Bin, aes(x = tree_maxdepth_range, y = ACC_mean), color = "blue") + xlab('Maximum Depth') + ylab('Accuracy') + ggtitle("Drzewo Decyzyjne - Binarna - Algorytm biblioteki: rpart")
print(p)

p = ggplot() + geom_line(data = maxdepth_test_Bin, aes(x = tree_maxdepth_range, y = AUC_mean), color = "blue") + xlab('Maximum Depth') + ylab('AUC') + ggtitle("Drzewo Decyzyjne - Binarna - Algorytm biblioteki: rpart")
print(p)


# --- Drzewko Binarne - reczne --- #
cat("\n")
print("### --- Tree - reczne --- ###")
Drzewko_Bin <- Tree( Y = "Y_out", Xnames = c("Recency", "Frequency","Monetary","Time"), data = Transfusion_Bin, depth = 5, minobs = 25)

plot(Drzewko_Bin)

Drzewko_Bin_Vis <- ToDataFrameTree(Drzewko_Bin)
print("Drzewo Decyzyjne z najlepszymi parametrami dla Drzewa z biblioteki rpart")
print(Drzewko_Bin_Vis)



for (x in 1:(ncol(Transfusion_Bin)-1)) {
  Transfusion_Bin[,x] = norm_minmax(Transfusion_Bin[,x])
}
print(summary(Transfusion_Bin))



# --- knn - reczne --- #
cat("\n")
print("### --- knn - reczne --- ###")
# knn_model_Bin <- KNNtrain(Transfusion_Bin[-5], Transfusion_Bin_Y, k=5, 0, 1)
# pred_knn_Bin <- KNNpred(knn_model_Bin, Transfusion_Bin[-5])
# print(ModelOcena(Transfusion_Bin$Y_out, pred_knn_Bin))

print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="my", knn_k = 10))

knn_k_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="my", knn_k = 10)
knn_k_range = 2:25
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="my", knn_k = k_knn))
}
knn_k_test_Bin_my <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Bin_my <- as.data.frame(knn_k_test_Bin_my)
knn_k_test_Bin_my <- drop_na(knn_k_test_Bin_my)

p = ggplot() + geom_line(data = knn_k_test_Bin_my, aes(x = knn_k_range, y = ACC_mean), color = "blue") + xlab('k (dla knn)') + ylab('Accuracy') + ggtitle("knn - binarny - Algorytm wlasny")
print(p)

p = ggplot() + geom_line(data = knn_k_test_Bin_my, aes(x = knn_k_range, y = AUC_mean), color = "blue") + xlab('k (dla knn)') + ylab('AUC') + ggtitle("knn - binarny - Algorytm wlasny")
print(p)

# --- knn - caret --- #
cat("\n")
print("### --- knn - caret --- ###")
# knn_model_Bin_caret <- knn3(Y_out ~ . , data = Transfusion_Bin, k=5)
# pred_knn_model_Bin_caret <- predict(knn_model_Bin_caret, Transfusion_Bin, type="prob")[,2]
# print(ModelOcena(Transfusion_Bin$Y_out, pred_knn_model_Bin_caret))
# ROCit_knn_Bin_caret <- rocit(score=pred_knn_model_Bin_caret, class = Transfusion_Bin$Y_out)

print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="R", knn_k = 10))

knn_k_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="R", knn_k = 10)
knn_k_range = 2:100
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="knn", algorytm="R", knn_k = k_knn))
}
knn_k_test_Bin_R <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Bin_R <- as.data.frame(knn_k_test_Bin_R)
knn_k_test_Bin_R <- drop_na(knn_k_test_Bin_R)

p = ggplot() + geom_line(data = knn_k_test_Bin_R, aes(x = knn_k_range, y = ACC_mean), color = "blue") + xlab('k (dla knn)') + ylab('Accuracy') + ggtitle("knn - binarny - Algorytm biblioteki: caret")
print(p)

p = ggplot() + geom_line(data = knn_k_test_Bin_R, aes(x = knn_k_range, y = AUC_mean), color = "blue") + xlab('k (dla knn)') + ylab('AUC') + ggtitle("knn - binarny - Algorytm biblioteki: caret")
print(p)


# --- SVM - reczne --- #
cat("\n")
print("### --- SVM - reczne --- ###")
# Transfusion_Bin_Y_class <- ifelse( Transfusion_Bin_Y == 0, -1, Transfusion_Bin_Y )
# SVM_model_Bin <- trainSVM( as.matrix(Transfusion_Bin[-5]), Transfusion_Bin_Y_class, C = 100, lr = 0.001, maxiter = 500 )
# pred_SVM_model_Bin <- predSVM( as.matrix(Transfusion_Bin[-5]), SVM_model_Bin$Theta, SVM_model_Bin$Theta0)
# print(ModelOcena_Class(as.vector(pred_SVM_model_Bin),Transfusion_Bin$Y_out))

print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="my", svm_cost=100, svm_lr = 0.001, svm_maxiter = 1000))

svm_C_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="my", svm_cost=100, svm_lr = 0.001, svm_maxiter = 1000)
svm_C_range = 20:200
for (C_svm in svm_C_range) {
  svm_C_test <- rbind(svm_C_test,Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="my", svm_cost=C_svm, svm_lr = 0.001, svm_maxiter = 1000))
}
svm_C_test_Bin_my <- cbind(svm_C_range, svm_C_test[-1,])
### WYKRES ###
svm_C_test_Bin_my <- as.data.frame(svm_C_test_Bin_my)
svm_C_test_Bin_my <- drop_na(svm_C_test_Bin_my)

p = ggplot() + geom_line(data = svm_C_test_Bin_my, aes(x = svm_C_range, y = ACC_mean), color = "blue") + xlab('C (koszt)') + ylab('Accuracy') + ggtitle("SVM - Binarna - Algorytm wlasny")
print(p)

p = ggplot() + geom_line(data = svm_C_test_Bin_my, aes(x = svm_C_range, y = AUC_mean), color = "blue") + xlab('C (koszt)') + ylab('AUC') + ggtitle("SVM - Binarna - Algorytm wlasny")
print(p)

# --- SVM - e1071 --- #
cat("\n")
print("### --- SVM - e1071 --- ###")
# SVM_model_Bin_e1071 <- svm(Y_out ~ . , data = Transfusion_Bin, probability = TRUE)
# pred_SVM_model_Bin_e1071 <- predict(SVM_model_Bin_e1071, Transfusion_Bin, probability = TRUE)
# prob_SVM_model_Bin_e1071 <- attr(pred_SVM_model_Bin_e1071, "probabilities")[,1]
# print(ModelOcena( Transfusion_Bin$Y_out, prob_SVM_model_Bin_e1071))
# print(ModelOcena_Class(pred_SVM_model_Bin_e1071, Transfusion_Bin$Y_out))

print(Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="R", svm_cost=100))

svm_C_test <- Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="R", svm_cost=100, svm_lr = 0.001, svm_maxiter = 1000)
svm_C_range = 20:200
for (C_svm in svm_C_range) {
  svm_C_test <- rbind(svm_C_test,Krosswalidacja_param(Dane = Transfusion_Bin, Dane_Y=Transfusion_Bin$Y_out, Dane_Y_Y=Transfusion_Bin_Y, k_folds=5, typ_danych="bin", model="svm", algorytm="R", svm_cost=C_svm, svm_lr = 0.001, svm_maxiter = 1000))
}
svm_C_test_Bin_R <- cbind(svm_C_range, svm_C_test[-1,])
### WYKRES ###
svm_C_test_Bin_R <- as.data.frame(svm_C_test_Bin_R)

p = ggplot() + geom_line(data = svm_C_test_Bin_R, aes(x = svm_C_range, y = ACC_mean), color = "blue") + xlab('C (koszt)') + ylab('Accuracy') + ggtitle("SVM - Binarna - Algorytm biblioteki: e1071")
print(p)

p = ggplot() + geom_line(data = svm_C_test_Bin_R, aes(x = svm_C_range, y = AUC_mean), color = "blue") + xlab('C (koszt)') + ylab('AUC') + ggtitle("SVM - Binarna - Algorytm biblioteki: e1071")
print(p)


# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #
# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #
# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #

### -------------------- ###
### --- Wieloklasowa --- ###
### -------------------- ###
print("### -------------------- ###")
print("### --- Wieloklasowa --- ###")
print("### -------------------- ###")

Dermatology <- as.data.frame(read.csv(file="http://archive.ics.uci.edu/ml/machine-learning-databases/dermatology/dermatology.data", header = FALSE))
# 35 kolumn - klasy w 35 kolumnie
# brak 8 sztuk w Age (kolumna 34) - zamiast sa "?"
# kolumna 34 wciagana jest jako "factor"

print(paste("Jakies wartosci ANY = " , any(is.na(Dermatology))))
Dermatology_wiek <- as.numeric(Dermatology[!(Dermatology[,34] == "?"),34])
wiek_mean <- round(mean(Dermatology_wiek),0)
Dermatology[,34] <- ifelse(Dermatology[,34] == "?", wiek_mean, Dermatology[,34] )

Dermatology_Y <- Dermatology$V35
Dermatology$V35 <- factor(Dermatology$V35)


# -->  przydalaby sie funkcja na losowanie danych treningowych z roznych klas

#training.samples <- Dermatology[,"V35"] %>% createDataPartition(p = 0.7, list = FALSE)
#train.data  <- Dermatology[training.samples, ]
#test.data <- Dermatology[-training.samples, ]


# --- Drzewko Wieloklasowe - rpart --- #
cat("\n")
print("### --- Tree - rpart --- ###")
# Drzewko_Class_rpart = rpart( formula = V35 ~. , data = train.data, minsplit = 1, maxdepth = 5, method = "class")
# #rpart.plot(Drzewko_Class_rpart, type = 1, extra = 1)
# pred_Drzewko_Class_rpart_class <- predict(Drzewko_Class_rpart, newdata = test.data, type="class")
# pred_Drzewko_Class_rpart <- predict(Drzewko_Class_rpart, newdata = test.data, type="prob")
# print(paste("Trafione klasy = ", Traf <- length(test.data[test.data$V35 == pred_Drzewko_Class_rpart_class,35]), " / ", All <- length(test.data$V35), " = ", Traf/All))

print(Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5))

minsplit_test <- Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_minsplit_range = 1:50
for (minsplit in tree_minsplit_range) {
  minsplit_test <- rbind(minsplit_test, Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="tree", algorytm="R", tree_minsplit=minsplit, tree_maxdepth=5))
}
minsplit_test_Class <- cbind(tree_minsplit_range, minsplit_test[-1,])
### WYKRES ###
minsplit_test_Class <- as.data.frame(minsplit_test_Class)
p = ggplot() + geom_line(data = minsplit_test_Class, aes(x = tree_minsplit_range, y = ACC), color = "blue") + xlab('Minimum Split') + ylab('Accuracy') + ggtitle("Drzewo Decyzyjne - Wieloklasowa - Algorytm biblioteki: rpart")
print(p)


maxdepth_test <- Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_maxdepth_range = 1:15
for (maxdep in tree_maxdepth_range) {
  maxdepth_test <- rbind(maxdepth_test,Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=maxdep))
}
maxdepth_test_Class <- cbind(tree_maxdepth_range, maxdepth_test[-1,])
### WYKRES ###
maxdepth_test_Class <- as.data.frame(maxdepth_test_Class)
p = ggplot() + geom_line(data = maxdepth_test_Class, aes(x = tree_maxdepth_range, y = ACC), color = "blue") + xlab('Maximum Depth') + ylab('Accuracy') + ggtitle("Drzewo Decyzyjne - Wieloklasowa - Algorytm biblioteki: rpart")
print(p)



# --- Drzewko Wieloklasowe - reczne --- #
cat("\n")
print("### --- Tree - reczne --- ###")
Drzewko_Class <- Tree( Y = "V35", Xnames = colnames(Dermatology)[-35], data = Dermatology, depth = 8, minobs = 48)
plot(Drzewko_Class)
Drzewko_Class_Vis <- ToDataFrameTree(Drzewko_Class)
print("Drzewo Decyzyjne z najlepszymi parametrami dla Drzewa z biblioteki rpart")
print(Drzewko_Class_Vis)


for (x in 1:(ncol(Dermatology)-1)) {
 Dermatology[,x] = norm_minmax(Dermatology[,x])
}



# --- knn - reczne --- #
cat("\n")
print("### --- knn - reczne --- ###")
# knn_model_Class <- KNNtrain(train.data[-35], train.data$V35, k=3, 0, 1)
# pred_knn_Class <- KNNpred(knn_model_Class, test.data[-35])
# print(paste("Trafione klasy = ", Traf <- length(test.data[test.data$V35 == pred_knn_Class$Klasa,35]), " / ", All <- length(test.data$V35), " = ", Traf/All))

print(Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="my", knn_k = 5))

knn_k_test <- Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="my", knn_k = 10)
knn_k_range = 2:25
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="my", knn_k = k_knn))
}
knn_k_test_Class_my <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Class_my <- as.data.frame(knn_k_test_Class_my)
p = ggplot() + geom_line(data = knn_k_test_Class_my, aes(x = knn_k_range, y = ACC), color = "blue") + xlab('k (dla knn)') + ylab('Accuracy') + ggtitle("knn - Wieloklasowa - Algorytm wlasny")
print(p)

# --- knn - caret --- #
cat("\n")
# print("### --- knn - caret --- ###")
# knn_model_Class_caret <- knn3(formula = V35 ~ . , data = train.data, k = 2)   # <--- KNN cos nie dziala 
# pred_knn_model_Class_caret <- predict(knn_model_Class_caret, test.data, type="class")
# print(paste("Trafione klasy = ", Traf <- length(test.data[test.data$V35 == pred_knn_model_Class_caret,35]), " / ", All <- length(test.data$V35), " = ", Traf/All))

print(Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="R", knn_k = 5))

knn_k_test <- Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="R", knn_k = 10)
knn_k_range = 2:100
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Dermatology, Dane_Y=Dermatology$V35, Dane_Y_Y=Dermatology_Y, k_folds=5, typ_danych="class", model="knn", algorytm="R", knn_k = k_knn))
}
knn_k_test_Class_R <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Class_R <- as.data.frame(knn_k_test_Class_R)
p = ggplot() + geom_line(data = knn_k_test_Class_R, aes(x = knn_k_range, y = ACC), color = "blue") + xlab('k (dla knn)') + ylab('Accuracy') + ggtitle("knn - Wieloklasowa - Algorytm biblioteki: caret")
print(p)


# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #
# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #
# ------------------------------------------- ### ---------------------------------------- ### --------------------------------------- ### --------------------------- #

### ---------------- ###
### --- REGRESJA --- ###
### ---------------- ###
print("### ---------------- ###")
print("### --- Regresja --- ###")
print("### ---------------- ###")

# url_concrete <- "http://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"
Concrete <- as.data.frame(read_xls("Concrete_Data.xls"))
colnames(Concrete)<-c("Cement","Zuzel","Popiol","Woda","Superplastyfikator","Krusz_grube","Krusz_drobne","Wiek","Wytrzymalosc")
print(summary(Concrete))


#training.samples <- Concrete[,9] %>% createDataPartition(p = 0.7, list = FALSE)
#train.data  <- Concrete[training.samples, ]
#test.data <- Concrete[-training.samples, ]


# --- Drzewko Regresja - rpart --- #
cat("\n")
print("### --- Tree - rpart --- ###")
# Drzewko_Reg_rpart = rpart( formula = Wytrzymalosc ~ . , data = train.data, method = "anova")
# #rpart.plot(Drzewko_Reg_rpart, type = 1, extra = 1)
# pred_Drzewko_Reg_rpart <- as.numeric(predict(Drzewko_Reg_rpart, newdata = test.data))
# print(ModelOcena( test.data$Wytrzymalosc , pred_Drzewko_Reg_rpart))

print(Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="tree", algorytm="R", tree_minsplit=10, tree_maxdepth=20))


minsplit_test <- Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_minsplit_range = 1:50
for (minsplit in tree_minsplit_range) {
  minsplit_test <- rbind(minsplit_test, Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="tree", algorytm="R", tree_minsplit=minsplit, tree_maxdepth=5))
}
minsplit_test_Reg <- cbind(tree_minsplit_range, minsplit_test[-1,])
### WYKRES ###
minsplit_test_Reg <- as.data.frame(minsplit_test_Reg)
p = ggplot() + geom_line(data = minsplit_test_Reg, aes(x = tree_minsplit_range, y = MAPE_mean), color = "blue") + xlab('Minimum Split') + ylab('MAPE') + ggtitle("Drzewo Decyzyjne - Regresja - Algorytm biblioteki: rpart")
print(p)

maxdepth_test <- Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=5)
tree_maxdepth_range = 1:15
for (maxdep in tree_maxdepth_range) {
  maxdepth_test <- rbind(maxdepth_test,Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="tree", algorytm="R", tree_minsplit=25, tree_maxdepth=maxdep))
}
maxdepth_test_Reg <- cbind(tree_maxdepth_range, maxdepth_test[-1,])
### WYKRES ###
maxdepth_test_Reg <- as.data.frame(maxdepth_test_Reg)
p = ggplot() + geom_line(data = maxdepth_test_Reg, aes(x = tree_maxdepth_range, y = MAPE_mean), color = "blue") + xlab('Maximum Depth') + ylab('MAPE') + ggtitle("Drzewo Decyzyjne - Regresja - Algorytm biblioteki: rpart")
print(p)


# --- Drzewko Regresja - reczne --- #
cat("\n")
print("### --- Tree - reczne --- ###")
Drzewko_Reg <- Tree( Y = "Wytrzymalosc", Xnames = colnames(Concrete)[-9], data = Concrete, depth = 10, minobs = 15)
Drzewko_Reg_Vis <- ToDataFrameTree(Drzewko_Reg)
print("Drzewo Decyzyjne z najlepszymi parametrami dla Drzewa z biblioteki rpart")
print(Drzewko_Reg_Vis)



for (x in 1:(ncol(Concrete)-1)) {
  Concrete[,x] = norm_minmax(Concrete[,x])
}


# --- knn - reczne --- #
cat("\n")
print("### --- knn - reczne --- ###")
# knn_model_Reg <- KNNtrain(train.data[,-9], train.data$Wytrzymalosc, k=3, 0, 1)
# pred_knn_Reg <- KNNpred(knn_model_Reg, test.data[,-9])
# print(ModelOcena( test.data$Wytrzymalosc , pred_knn_Reg))

print(Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="my", knn_k = 5))

knn_k_test <- Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="my", knn_k = 10)
knn_k_range = 2:25
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="my", knn_k = k_knn))
}
knn_k_test_Reg_my <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Reg_my <- as.data.frame(knn_k_test_Reg_my)
p = ggplot() + geom_line(data = knn_k_test_Reg_my, aes(x = knn_k_range, y = MAPE_mean), color = "blue") + xlab('k (dla knn)') + ylab('MAPE') + ggtitle("knn - Regresja - Algorytm wlasny")
print(p)


# --- knn - caret --- #
cat("\n")
print("### --- knn - caret --- ###")
# knn_model_Reg_caret <- knn3(Wytrzymalosc ~ . , data = train.data, k=5)
# pred_knn_model_Reg_caret <- predict(knn_model_Reg_caret, test.data)
# print(ModelOcena(test.data$Wytrzymalosc, pred_knn_model_Reg_caret))

print(Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="R", knn_k = 5))

knn_k_test <- Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="R", knn_k = 10)
knn_k_range = 2:100
for (k_knn in knn_k_range) {
  knn_k_test <- rbind(knn_k_test,Krosswalidacja_param(Dane = Concrete, Dane_Y=Concrete$Wytrzymalosc, Dane_Y_Y=NULL, k_folds=5, typ_danych="reg", model="knn", algorytm="R", knn_k = k_knn))
}
knn_k_test_Reg_R <- cbind(knn_k_range, knn_k_test[-1,])
### WYKRES ###
knn_k_test_Reg_R <- as.data.frame(knn_k_test_Reg_R)
p = ggplot() + geom_line(data = knn_k_test_Reg_R, aes(x = knn_k_range, y = MAPE_mean), color = "blue") + xlab('k (dla knn)') + ylab('MAPE') + ggtitle("knn - Regresja - Algorytm biblioteki: caret")
print(p)



