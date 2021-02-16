#setwd("C:\\Users\\Dell\\Desktop\\Projekt_PG")

rm(list=ls())

source("funkcje.R")


#dane do klasyfikacji binarnej

df_bin <- read.csv("caesarian.csv",header=T, sep=",")
df_bin[,6] = as.factor(df_bin[,6])
df_bin_original <- df_bin
X_nazwy_bin = colnames(df_bin)[1:5]
Y_nazwy_bin = colnames(df_bin)[6]

#X_bin = df_bin[,1:5]
class(df_bin)
class(df_bin[,6])


#dane do klasyfikacji wieloklasowej
df_multi <- read.csv("balance.csv",header=T, sep=",")

df_multi = as.data.frame(cbind(df_multi[,2:5], Class.Name = df_multi$Class.Name))
df_multi$Class.Name <- as.factor(as.numeric(df_multi$Class.Name))
df_multi_original <- df_multi

X_nazwy_multi = colnames(df_multi)[1:4]
Y_nazwy_multi = colnames(df_multi)[5]

class(df_multi)
class(df_multi[,5])


#dane do regresji
df_reg <- read.csv("servo.csv",header=T, sep=",")

df_reg$motor <- as.numeric(df_reg$motor)
df_reg$screw <- as.numeric(df_reg$screw)
df_reg_original <- df_reg

X_nazwy_reg = colnames(df_reg)[1:4]
Y_nazwy_reg = colnames(df_reg)[5]

class(df_reg)
class(df_reg[,5])

#X_reg = df_reg[,1:4]
#Y_reg = df_reg[,5]

df_bin_norm <- as.data.frame(cbind(sapply(df_bin[,1:5],MinMax), Caesarian = df_bin$Caesarian))
df_multi_norm <- as.data.frame(cbind(sapply(df_multi[,1:4],MinMax), Class.Name = df_multi$Class.Name))
df_reg_norm <- as.data.frame(cbind(sapply(df_reg[,1:4],MinMax), class = df_reg$class))



######## Drzewa decyzyjne ########

### Szablon
#Tree <- function(Y, X, data, type, depth, minobs, overfit, cf)
  
#binarna
Drzewko_bin <- Tree("Caesarian", X_nazwy_bin, data=df_bin, type='Gini', depth=2, minobs=2, overfit='none', cf=0.001)

#wieloklasowa
Drzewko_multi <- Tree("Class.Name", X_nazwy_multi, data=df_multi, type='Gini', depth=6, minobs=2, overfit='none', cf=0.001)        


#regresja - 
Drzewko_reg <- Tree("class", X_nazwy_reg, data=df_reg, type='SS', depth=9, minobs=2, overfit='none', cf=0.001)



######## K najbliższych sąsiadów ########

### Szablon:
# KNNmodel <- KNNtrain( X, y_tar, k = 5, 0,1 )
# KNNpredict_1 <- KNNpred(KNNmodel, X)

#binarna
X <- df_bin[,1:5]
y_tar <- df_bin[,6]
KNNmodel <- KNNtrain( X, y_tar, k = 5, 0,1 )
KNN_predict_Bin <- KNNpred(KNNmodel, X)
KNN_Ocena_Bin <- ModelOcena(y_tar, as.numeric(KNN_predict_Bin[,1]))
print("KNN - Ocena modelu na calym zbiorze: BINARNY")
print(KNN_Ocena_Bin[[3]])
print("--------------------------------------------------")


#wieloklasowa
X <- df_multi[,1:4]
y_tar <- df_multi[,5]
KNNmodel <- KNNtrain( X, y_tar, k = 5, 0,1 )
KNN_predict_Multi <- KNNpred(KNNmodel, X)
KNN_Ocena_Multi <- ModelOcena_Jakosc(y_tar, as.numeric(KNN_predict_Multi$Klasa))                                                       
print("KNN - Ocena modelu na calym zbiorze: WIELOKLASOWY")
print(KNN_Ocena_Multi)
print("--------------------------------------------------")


#regresja
X <- df_reg[,1:4]
y_tar <- df_reg[,5]
KNNmodel <- KNNtrain( X, y_tar, k = 5, 0,1 )
KNN_predict_Reg <- KNNpred(KNNmodel, X)
KNN_Ocena_Reg <- ModelOcena(y_tar, KNN_predict_Reg)
print("KNN - Ocena modelu na calym zbiorze: REGRESJA")
print(KNN_Ocena_Reg)
print("--------------------------------------------------")
print("--------------------------------------------------")
print("--------------------------------------------------")
print(" ")


######## Maszyna wektorow nosnych ######## - tylko klasyfikacja binarna

#svm_grid = expand.grid(C=1:10, lr=c(0.0001, 0.001, 0.01, 0.1))
#svm_crossval_results = CrossValidTune(dane_binary_scaled, Xnames_binary, 'Y', method='svm', k=5, param_grid=svm_grid)


### Szablon:
# trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 )
# predSVM <- function( X, theta, theta0 )


#binarna
df_bin_sign <- ifelse( df_bin[,6] == 0, -1, 1)

SVM_model <- trainSVM(as.matrix(df_bin_norm[,1:5]), df_bin_sign, C=100, lr = 0.001, maxiter = 500)
SVM_predict_Bin <- predSVM(as.matrix(df_bin_norm[,1:5]), SVM_model$Theta, SVM_model$Theta0)
SVM_Ocena_Bin <- ModelOcena_Jakosc(as.factor(df_bin_sign), SVM_predict_Bin)                                                          
print("SVM - Ocena modelu na calym zbiorze: BINARNY")
print(SVM_Ocena_Bin)
print("--------------------------------------------------")



######## Sieci neuronowe ########

### Szablon:
# trainNN <- function( x, y_tar, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "binarna" / "wieloklasowa" / "regresja")
# predNN <- function( xnew, nn, typ = "binarna" / "wieloklasowa" / "regresja" )


#binarna
print("Neural Network - OLD - Ocena modelu na calym zbiorze: BINARNY")

X = as.matrix(df_bin_norm[,1:5])
Y = as.matrix(MinMax(df_bin_norm[,6]))

# NN_model_Bin <- trainNN( X, Y, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "binarna", f_aktywacji = sigmoid, df_aktywacji = dsigmoid)
# NN_predict_Bin <- predNN( X, NN_model_Bin, typ = "binarna", f_aktywacji = sigmoid)
# print(NN_predict_Bin)
# ModelOcena((df_bin[,6]), as.numeric(NN_predict_Bin))

NN_model_Bin_old <- trainNN_old( X, Y, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "binarna")
NN_predict_Bin_old <- predNN_old( X, NN_model_Bin_old, typ = "binarna")
# print(NN_predict_Bin_old)
# print("Neural Network - Ocena modelu na calym zbiorze: BINARNY")
print(ModelOcena((df_bin[,6]), as.numeric(NN_predict_Bin_old))[[3]])
print("--------------------------------------------------")


#wieloklasowa
# print("Neural Network - NEW - Ocena modelu na calym zbiorze: WIELOKLASOWY")

# X = as.matrix(df_multi_norm[,1:4])
# Y = model.matrix( ~ df_multi[,5] - 1, df_multi )

# NN_model_Multi <- trainNN( X, Y, h = c(5,5), lr = 0.01, iter = 100000, seed = 123, typ = "wieloklasowa",  f_aktywacji = sigmoid, df_aktywacji = dsigmoid)
# NN_predict_Multi <- predNN( X, NN_model_Multi, typ = "wieloklasowa", f_aktywacji = sigmoid)

# print(NN_predict_Multi)
# ModelOcena_Jakosc((df_multi[,5]), as.numeric(NN_predict_Multi))

# etykiety <- levels( df_multi[,5] )
# y_hatTrain <- as.numeric( etykiety[apply( NN_predict_Multi, 1, which.max )] )

# print(ModelOcena_Jakosc((df_multi[,5]), y_hatTrain))
# print("--------------------------------------------------")



print("Neural Network - OLD - Ocena modelu na calym zbiorze: WIELOKLASOWY")
NN_model_Multi_old <- trainNN_old( X, Y, h = c(5,5), lr = 0.01, iter = 80000, seed = 123, typ = "wieloklasowa")
NN_predict_Multi_old <- predNN_old( X, NN_model_Multi_old, typ = "wieloklasowa")
# print(NN_predict_Multi_old)
etykiety <- levels( df_multi[,5] )
y_hatTrain <- as.numeric( etykiety[apply( NN_predict_Multi_old, 1, which.max )] )

print(ModelOcena_Jakosc((df_multi[,5]), y_hatTrain))
print("--------------------------------------------------")
# NN_predict_Multi_old <- cbind(NN_predict_Multi_old, y_hatTrain)


#regresja
print("Neural Network - OLD - Ocena modelu na calym zbiorze: REGRESJA")

X = as.matrix(df_reg_norm[,1:4])
Y = as.matrix(MinMax(df_reg_norm[,5]))
Y_min = min(Y)
Y_max = max(Y)

# NN_model_Reg <- trainNN( X, as.numeric(Y), h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "regresja", f_aktywacji = sigmoid, df_aktywacji = dsigmoid)
# NN_predict_Reg <- predNN( X, NN_model_Reg, typ = "regresja", f_aktywacji = sigmoid)
# print(NN_predict_Reg)
# print(ModelOcena((df_reg[,5]), NN_predict_Reg))

NN_model_Reg_old <- trainNN_old( X, Y, h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "regresja")
NN_predict_Reg_old <- predNN_old( X, NN_model_Reg_old, typ = "regresja")
NN_predict_Reg_old_Scale <- MinMaxOdwrot(NN_predict_Reg_old, Y_min, Y_max)

print(ModelOcena((df_reg_norm[,5]), NN_predict_Reg_old_Scale))
print("--------------------------------------------------")





### Cross-Validation ###

### Szablon:
# CrossValidTune(dane, X, y, kFold, parTune, seed, algorytm = "KNN" / "SVM" / "sieci")


### knn - CV ###
# knn_grid_bin = expand.grid(k=2:20)

knn_grid_bin = expand.grid(k=2:20)
KNN_CV_bin = CrossValidTune(df_bin_original, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = knn_grid_bin, seed = 152, algorytm = "KNN")

knn_grid_multi = expand.grid(k=2:20)
KNN_CV_multi = CrossValidTune(df_multi_original, X_nazwy_multi, Y_nazwy_multi, kFold = 10, parTune = knn_grid_multi, seed = 152, algorytm = "KNN")

knn_grid_reg = expand.grid(k=2:20)
KNN_CV_reg = CrossValidTune(df_reg_original, X_nazwy_reg, Y_nazwy_reg, kFold = 5, parTune = knn_grid_multi, seed = 152, algorytm = "KNN")


### SVM - CV ###
# svm_grid_bin = expand.grid(C=5:100, lr = c(0.01, 0.001, 0.0001), maxiter = 500)

svm_grid_bin = expand.grid(C=c(2:100), lr = c(0.01,0.001,0.0001), maxiter = c(500))
SVM_CV_bin = CrossValidTune(df_bin, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = svm_grid_bin, seed = 152, algorytm = "SVM")


### Sieci-NN - CV ###
# nn_grid_bin = expand.grid(h=c(5,5), lr = c(0.01, 0.001, 0.0001), iter = 10000)

# h_list = expand.grid(2:6,2:6)
# nn_grid_bin = expand.grid(h=list(data.frame(2:6,5)), lr = c(0.001), iter = 20000)

nn_grid_bin = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6)), lr = c(0.001), iter = c(20000, 50000))
NN_CV_bin = CrossValidTune(df_bin, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = nn_grid_bin, seed = 152, algorytm = "sieci")


nn_grid_multi = expand.grid(h=list(data.frame(5,5)), lr = c(0.001), iter = c(20000, 50000, 80000))
NN_CV_multi = CrossValidTune(df_multi, X_nazwy_multi, Y_nazwy_multi, kFold = 5, parTune = nn_grid_multi, seed = 152, algorytm = "sieci")


nn_grid_reg = expand.grid(h=list(data.frame(5,5)), lr = c(0.001), iter = c(20000, 50000, 100000))
NN_CV_reg = CrossValidTune(df_reg, X_nazwy_reg, Y_nazwy_reg, kFold = 5, parTune = nn_grid_reg, seed = 152, algorytm = "sieci")



