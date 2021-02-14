#setwd("C:\\Users\\Dell\\Desktop\\Projekt_PG")

rm(list=ls())

source("funkcje.R")


#dane do klasyfikacji binarnej
df_bin <- read.csv("caesarian.csv",header=T, sep=",")
df_bin[,6] = as.factor(df_bin[,6])
X_nazwy_bin = colnames(df_bin)[1:5]

#X_bin = df_bin[,1:5]
class(df_bin)
class(df_bin[,6])


#dane do klasyfikacji wieloklasowej
df_multi <- read.csv("balance.csv",header=T, sep=",")

df_multi = as.data.frame(cbind(df_multi[,2:5], Class.Name = df_multi$Class.Name))
df_multi$Class.Name <- as.factor(as.numeric(df_multi$Class.Name))

X_nazwy_multi = colnames(df_multi)[1:4]

class(df_multi)
class(df_multi[,5])


#dane do regresji
df_reg <- read.csv("servo.csv",header=T, sep=",")

df_reg$motor <- as.numeric(df_reg$motor)
df_reg$screw <- as.numeric(df_reg$screw)

X_nazwy_reg = colnames(df_reg)[1:4]

class(df_reg)
class(df_reg[,5])

#X_reg = df_reg[,1:4]
#Y_reg = df_reg[,5]



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


######## Maszyna wektorow nosnych ######## - tylko klasyfikacja binarna

#svm_grid = expand.grid(C=1:10, lr=c(0.0001, 0.001, 0.01, 0.1))
#svm_crossval_results = CrossValidTune(dane_binary_scaled, Xnames_binary, 'Y', method='svm', k=5, param_grid=svm_grid)


### Szablon:
# trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 )
# predSVM <- function( X, theta, theta0 )


df_bin_norm <- as.data.frame(cbind(sapply(df_bin[,1:5],norm_0_1), Caesarian = df_bin$Caesarian))
df_multi_norm <- as.data.frame(cbind(sapply(df_multi[,1:4],norm_0_1), Class.Name = df_multi$Class.Name))
df_reg_norm <- as.data.frame(cbind(sapply(df_reg[,1:4],norm_0_1), class = df_reg$class))



#binarna
df_bin_sign <- ifelse( df_bin[,6] == 0, -1, df_bin[,6])
df_bin_sign <- ifelse( df_bin_sign == 2, 1, df_bin_sign)

SVM_model <- trainSVM(as.matrix(df_bin_norm[,1:5]), df_bin_sign, C=10, lr = 0.001, maxiter = 500)
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
NN_model_Bin <- trainNN( df_bin_norm[,1:5], df_bin_norm[,6], h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "binarna")
NN_predict_Bin <- predNN( df_bin_norm[,1:5], NN_model_Bin, typ = "binarna")


NN_model_Bin <- trainNN( df_multi_norm[,1:4], df_multi_norm[,5], h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "wieloklasowa")
NN_predict_Bin <- predNN( df_multi_norm[,1:4], NN_model_Bin, typ = "wieloklasowa")


NN_model_Bin <- trainNN( df_reg_norm[,1:4], as.numeric(df_reg_norm[,5]), h = c(5,5), lr = 0.01, iter = 10000, seed = 123, typ = "regresja")
NN_predict_Bin <- predNN( df_reg_norm[,1:4], NN_model_Bin, typ = "regresja")




