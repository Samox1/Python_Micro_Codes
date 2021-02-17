#setwd("C:\\Users\\Dell\\Desktop\\Projekt_PG")

rm(list=ls())

library(rpart.plot)
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

print("Neural Network - OLD - Ocena modelu na calym zbiorze: WIELOKLASOWY")
X = as.matrix(df_multi_norm[,1:4])
Y = model.matrix( ~ df_multi[,5] - 1, df_multi )

NN_model_Multi_old <- trainNN_old( X, Y, h = c(5,5), lr = 0.01, iter = 80000, seed = 123, typ = "wieloklasowa")
NN_predict_Multi_old <- predNN_old( X, NN_model_Multi_old, typ = "wieloklasowa")

klasy <- levels( df_multi[,5] )
NN_pred_Klasy <- as.numeric( klasy[apply( NN_predict_Multi_old, 1, which.max )] )

print(ModelOcena_Jakosc((df_multi[,5]), NN_pred_Klasy))
print("--------------------------------------------------")



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

NN_model_Reg_old <- trainNN_old( X, Y, h = c(5,5), lr = 0.01, iter = 50000, seed = 123, typ = "regresja")
NN_predict_Reg_old <- predNN_old( X, NN_model_Reg_old, typ = "regresja")
NN_predict_Reg_old_Scale <- MinMaxOdwrot(NN_predict_Reg_old, Y_min, Y_max)

print(ModelOcena((df_reg_norm[,5]), NN_predict_Reg_old_Scale))
print("--------------------------------------------------")





### Cross-Validation ###

### Szablon:
# CrossValidTune(dane, X, y, kFold, parTune, seed, algorytm = "KNN" / "SVM" / "sieci")


### knn - CV ###
# knn_grid_bin = expand.grid(k=2:20)

knn_grid_bin = expand.grid(k=2:50)
KNN_CV_bin = CrossValidTune(df_bin_original, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = knn_grid_bin, seed = 152, algorytm = "KNN")
#write.csv(KNN_CV_bin, "KNN_CV_bin.csv", row.names = FALSE)
print("KNN - Binarny - Cross-Validation - Wyniki:")
print(KNN_CV_bin[which.max(KNN_CV_bin$Jakosc_TRAIN),])
print(KNN_CV_bin[which.max(KNN_CV_bin$Jakosc_TEST),])
print("Najlepsze KNN dla zbioru: ")
print("*** Treningowy --> Jakosc = 0.775 : k = 2")
print("*** Testowy --> Jakosc = 0.8125 : k = 11")

ggplot(KNN_CV_bin , aes(x=k)) +
  geom_line(aes(y = Jakosc_TRAIN, color='blue'), size=1, ) +
  geom_line(aes(y = Jakosc_TEST, color='red'), size=1,) +
  labs(title='KNN: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Zbiór", labels = c("Treningowy", "Testowy")) +
  theme(legend.position = "bottom")



knn_grid_multi = expand.grid(k=2:50)
KNN_CV_multi = CrossValidTune(df_multi_original, X_nazwy_multi, Y_nazwy_multi, kFold = 10, parTune = knn_grid_multi, seed = 152, algorytm = "KNN")
#write.csv(KNN_CV_multi, "KNN_CV_multi.csv", row.names = FALSE)
print("KNN - Wieloklasowy - Cross-Validation - Wyniki:")
print(KNN_CV_multi[which.max(KNN_CV_multi$Jakosc_TRAIN),])
print(KNN_CV_multi[which.max(KNN_CV_multi$Jakosc_TEST),])
print("Najlepsze KNN dla zbioru: ")
print("*** Treningowy --> Jakosc = 0.9166 : k = 2")
print("*** Testowy --> Jakosc = 0.9194 : k = 42")


ggplot(KNN_CV_multi , aes(x=k)) +
  geom_line(aes(y = Jakosc_TRAIN, color='blue'), size=1, ) +
  geom_line(aes(y = Jakosc_TEST, color='red'), size=1,) +
  labs(title='KNN: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Zbiór", labels = c("Treningowy", "Testowy")) +
  theme(legend.position = "bottom")



knn_grid_reg = expand.grid(k=2:50)
KNN_CV_reg = CrossValidTune(df_reg_original, X_nazwy_reg, Y_nazwy_reg, kFold = 5, parTune = knn_grid_multi, seed = 152, algorytm = "KNN")
#write.csv(KNN_CV_reg, "KNN_CV_reg.csv", row.names = FALSE)
print("KNN - Regresja - Cross-Validation - Wyniki:")
print(KNN_CV_reg[which.min(KNN_CV_reg$MAPE_TRAIN),])
print(KNN_CV_reg[which.min(KNN_CV_reg$MAPE_TEST),])
print("Najlepsze KNN dla zbioru: ")
print("*** Treningowy --> MAPE = 0.1666 : k = 2")
print("*** Testowy --> MAPE = 0.2782 : k = 2")


ggplot(KNN_CV_reg , aes(x=k)) +
  geom_line(aes(y = MAE_TRAIN, color='blue'), size=1, ) +
  geom_line(aes(y = MAE_TEST, color='red'), size=1,) +
  labs(title='KNN: MAE od k', x='k', y='MAE') +
  scale_color_discrete(name = "Zbiór", labels = c("Treningowy", "Testowy")) +
  theme(legend.position = "bottom")



### SVM - CV ###
# svm_grid_bin = expand.grid(C=5:100, lr = c(0.01, 0.001, 0.0001), maxiter = 500)

#svm_grid_bin = expand.grid(C=c(2:200), lr = c(0.01,0.001,0.0001), maxiter = c(500, 1000, 5000))
svm_grid_bin = expand.grid(C=c(2:200), lr = c(0.001), maxiter = c(1000))
SVM_CV_bin = CrossValidTune(df_bin, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = svm_grid_bin, seed = 152, algorytm = "SVM")
#write.csv(SVM_CV_bin, "SVM_CV_bin.csv", row.names = FALSE)
print("SVM - Binarny - Cross-Validation - Wyniki:")
print(SVM_CV_bin[which.max(SVM_CV_bin$Jakosc_TRAIN),])
print(SVM_CV_bin[which.max(SVM_CV_bin$Jakosc_TEST),])
print("Najlepsze SVM dla zbioru: ")
print("*** Treningowy --> Jakosc = 0.7688 : Cost = 196 || lr = 0.0001 || maxiter = 5000")
print("*** Testowy --> Jakosc = 0.7625 : Cost = 30 || lr = 0.001 || maxiter = 500")

ggplot(SVM_CV_bin[1:100,] , aes(x=C)) +
  geom_line(aes(y = Jakosc_TRAIN, color='blue'), size=1, ) +
  geom_line(aes(y = Jakosc_TEST, color='red'), size=1,) +
  labs(title='SVM: Jakosc od C (kosztu)', x='C', y='Accuracy') +
  scale_color_discrete(name = "Zbiór", labels = c("Treningowy", "Testowy")) +
  theme(legend.position = "bottom")



### Sieci-NN - CV ###
# nn_grid_bin = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6)), lr = c(0.01, 0.001, 0.0001), iter = 10000)

# nn_grid_bin = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(10000, 20000, 50000, 80000, 100000))
nn_grid_bin = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(50000))
NN_CV_bin = CrossValidTune(df_bin, X_nazwy_bin, Y_nazwy_bin, kFold = 5, parTune = nn_grid_bin, seed = 152, algorytm = "sieci")
# h_bin = (NN_CV_bin[,1])
# h_table = data.frame()
# for (ii in 1:length(h_bin)) {
#   h_table[ii,"h1"] = h_bin[[ii]][1]
#   h_table[ii,"h2"] = h_bin[[ii]][2]
# }
# NN_CV_bin_cbind = cbind(h_table,NN_CV_bin[,2:length(NN_CV_bin[1,])])
# write.csv(NN_CV_bin_cbind, "NN_CV_bin.csv", row.names = FALSE)
print("Neural Network - Binarny - Cross-Validation - Wyniki:")
print(NN_CV_bin[which.max(NN_CV_bin$Jakosc_TRAIN),])
print(NN_CV_bin[which.max(NN_CV_bin$Jakosc_TEST),])
print("Najlepsze NN dla zbioru: ")
print("*** Treningowy --> Jakosc = 0.60 : h = (6,6) || lr = 0.001 || maxiter = 10000")
print("*** Testowy --> Jakosc = 0.66 : h = (4,4) || lr = 0.001 || maxiter = 10000")
print(NN_CV_bin)

# nn_grid_multi = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(10000, 20000, 50000, 80000, 100000))
nn_grid_multi = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(50000))
NN_CV_multi = CrossValidTune(df_multi, X_nazwy_multi, Y_nazwy_multi, kFold = 5, parTune = nn_grid_multi, seed = 152, algorytm = "sieci")
# h_bin = (NN_CV_multi[,1])
# h_table = data.frame()
# for (ii in 1:length(h_bin)) {
#   h_table[ii,"h1"] = h_bin[[ii]][1]
#   h_table[ii,"h2"] = h_bin[[ii]][2]
# }
# NN_CV_multi_cbind = cbind(h_table,NN_CV_multi[,2:length(NN_CV_multi[1,])])
# write.csv(NN_CV_multi_cbind, "NN_CV_multi.csv", row.names = FALSE)
print("Neural Network - Wieloklasowy - Cross-Validation - Wyniki:")
print(NN_CV_multi[which.max(NN_CV_multi$Jakosc_TRAIN),])
print(NN_CV_multi[which.max(NN_CV_multi$Jakosc_TEST),])
print("Najlepsze NN dla zbioru: ")
print("*** Treningowy --> Jakosc = 0.8796 : h = (5,5) || lr = 0.001 || maxiter = 100000")
print("*** Testowy --> Jakosc = 0.4736 : h = (6,6) || lr = 0.001 || maxiter = 80000")
print(NN_CV_multi)

# nn_grid_reg = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(10000, 20000, 50000, 80000, 100000))
nn_grid_reg = expand.grid(h=list(data.frame(4,4), data.frame(5,5), data.frame(6,6), data.frame(7,7), data.frame(8,8)), lr = c(0.001), iter = c(50000))
NN_CV_reg = CrossValidTune(df_reg, X_nazwy_reg, Y_nazwy_reg, kFold = 5, parTune = nn_grid_reg, seed = 152, algorytm = "sieci")
# h_bin = (NN_CV_reg[,1])
# h_table = data.frame()
# for (ii in 1:length(h_bin)) {
#   h_table[ii,"h1"] = h_bin[[ii]][1]
#   h_table[ii,"h2"] = h_bin[[ii]][2]
# }
# NN_CV_reg_cbind = cbind(h_table,NN_CV_reg[,2:length(NN_CV_reg[1,])])
# write.csv(NN_CV_reg_cbind, "NN_CV_reg.csv", row.names = FALSE)
print("Neural Network - Wieloklasowy - Cross-Validation - Wyniki:")
print(NN_CV_reg[which.min(NN_CV_reg$MAPE_TRAIN),])
print(NN_CV_reg[which.min(NN_CV_reg$MAPE_TEST),])
print("Najlepsze NN dla zbioru: ")
print("*** Treningowy --> MAPE = 0.3314 : h = (8,8) || lr = 0.001 || maxiter = 100000")
print("*** Testowy --> MAPE = 0.26669 : h = (8,8) || lr = 0.001 || maxiter = 100000")
print(NN_CV_reg)



### Funkcje wbudowane w biblioteki R ###
cat("\n")
print("*** Modele z bibliotek R ***")
cat("\n")

cv_R <- trainControl(method="cv", number=10)


print("KNN - R")

knn_grid_bin = expand.grid(k=2:50)
KNN_bin_R = train(x=df_bin_norm[,X_nazwy_bin], y=as.factor(df_bin_norm[,Y_nazwy_bin]), tuneGrid=knn_grid_bin, method='knn', metric='Accuracy', trControl=cv_R)
KNN_bin_R_Wynik = KNN_bin_R$results
print(paste("Najlepszy KNN w R - Binarny: k = ", KNN_bin_R$finalModel$k, " | Accuracy = " ,KNN_bin_R_Wynik$Accuracy[KNN_bin_R_Wynik$k == KNN_bin_R$finalModel$k]))

Porownanie_KNN_Bin <- data.frame(cbind(k = KNN_CV_bin$k[1:40], KNN_W = KNN_CV_bin$Jakosc_TEST[1:40], KNN_R = KNN_bin_R_Wynik$Accuracy[1:40]))

ggplot(Porownanie_KNN_Bin , aes(x=k)) +
  geom_line(aes(y = KNN_W, color='blue'), size=1, ) +
  geom_line(aes(y = KNN_R, color='red'), size=1,) +
  labs(title='KNN: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")



knn_grid_multi = expand.grid(k=2:50)
KNN_multi_R = train(x=df_multi_norm[,X_nazwy_multi], y=as.factor(df_multi_norm[,Y_nazwy_multi]), tuneGrid=knn_grid_multi, method='knn', metric='Accuracy', trControl=cv_R)
KNN_multi_R_Wynik = KNN_multi_R$results
print(paste("Najlepszy KNN w R - Wieloklasowy: k = ", KNN_multi_R$finalModel$k, " | Accuracy = " ,KNN_multi_R_Wynik$Accuracy[KNN_multi_R_Wynik$k == KNN_multi_R$finalModel$k]))

Porownanie_KNN_Multi <- data.frame(cbind(k = KNN_CV_multi$k[1:40], KNN_W = KNN_CV_multi$Jakosc_TEST[1:40], KNN_R = KNN_multi_R_Wynik$Accuracy[1:40]))

ggplot(Porownanie_KNN_Multi , aes(x=k)) +
  geom_line(aes(y = KNN_W, color='blue'), size=1, ) +
  geom_line(aes(y = KNN_R, color='red'), size=1,) +
  labs(title='KNN: Accuracy od k', x='k', y='Accuracy') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")



knn_grid_reg = expand.grid(k=2:50)
KNN_reg_R = train(x=df_reg_norm[,X_nazwy_reg], y=(df_reg_norm[,Y_nazwy_reg]), tuneGrid=knn_grid_reg, method='knn', metric='MAE', trControl=cv_R)
KNN_reg_R_Wynik = KNN_reg_R$results
print(paste("Najlepszy KNN w R - Regresja: k = ", KNN_reg_R$finalModel$k, " | MAE = " ,KNN_reg_R_Wynik$MAE[KNN_reg_R_Wynik$k == KNN_reg_R$finalModel$k]))

Porownanie_KNN_Reg <- data.frame(cbind(k = KNN_CV_reg$k[1:40], KNN_W = KNN_CV_reg$MAE_TEST[1:40], KNN_R = KNN_reg_R_Wynik$MAE[1:40]))

ggplot(Porownanie_KNN_Reg , aes(x=k)) +
  geom_line(aes(y = KNN_W, color='blue'), size=1, ) +
  geom_line(aes(y = KNN_R, color='red'), size=1,) +
  labs(title='KNN: MAE od k', x='k', y='MAE') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")



print("SVM - R")

svm_grid_bin = expand.grid(C=c(2:200))
SVM_bin_R <- train(x=df_bin_norm[,X_nazwy_bin], y=as.factor(df_bin_norm[,Y_nazwy_bin]), tuneGrid=svm_grid_bin, method='svmLinear', metric='Accuracy', trControl=cv_R)
SVM_bin_R_Wynik = SVM_bin_R$results
print(paste("Najlepszy SVM w R - Binarny: Cost = ", SVM_bin_R[["finalModel"]]@param[["C"]], " | Accuracy = " , SVM_bin_R_Wynik$Accuracy[SVM_bin_R_Wynik$C == SVM_bin_R[["finalModel"]]@param[["C"]]]))

Porownanie_SVM_Bin <- data.frame(cbind(C = SVM_CV_bin$C[1:100], SVM_W = SVM_CV_bin$Jakosc_TEST[1:100], SVM_R = SVM_bin_R_Wynik$Accuracy[1:100]))

ggplot(Porownanie_SVM_Bin , aes(x=C)) +
  geom_line(aes(y = SVM_W, color='blue'), size=1, ) +
  geom_line(aes(y = SVM_R, color='red'), size=1,) +
  labs(title='SVM Porownanie: Accuracy od C', x='C', y='Accuracy') +
  scale_color_discrete(name = "Implementacja", labels = c("Wlasna", "Biblioteka R")) +
  theme(legend.position = "bottom")




print("Neural Network - R")

nn_grid_bin = expand.grid(size=4:10, decay=0.00001)
NN_bin_R = train(x=df_bin_norm[,X_nazwy_bin], y=as.factor(df_bin_norm[,Y_nazwy_bin]), tuneGrid=nn_grid_bin, method='nnet', metric='Accuracy', trControl=cv_R)
NN_bin_R_Wynik = NN_bin_R$results
print(paste("Najlepszy NN w R - Binarny: h = ", NN_bin_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_bin_R_Wynik$Accuracy[NN_bin_R_Wynik$size == NN_bin_R[["finalModel"]][["tuneValue"]][["size"]]]))

ggplot(NN_bin_R_Wynik , aes(x=size)) +
  geom_line(aes(y = Accuracy), size=1, color="blue") +
  labs(title='NNET: Accuracy w zaleznosci od liczby neuronow - Binarny', x='h', y='Accuracy')


nn_grid_multi = expand.grid(size=4:10, decay=0.00001)
NN_multi_R = train(x=df_multi_norm[,X_nazwy_multi], y=as.factor(df_multi_norm[,Y_nazwy_multi]), tuneGrid=nn_grid_multi, method='nnet', metric='Accuracy', trControl=cv_R)
NN_multi_R_Wynik = NN_multi_R$results
print(paste("Najlepszy NN w R - Wieloklasowy: h = ", NN_multi_R[["finalModel"]][["tuneValue"]][["size"]], " | Accuracy = " , NN_multi_R_Wynik$Accuracy[NN_multi_R_Wynik$size == NN_multi_R[["finalModel"]][["tuneValue"]][["size"]]]))

ggplot(NN_multi_R_Wynik , aes(x=size)) +
  geom_line(aes(y = Accuracy), size=1, color="blue") +
  labs(title='NNET: Accuracy w zaleznosci od liczby neuronow - Wieloklasowy', x='h', y='Accuracy')


nn_grid_reg = expand.grid(size=4:10, decay = 0.00001)
NN_reg_R = train(x=df_reg_norm[,X_nazwy_reg], y=(df_reg[,Y_nazwy_reg]), tuneGrid=nn_grid_reg, method='nnet', metric='MAE', trControl=cv_R)
NN_reg_R_Wynik = NN_reg_R$results
print(paste("Najlepszy NN w R - Regresja: h = ", NN_reg_R[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , NN_reg_R_Wynik$MAE[NN_reg_R_Wynik$size == NN_reg_R[["finalModel"]][["tuneValue"]][["size"]]]))

ggplot(NN_reg_R_Wynik , aes(x=size)) +
  geom_line(aes(y = MAE), size=1, color="blue") +
  labs(title='NNET: MAE w zaleznosci od liczby neuronow - Regresja', x='h', y='MAE')



print("Tree - R")

tree_grid_bin = expand.grid(maxdepth=2:15)
Tree_bin_R = train(x=df_bin[,X_nazwy_bin], y=as.factor(df_bin_norm[,Y_nazwy_bin]), tuneGrid=tree_grid_bin, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_bin_R_Wynik = Tree_bin_R$results
print(paste("Najlepszy Tree w R - Binarny: Max Depth = ", Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_bin_R_Wynik$Accuracy[Tree_bin_R_Wynik$maxdepth == Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

ggplot(Tree_bin_R_Wynik , aes(x=maxdepth)) +
  geom_line(aes(y = Accuracy), size=1, color='blue') +
  labs(title='RPART: Zaleznosc pomiedzy jakoscia a glebokoscia - Binarna', x='Max Depth', y='Accuracy')+
  theme(legend.position = "bottom")

rpart.plot(Tree_bin_R$finalModel)

tree_grid_multi = expand.grid(maxdepth=2:15)
Tree_multi_R = train(x=df_multi[,X_nazwy_multi], y=as.factor(df_multi_norm[,Y_nazwy_multi]), tuneGrid=tree_grid_multi, method='rpart2', metric='Accuracy', trControl=cv_R)
Tree_multi_R_Wynik = Tree_multi_R$results
print(paste("Najlepszy Tree w R - Wieloklasowy: Max Depth = ", Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Accuracy = " , Tree_multi_R_Wynik$Accuracy[Tree_multi_R_Wynik$maxdepth == Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

ggplot(Tree_multi_R_Wynik , aes(x=maxdepth)) +
  geom_line(aes(y = Accuracy), size=1, color='blue') +
  labs(title='RPART: Zaleznosc pomiedzy jakoscia a glebokoscia - Wieloklasowa', x='Max Depth', y='Accuracy')+
  theme(legend.position = "bottom")

rpart.plot(Tree_multi_R$finalModel)

tree_grid_reg = expand.grid(maxdepth=2:15)
Tree_reg_R = train(x=df_reg[,X_nazwy_reg], y=as.numeric(df_reg_norm[,Y_nazwy_reg]), tuneGrid=tree_grid_reg, method='rpart2', metric='MAE', trControl=cv_R)
Tree_reg_R_Wynik = Tree_reg_R$results
print(paste("Najlepszy Tree w R - Regresja: Max Depth = ", Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , Tree_reg_R_Wynik$MAE[Tree_reg_R_Wynik$maxdepth == Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]]]))

ggplot(Tree_reg_R_Wynik , aes(x=maxdepth)) +
  geom_line(aes(y = MAE), size=1, color='blue') +
  labs(title='RPART: Zaleznosc pomiedzy jakoscia a glebokoscia - Regresja', x='Max Depth', y='MAE')+
  theme(legend.position = "bottom")

rpart.plot(Tree_reg_R$finalModel)


######## Drzewa decyzyjne - Wlasna Implementacja ########

### Szablon
#Tree <- function(Y, X, data, type, depth, minobs, overfit, cf)

#Binarna
print("Wlasna Implementacja - Drzewa Decyzyjne - Diagramy")
cat("\n")
print("Drzewa Decyzyjne - Binarny")
Drzewko_bin <- Tree(Y_nazwy_bin, X_nazwy_bin, data=df_bin, type='Gini', depth=Tree_bin_R[["finalModel"]][["tuneValue"]][["maxdepth"]], minobs=2, overfit='none', cf=0.001)
plot(Drzewko_bin)
print(Drzewko_bin)

#Wieloklasowa
print("Drzewa Decyzyjne - Wieloklasowy")
Drzewko_multi <- Tree(Y_nazwy_multi, X_nazwy_multi, data=df_multi, type='Gini', depth=Tree_multi_R[["finalModel"]][["tuneValue"]][["maxdepth"]], minobs=2, overfit='none', cf=0.001)        
plot(Drzewko_multi)
print(Drzewko_multi)

#Regresja
print("Drzewa Decyzyjne - Regresja")
Drzewko_reg <- Tree(Y_nazwy_reg, X_nazwy_reg, data=df_reg, type='SS', depth=Tree_reg_R[["finalModel"]][["tuneValue"]][["maxdepth"]], minobs=2, overfit='none', cf=0.001)
plot(Drzewko_reg)
print(Drzewko_reg)



