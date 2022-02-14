#bilbioteki

library(caret)
library(rpart) 
library(data.tree)
library(caTools)
library(rattle)
library(pROC)
library(e1071)
library(class)
library(pracma)
library(psych)
library(dplyr)
library(fastDummies)
source('funkcje.R')



#KLASYFIKACJA WIELOKLASOWA

#dane (obligatoryjny zbior danych)
f <- file("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", )
data_kl_w_org <- read.table(f, dec=" ",sep=",", header=F)
colnames(data_kl_w_org)<-c("buying","maint","persons","lug_boot","safety","Y")


#sprawdzenie brakujacych obserwacji 
sum(is.na(f))


data_kl_w_org <- data_kl_w_org %>% select(-buying)
data_kl_w_org<- data_kl_w_org %>% select(-maint)
data_kl_w_org <- data_kl_w_org %>% select(-persons)
data_kl_w_org <- data_kl_w_org %>% select(-lug_boot)
data_kl_w_org<- data_kl_w_org %>% select(-safety)



data_kl_w_org$buying<-as.character(as.numericr(data_kl_w_org$buying))
data_kl_w_org$maint<-as.character(as.numeric(data_kl_w_org$maint))
data_kl_w_org$persons<-as.character(as.numeric(data_kl_w_org$persons))
data_kl_w_org$lug_boot<-as.character(as.numeric(data_kl_w_org$lug_boot))
data_kl_w_org$safety<-as.character(as.numeric(data_kl_w_org$safety))


#normalizacja
data_kl_w_org$buying<-normalizacja(data_kl_w_org$buying)
data_kl_w_org$maint<-normalizacja(data_kl_w_org$maint)
data_kl_w_org$persons<-normalizacja(data_kl_w_org$persons)
data_kl_w_org$lug_boot<-normalizacja(data_kl_w_org$lug_boot)
data_kl_w_org$safety<-normalizacja(data_kl_w_org$safety)




#podzial orginalnego zbioru na zbior uczacy i testowy
data_kl_w<-data_kl_w_org

Y_kl_w <- data_kl_w %>% select(Y)
data_kl_w <- data_kl_w %>% select(-Y)


set.seed(123) 
smp_size1 <- floor(0.75 * nrow(data_kl_w))
sample1 <- sample(seq_len(nrow(data_kl_w)), size = smp_size1)

data_kl_w_Train <- data_kl_w[sample1, ]
data_kl_w_Pred <- data_kl_w[-sample1, ]

Y_kl_w_train <- Y_kl_w[sample1,, ]
Y_kl_w_pred <-Y_kl_w[-sample1,, ]


#KLASYFIKACJA BINARNA
#dane 

f2 <-file("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", open="r")
data_kl_b_org <- read.table(f2, sep=",", header=F)
colnames(data_kl_b_org)<-c("Variance","Skewness","Curtosis","Entropy","Y")

#normalizacja
data_kl_b_org$Variance<-normalizacja(data_kl_b_org$Variance)
data_kl_b_org$Skewness<-normalizacja(data_kl_b_org$Skewness)
data_kl_b_org$Curtosis<-normalizacja(data_kl_b_org$Curtosis)
data_kl_b_org$Entropy<-normalizacja(data_kl_b_org$Entropy)



#podzial orginalnego zbioru na zbior uczacy i testowy
data_kl_b<-data_kl_b_org
Y_kl_b <- data_kl_b %>% select(Y)
data_kl_b <- data_kl_b %>% select(-Y)


set.seed(123) 
smp_size2 <- floor(0.75 * nrow(data_kl_b))
sample2 <- sample(seq_len(nrow(data_kl_b)), size = smp_size2)

data_kl_b_Train <- data_kl_b[sample2, ]
data_kl_b_Pred <- data_kl_b[-sample2, ]

Y_kl_b_train <- Y_kl_b[sample2,, ]
Y_kl_b_pred <-Y_kl_b[-sample2,, ]


#REGRESJA
#dane
f3 <-file("https://archive.ics.uci.edu/ml/machine-learning-databases/servo/servo.data", open="r")
data_reg_org <- read.table(f3, dec=" ",sep=",", header=F)
colnames(data_reg_org)<-c("Motor","Screw","Pgain","Vgain","Y")

#stworzenie kolumn zero-jedynkowych dla zmiennych jakosciowych, usuniecie pierwszej kolumny 
data_reg_org<-dummy_cols(data_reg_org,select_columns=c("Motor","Screw"),remove_first_dummy = TRUE)
data_reg_org <- data_reg_org %>% select(-Motor)
data_reg_org <- data_reg_org %>% select(-Screw)

data_reg_org$Y<-as.numeric(as.character(data_reg_org$Y))

data_reg_org$Pgain<-normalizacja(data_reg_org$Pgain)
data_reg_org$Vgain<-normalizacja(data_reg_org$Vgain)
show(data_reg_org)

#podzial orginalnego zbioru na zbior uczacy i testowy

data_reg<- data_reg_org
Y_reg <- data_reg %>% select(Y)
data_reg <- data_reg %>% select(-Y)


set.seed(123) 
smp_size3 <- floor(0.75 * nrow(data_reg))
sample3 <- sample(seq_len(nrow(data_reg)), size = smp_size3)

data_reg_Train <- data_reg[sample3, ]
data_reg_Pred <- data_reg[-sample3, ]

Y_reg_train <- Y_reg[sample3,, ]
Y_reg_pred <-Y_reg[-sample3,, ]


#KNN PAKIET KLASYFIKACJA BINARNA

data_kl_b_org2 <- data_kl_b_org
data_kl_b_org2$Y <- as.character(data_kl_b_org$Y)

toControl<-trainControl(method = "cv", number = 10)

set.seed(123) 
train(Y  ~ ., method     = "knn",
      tuneGrid   = expand.grid(k = 1:10),
      trControl  = toControl,
      metric     = "Accuracy",
      data       = data_kl_b_org2)

set.seed(123) 
knn_klb_pakiet<-find_best_knn_kl_b_pakiet(data_kl_b_Train,Y_kl_b_train,data_kl_b_Pred,Y_kl_b_pred,10)
knn_klb_pakiet


plot(knn_klb_pakiet[,1], knn_klb_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of k",ylab = "KNN klasyfikacja binarna pakiet",ylim=range( c(knn_klb_pakiet[,2], knn_klb_pakiet[,3],knn_klb_pakiet[,4]) ))
lines(knn_klb_pakiet[,1], knn_klb_pakiet[,3], type = "b",col = "blue",pch = 19)
lines(knn_klb_pakiet[,1], knn_klb_pakiet[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

knn_klb_pakiet_tab<-table(knn(data_kl_b_Train,data_kl_b_Pred,cl=Y_kl_b_train,k=3),Y_kl_b_pred)
colnames(knn_klb_pakiet_tab)=c("Faktyczne 0","Faktyczne 1")
rownames(knn_klb_pakiet_tab)=c("Przewidywane 0","Przewidywane 1")
knn_klb_pakiet_tab

plot( roc( knn(data_kl_b_Train,data_kl_b_Pred,cl=Y_kl_b_train,k=3) ,Y_kl_b_pred ),legacy.axes = TRUE)
auc(knn(data_kl_b_Train,data_kl_b_Pred,cl=Y_kl_b_train,k=3),Y_kl_b_pred)

#KNN KOD WLASNY KLASYFIKACJA BINARNA

set.seed(123)
knn_klb_kod<-find_best_knn_kl_b_kod( data_kl_b_Train, Y_kl_b_train, data_kl_b_Pred,Y_kl_b_pred,3)
knn_klb_kod


plot(knn_klb_kod[,1], knn_klb_kod[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of k",ylab = "KNN klasyfikacja binarna kod wlasny",ylim=range( c(knn_klb_kod[,2], knn_klb_kod[,3],knn_klb_kod[,4]) ))
lines(knn_klb_kod[,1], knn_klb_kod[,3], type = "b",col = "blue",pch = 19)
lines(knn_klb_kod[,1], knn_klb_kod[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)


knn_klb_kod_3<-KKNpred_kl( KNNtrain( data_kl_b_Train, Y_kl_b_train, 3 ), data_kl_b_Pred )
knn_klb_kod_tab<-table(knn_klb_kod_3,Y_kl_b_pred)
colnames(knn_klb_kod_tab)=c("Faktyczne 0","Faktyczne 1")
rownames(knn_klb_kod_tab)=c("Przewidywane 0","Przewidywane 1")
knn_klb_kod_tab

plot( roc( knn_klb_kod_3 ,Y_kl_b_pred ),legacy.axes = TRUE)
auc(knn_klb_kod_3,Y_kl_b_pred)



#DRZEWA DECYZYJNE PAKIET KLASYFIKACJA BINARNA



set.seed(123) 
drzewko_klb_kod<-find_best_tree_kl_pakiet(data_kl_b_Train,Y_kl_b_train,data_kl_b_Pred,Y_kl_b_pred,10,cp=0)
drzewko_klb_kod
data_kl_b_org2 <- data_kl_b_org
data_kl_b_org2$Y <- as.character(data_kl_b_org$Y)

toControl<-trainControl(method = "cv", number = 10)

set.seed(123) 
train(Y  ~ ., 
      method     = "gbm",
      verbose = FALSE,
      metric     = "Accuracy",
      trControl  = toControl,
      data       = data_kl_b_org2)



plot(drzewko_klb_kod[,1], drzewko_klb_kod[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Max of Deph",ylab = "Drzewo decyzyjne klasyfikacja binarna pakiet",ylim=range( c(drzewko_klb_kod[,2], drzewko_klb_kod[,3],drzewko_klb_kod[,4]) ))
lines(drzewko_klb_kod[,1], drzewko_klb_kod[,3], type = "b",col = "blue",pch = 19)
lines(drzewko_klb_kod[,1], drzewko_klb_kod[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

set.seed(123) 
drzewko_klb_kod_cp<-find_best_tree_kl_pakiet(data_kl_b_Train,Y_kl_b_train,data_kl_b_Pred,Y_kl_b_pred,10,cp=0.05)
drzewko_klb_kod_cp

plot(drzewko_klb_kod_cp[,1], drzewko_klb_kod_cp[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Max of Deph",ylab = "Drzewo decyzyjne klasyfikacja binarna pakiet cp=0,05",ylim=range( c(drzewko_klb_kod_cp[,2], drzewko_klb_kod_cp[,3],drzewko_klb_kod_cp[,4]) ))
lines(drzewko_klb_kod_cp[,1], drzewko_klb_kod_cp[,3], type = "b",col = "blue",pch = 19)
lines(drzewko_klb_kod_cp[,1], drzewko_klb_kod_cp[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

drzewko_klb_kod_cp[3,]

d_kl_b_p<-rpart( formula = Y_kl_b_train~., data = data_kl_b_Train, minsplit = 2, maxdepth = 3, cp = 0.05, method="class" )

dd_klb_kod_tab<-table(predict(d_kl_b_p, data_kl_b_Pred, type = "class"),Y_kl_b_pred)
colnames(dd_klb_kod_tab)=c("Faktyczne 0","Faktyczne 1")
rownames(dd_klb_kod_tab)=c("Przewidywane 0","Przewidywane 1")
dd_klb_kod_tab

plot( roc( predict(d_kl_b_p, data_kl_b_Pred, type = "class"),Y_kl_b_pred  ),legacy.axes = TRUE )
auc(predict(d_kl_b_p, data_kl_b_Pred, type = "class"),Y_kl_b_pred)

fancyRpartPlot(d_kl_b_p)

#DRZEWA DECYZYJNE KOD WLASNY KLASYFIKACJA BINARNA

data_kl_b_Train_with_Y<-data_kl_b_Train
data_kl_b_Train_with_Y$Y=Y_kl_b_train


Drzewko_kl_b_kod_wl <- Tree( Y="Y", Xnames = c("Variance","Skewness","Curtosis","Entropy"), data = data_kl_b_Train_with_Y, depth = 3, minobs = 1)
print( Drzewko_kl_b_kod_wl , "Count", "Prob", "Leaf" )

Drzewko_kl_b_kod_wl_wyniki<- matrix(0,8,1)
Drzewko_kl_b_kod_wl_wyniki[1,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness <= 0.733643408532182`$`Curtosis <= 0.357559178926056`$Count
Drzewko_kl_b_kod_wl_wyniki[2,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness <= 0.733643408532182`$`Curtosis >  0.357559178926056`$Count * Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness <= 0.733643408532182`$`Curtosis >  0.357559178926056`$Prob[2]
Drzewko_kl_b_kod_wl_wyniki[3,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness >  0.733643408532182`$`Variance <= 0.208712834159041`$Count
Drzewko_kl_b_kod_wl_wyniki[4,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness >  0.733643408532182`$`Variance >  0.208712834159041`$Count
Drzewko_kl_b_kod_wl_wyniki[5,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis <= 0.0959657096086329`$Count
Drzewko_kl_b_kod_wl_wyniki[6,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis >  0.0959657096086329`$Count * Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis >  0.0959657096086329`$Prob[1]
Drzewko_kl_b_kod_wl_wyniki[7,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance >  0.636739285637021`$`Variance <= 0.680310667849339`$Count * Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance >  0.636739285637021`$`Variance <= 0.680310667849339`$Prob[1]
Drzewko_kl_b_kod_wl_wyniki[8,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance >  0.636739285637021`$`Variance >  0.680310667849339`$Count

Drzewko_kl_b_kod_wl_Trafnosc<- (sum(Drzewko_kl_b_kod_wl_wyniki[,1]) / Drzewko_kl_b_kod_wl$Count) * 100
Drzewko_kl_b_kod_wl_Trafnosc

PEP( Drzewko_kl_b_kod_wl, cf = 0.05 )
print(Drzewko_kl_b_kod_wl, "Count", "Prob", "Leaf" )

Drzewko_kl_b_kod_wl_wyniki_CP<-matrix(0,6,1)
Drzewko_kl_b_kod_wl_wyniki_CP[1,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness <= 0.733643408532182`$Count * Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness <= 0.733643408532182`$Prob[2]
Drzewko_kl_b_kod_wl_wyniki_CP[2,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness >  0.733643408532182`$`Variance <= 0.208712834159041`$Count
Drzewko_kl_b_kod_wl_wyniki_CP[3,1]<-Drzewko_kl_b_kod_wl$`Variance <= 0.529527868521443`$`Skewness >  0.733643408532182`$`Variance >  0.208712834159041`$Count
Drzewko_kl_b_kod_wl_wyniki_CP[4,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis <= 0.0959657096086329`$Count
Drzewko_kl_b_kod_wl_wyniki_CP[5,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis >  0.0959657096086329`$Count * Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance <= 0.636739285637021`$`Curtosis >  0.0959657096086329`$Prob[1]
Drzewko_kl_b_kod_wl_wyniki_CP[6,1]<-Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance >  0.636739285637021`$Count * Drzewko_kl_b_kod_wl$`Variance >  0.529527868521443`$`Variance >  0.636739285637021`$Prob[1]

Drzewko_kl_b_kod_wl_Trafnosc_CP<- (sum(Drzewko_kl_b_kod_wl_wyniki_CP[,1]) / Drzewko_kl_b_kod_wl$Count) * 100
Drzewko_kl_b_kod_wl_Trafnosc_CP


#MASZYNA WEKTOROW NOSNYCH PAKIET KLASYFIKACJA BINARNA


data_kl_b_org2 <- data_kl_b_org
data_kl_b_org2$Y <- as.character(data_kl_b_org$Y)

toControl<-trainControl(method = "cv", number = 10)


y_mwn_kl_b_Train <- ifelse( Y_kl_b_train == 0, -1, 1 )
y_mwn_kl_b_Pred <- ifelse( Y_kl_b_pred == 0, -1, 1 )

set.seed(123) 
svm_kl_b_pakiet<-find_best_knn_svm_pakiet(data_kl_b_Train,y_mwn_kl_b_Train,y_mwn_kl_b_Pred,y_mwn_kl_b_Pred,10)
svm_kl_b_pakiet



plot(svm_kl_b_pakiet[,1], svm_kl_b_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of C",ylab = "SVM klasyfikacja binarna pakiet",ylim=range( c(svm_kl_b_pakiet[,2], svm_kl_b_pakiet[,3],svm_kl_b_pakiet[,4]) ))
lines(svm_kl_b_pakiet[,1], svm_kl_b_pakiet[,3], type = "b",col = "blue",pch = 19)
lines(svm_kl_b_pakiet[,1], svm_kl_b_pakiet[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

svm_klb_pakiet_tab<-table(predict( svm( y = factor(y_mwn_kl_b_Train), x = data_kl_b_Train, kernel = "linear" ), data_kl_b_Pred  ),y_mwn_kl_b_Pred)
colnames(svm_klb_pakiet_tab)=c("Faktyczne 0","Faktyczne 1")
rownames(svm_klb_pakiet_tab)=c("Przewidywane 0","Przewidywane 1")
svm_klb_pakiet_tab

plot( roc( predict( svm( y = factor(y_mwn_kl_b_Train), x = data_kl_b_Train, kernel = "linear" ), data_kl_b_Pred  ),y_mwn_kl_b_Pred ),legacy.axes = TRUE)
auc(predict( svm( y = factor(y_mwn_kl_b_Train), x = data_kl_b_Train, kernel = "linear" ), data_kl_b_Pred  ),y_mwn_kl_b_Pred)


#MASZYNA WEKTOROW NOSNYCH KOD WLASNY KLASYFIKACJA BINARNA

y_mwn_kl_b_Train <- ifelse( Y_kl_b_train == 0, -1, 1 )
y_mwn_kl_b_Pred <- ifelse( Y_kl_b_pred == 0, -1, 1 )

set.seed(123) 
svm_kl_b_kod<-find_best_knn_svm_kod_wlasny(data_kl_b_Train,y_mwn_kl_b_Train,y_mwn_kl_b_Pred,y_mwn_kl_b_Pred,10)
svm_kl_b_kod



plot(svm_kl_b_kod[,1], svm_kl_b_kod[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of C",ylab = "SVM klasyfikacja binarna pakiet",ylim=range( c(svm_kl_b_kod[,2], svm_kl_b_kod[,3],svm_kl_b_kod[,4]) ))
lines(svm_kl_b_kod[,1], svm_kl_b_kod[,3], type = "b",col = "blue",pch = 19)
lines(svm_kl_b_kod[,1], svm_kl_b_kod[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("Trafnosc", "Czulosc","Specyficznosc"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

svm_kl_b_kodC<-trainSVM( as.matrix( data_kl_b_Train ), y_mwn_kl_b_Train, C = 7, lr = 0.001, maxiter = 5000 )
svm_kl_b_kodC_tab<-predSVM( as.matrix( data_kl_b_Pred ), svm_kl_b_kodC$Theta, svm_kl_b_kodC$Theta0 )

svm_klb_kodwlasny_tab<-table(svm_kl_b_kodC_tab,y_mwn_kl_b_Pred)
colnames(svm_klb_kodwlasny_tab)=c("Faktyczne 0","Faktyczne 1")
rownames(svm_klb_kodwlasny_tab)=c("Przewidywane 0","Przewidywane 1")
svm_klb_kodwlasny_tab

plot( roc( as.vector(svm_kl_b_kodC_tab),y_mwn_kl_b_Pred ),legacy.axes = TRUE)
auc(as.vector(svm_kl_b_kodC_tab),y_mwn_kl_b_Pred)



#KNN PAKIET KLASYFIKACJA WIELOKLASOWA



toControl<-trainControl(method = "cv", number = 10)

set.seed(123) 
train(Y  ~ ., method     = "knn",
      tuneGrid   = expand.grid(k = 1:10),
      trControl  = toControl,
      metric     = "Accuracy",
      data       = data_kl_w_org)



set.seed(123) 
knn_klw_pakiet<-find_best_knn_kl_w_pakiet(data_kl_w_Train,Y_kl_w_train,data_kl_w_Pred,Y_kl_w_pred,10)
knn_klw_pakiet


plot(knn_klw_pakiet[,1], knn_klw_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of k",ylab = "KNN klasyfikacja wieloklasowa pakiet",ylim=range( knn_klw_pakiet[,2]) )
legend("topleft", legend="Trafnosc",
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

set.seed(123)
knn_kl_w_pakiet_tab(data_kl_w_Train,Y_kl_w_train,data_kl_w_Pred,Y_kl_w_pred,3)


#KNN KOD KLASYFIKACJA WIELOKLASOWA


set.seed(123) 
kl_w_knn_kod<-KKNpred_kl( KNNtrain( data_kl_w_Train, Y_kl_w_train, 3 ), data_kl_w_Pred )
table(Y_kl_w_pred,kl_w_knn_kod)


#DRZEWO DECYZYJNE PAKIET KLASYFIKACJA WIELOKLASOWA



set.seed(123) 
drzewko_klw_pakiet<-find_best_tree_kl_W_pakiet(data_kl_w_Train,Y_kl_w_train,data_kl_w_Pred,Y_kl_w_pred,max_deph=10, cp=0)
drzewko_klw_pakiet

plot(drzewko_klw_pakiet[,1], drzewko_klw_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Max of Deph",ylab = "Drzewo decyzyjne klasyfikacja binarna pakiet",ylim=range( drzewko_klw_pakiet[,2]) )

legend("topleft", legend="Trafnosc",
       col=c("red"), lty = 1:2, cex=0.8)

set.seed(123) 
drzewko_klw_pakiet_cp<-find_best_tree_kl_W_pakiet(data_kl_w_Train,Y_kl_w_train,data_kl_w_Pred,Y_kl_w_pred,max_deph=10, cp=0.03)
drzewko_klw_pakiet_cp

plot(drzewko_klw_pakiet_cp[,1], drzewko_klw_pakiet_cp[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Max of Deph",ylab = "Drzewo decyzyjne klasyfikacja binarna pakiet CP=0,03",ylim=range( drzewko_klw_pakiet_cp[,2]) )

drzewko_klw_pakiet_cp[3,]

d_kl_w_p<-rpart( formula = Y_kl_w_train~., data = data_kl_w_Train, minsplit = 2, maxdepth = 3, cp = 0.03, method="class" )

dd_klw_pakiet_tab<-table(predict(d_kl_w_p, data_kl_w_Pred, type = "class"),Y_kl_w_pred)
dd_klw_pakiet_tab


fancyRpartPlot(d_kl_w_p)



#DRZEWO DECYZYJNE KOD WLASNY KLASYFIKACJA WIELOKLASOWA


data_kl_w_Train_with_Y<-data_kl_w_Train
data_kl_w_Train_with_Y$Y=Y_kl_w_train

set.seed(123) 
Drzewko_kl_w_kod_wl <- Tree( Y="Y", Xnames = c("mcg","gvh","aac","alm1","alm2"), data = data_kl_w_Train_with_Y, depth = 3, minobs = 1)
print( Drzewko_kl_w_kod_wl , "Class","Count","Leaf" )


Drzewko_kl_w_kod_wl_wyniki<- matrix(0,8,1)
Drzewko_kl_w_kod_wl_wyniki[1,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg <= 0.685393258426966`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg <= 0.685393258426966`$Prob[1]
Drzewko_kl_w_kod_wl_wyniki[2,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg >  0.685393258426966`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg >  0.685393258426966`$Prob[6]
Drzewko_kl_w_kod_wl_wyniki[3,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac <= 0.715909090909091`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac <= 0.715909090909091`$Prob[8]
Drzewko_kl_w_kod_wl_wyniki[4,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac >  0.715909090909091`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac >  0.715909090909091`$Prob[6]
Drzewko_kl_w_kod_wl_wyniki[5,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$`alm2 <= 0.525252525252525`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$`alm2 <= 0.525252525252525`$Prob[2]
Drzewko_kl_w_kod_wl_wyniki[6,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$`alm2 >  0.525252525252525`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$`alm2 >  0.525252525252525`$Prob[2]
Drzewko_kl_w_kod_wl_wyniki[7,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$`mcg <= 0.910112359550562`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$`mcg <= 0.910112359550562`$Prob[5]
Drzewko_kl_w_kod_wl_wyniki[8,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$`mcg >  0.910112359550562`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$`mcg >  0.910112359550562`$Prob[5]

Drzewko_kl_w_kod_wl_Trafnosc<- (sum(Drzewko_kl_w_kod_wl_wyniki[,1]) / Drzewko_kl_w_kod_wl$Count) * 100
Drzewko_kl_w_kod_wl_Trafnosc

PEP( Drzewko_kl_w_kod_wl, cf = 0.03 )
print( Drzewko_kl_w_kod_wl , "Class","Count","Leaf" )

Drzewko_kl_w_kod_wl_wyniki_CP<- matrix(0,6,1)
Drzewko_kl_w_kod_wl_wyniki_CP[1,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg <= 0.685393258426966`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg <= 0.685393258426966`$Prob[1]
Drzewko_kl_w_kod_wl_wyniki_CP[2,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg >  0.685393258426966`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh <= 0.5`$`mcg >  0.685393258426966`$Prob[6]
Drzewko_kl_w_kod_wl_wyniki_CP[3,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac <= 0.715909090909091`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac <= 0.715909090909091`$Prob[8]
Drzewko_kl_w_kod_wl_wyniki_CP[4,1]<-Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac >  0.715909090909091`$Count * Drzewko_kl_w_kod_wl$`alm1 <= 0.556701030927835`$`gvh >  0.5`$`aac >  0.715909090909091`$Prob[6]
Drzewko_kl_w_kod_wl_wyniki_CP[5,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg <= 0.831460674157303`$Prob[2]
Drzewko_kl_w_kod_wl_wyniki_CP[6,1]<-Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$Count * Drzewko_kl_w_kod_wl$`alm1 >  0.556701030927835`$`mcg >  0.831460674157303`$Prob[5]

Drzewko_kl_w_kod_wl_Trafnosc_CP<- (sum(Drzewko_kl_w_kod_wl_wyniki_CP[,1]) / Drzewko_kl_w_kod_wl$Count) * 100
Drzewko_kl_w_kod_wl_Trafnosc_CP


#KNN PAKIET REGRESJA

toControl<-trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
train(Y  ~ ., method     = "knn",
      tuneGrid   = expand.grid(k = 1:10),
      trControl  = toControl,
      data       = data_reg_org)

knn_reg_pakiet<-find_best_knn_reg_pakiet(data_reg_Train,Y_reg_train,data_reg_Pred,Y_reg_pred,10)
knn_reg_pakiet

plot(knn_reg_pakiet[,1], knn_reg_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of k",ylab = "KNN reg pakiet",ylim=range( c(knn_reg_pakiet[,2], knn_reg_pakiet[,3],knn_reg_pakiet[,4]) ))
lines(knn_reg_pakiet[,1], knn_reg_pakiet[,3], type = "b",col = "blue",pch = 19)
lines(knn_reg_pakiet[,1], knn_reg_pakiet[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("MAE", "MSE","MAPE"),
       col=c("red", "blue", "black"), lty = 1:2, cex=0.8)

knn_reg_pakiet[6,]

#KNN KOD WLASNY REGRESJA

set.seed(123)
knn_reg_kod<-find_best_knn_reg_kod(data_reg_Train,Y_reg_train,data_reg_Pred,Y_reg_pred,max_k=10)
knn_reg_kod

plot(knn_reg_kod[,1], knn_reg_kod[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Number of k",ylab = "KNN reg kod",ylim=range( c(knn_reg_kod[,2], knn_reg_kod[,3],knn_reg_kod[,4]) ))
lines(knn_reg_kod[,1], knn_reg_kod[,3], type = "b",col = "blue",pch = 19)
lines(knn_reg_kod[,1], knn_reg_kod[,4], type = "b",col = "black",pch = 19)
legend("topleft", legend=c("MAE", "MSE","MAPE"),
       col=c("red", "blue","black"), lty = 1:2, cex=0.8)

knn_reg_kod[8,]

#DRZEWA DECYZYJNE PAKIET REGRESJA

toControl<-trainControl(method = "cv", number = 10)

set.seed(123) 
train(Y  ~ ., 
      method     = "gbm",
      verbose = FALSE,
      trControl  = toControl,
      data       = data_reg_org)



plot(drzewo_reg_pakiet[,1], drzewo_reg_pakiet[,2], type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Max of Deph",ylab = "Drzewo reg pakiet",ylim=range( c(drzewo_reg_pakiet[,2], drzewo_reg_pakiet[,3],drzewo_reg_pakiet[,4]) ))
lines(drzewo_reg_pakiet[,1], drzewo_reg_pakiet[,3], type = "b",col = "blue",pch = 19)
lines(drzewo_reg_pakiet[,1], drzewo_reg_pakiet[,4], type = "b",col = "black",pch = 19)
legend("set.seed(123)
drzewo_reg_pakiet<-find_best_tree_reg_pakiet(data_reg_Train,Y_reg_train,data_reg_Pred,Y_reg_pred,10)
drzewo_reg_pakiet
topleft", legend=c("MAE", "MSE","MAPE"),
       col=c("red", "blue", "black"), lty = 1:2, cex=0.8)

drzewo_reg_pakiet[7,]

d_reg_p<-rpart( formula = Y_reg_train~., data = data_reg_Train, minsplit = 2, maxdepth = 7, method="anova")
fancyRpartPlot(d_reg_p)






#DRZEWA DECYZYJNE KOD WLASNY REGRESJA



data_reg_Train_with_Y<-data_reg_Train
data_reg_Train_with_Y$Y=Y_reg_train


set.seed(123) 
Drzewko_reg_kod_wl <- Tree( Y="Y", Xnames = c("Vgain"), data = data_reg_Train_with_Y, depth = 1, minobs = 1)
print( Drzewko_reg_kod_wl ,"Count","Leaf" )

