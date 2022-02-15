library(caret)
library(rpart) 
library(pROC)
library(e1071)
library(class)
library(dplyr)
source("funkcje_SZYBKIE.R")


# Klasyfikacja Binarna - Banknote Authentication
data_klasyfikacja_binarna <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", header = FALSE)
summary(data_klasyfikacja_binarna)
data_klasyfikacja_binarna[,5] <- as.factor(data_klasyfikacja_binarna[,5])
data_klasyfikacja_binarna_X <- colnames(data_klasyfikacja_wieloklasowa)[-5]
data_klasyfikacja_binarna_Y <- 'V5'
summary(data_klasyfikacja_binarna)


# Klasyfikacja Wieloklasowa - Car
data_klasyfikacja_wieloklasowa <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", header = FALSE)
summary(data_klasyfikacja_wieloklasowa)
data_klasyfikacja_wieloklasowa[,1] <- as.numeric(data_klasyfikacja_wieloklasowa[,1])
data_klasyfikacja_wieloklasowa[,2] <- as.numeric(data_klasyfikacja_wieloklasowa[,2])
data_klasyfikacja_wieloklasowa[,3] <- as.numeric(data_klasyfikacja_wieloklasowa[,3])
data_klasyfikacja_wieloklasowa[,4] <- as.numeric(data_klasyfikacja_wieloklasowa[,4])
data_klasyfikacja_wieloklasowa[,5] <- as.numeric(data_klasyfikacja_wieloklasowa[,5])
data_klasyfikacja_wieloklasowa[,6] <- as.numeric(data_klasyfikacja_wieloklasowa[,6])
data_klasyfikacja_wieloklasowa_X <- colnames(data_klasyfikacja_wieloklasowa)[-7]
data_klasyfikacja_wieloklasowa_Y <- 'V7'
summary(data_klasyfikacja_wieloklasowa)


# Regresja - Servo
data_regresja <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/servo/servo.data", header = FALSE)
summary(data_regresja)
data_regresja[,1] <- as.numeric(data_regresja[,1])
data_regresja[,2] <- as.numeric(data_regresja[,2])
data_regresja_X <- colnames(data_regresja)[-5]
data_regresja_Y <- 'V5'
summary(data_regresja)



# KNN
hiper_parametry_KNN_bin <- expand.grid(k=c(2,4,6,8,10,12))   
Kroswalidacja_KNN_bin <- CrossValidTune(data_klasyfikacja_binarna, data_klasyfikacja_binarna_X, data_klasyfikacja_binarna_Y, kFold = 8, hiper_parametry_KNN_bin, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_bin)

Kroswalidacja_KNN_bin_TABELA <- Kroswalidacja_KNN_bin
Kroswalidacja_KNN_bin_TABELA[is.na(Kroswalidacja_KNN_bin_TABELA)] <- 0.0
Kroswalidacja_KNN_bin_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_bin_TABELA 
                                                        %>% group_by(k) 
                                                        %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Kroswalidacja_KNN_bin_najlepsze_T <- Kroswalidacja_KNN_bin_TABELA_grupowana[which.max(Kroswalidacja_KNN_bin_TABELA_grupowana$JakoscT),]
Kroswalidacja_KNN_bin_najlepsze_W <- Kroswalidacja_KNN_bin_TABELA_grupowana[which.max(Kroswalidacja_KNN_bin_TABELA_grupowana$JakoscW),]

print(Kroswalidacja_KNN_bin_TABELA_grupowana)
print(Kroswalidacja_KNN_bin_najlepsze_T)
print(Kroswalidacja_KNN_bin_najlepsze_W)

ggplot(Kroswalidacja_KNN_bin_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_bin_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = JakoscT, color = 'blue'), size=1,) + 
   geom_line(aes(y = JakoscW, color = 'red'), size=1,) + 
   labs(title='KNN - klasyfikacja binarna', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")


save(hiper_parametry_KNN_bin, Kroswalidacja_KNN_bin, Kroswalidacja_KNN_bin_TABELA_grupowana, file = '1_KNN_BIN.RData')




resample_multi <- sample( 1:nrow(data_klasyfikacja_wieloklasowa), size = 800, replace = F )
data_klasyfikacja_wieloklasowa_sample <- data_klasyfikacja_wieloklasowa[resample_multi,]
summary(data_klasyfikacja_wieloklasowa_sample)



hiper_parametry_KNN_multi <- expand.grid(k=c(2,4,6,8,10,12))  
Kroswalidacja_KNN_multi <- CrossValidTune(data_klasyfikacja_wieloklasowa_sample, data_klasyfikacja_wieloklasowa_X, data_klasyfikacja_wieloklasowa_Y, kFold = 8, hiper_parametry_KNN_multi, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_multi)

Kroswalidacja_KNN_multi_TABELA <- Kroswalidacja_KNN_multi
Kroswalidacja_KNN_multi_TABELA[is.na(Kroswalidacja_KNN_multi_TABELA)] <- 0.0
Kroswalidacja_KNN_multi_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_multi_TABELA 
                                                          %>% group_by(k) 
                                                          %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Kroswalidacja_KNN_multi_najlepsze_T <- Kroswalidacja_KNN_multi_TABELA_grupowana[which.max(Kroswalidacja_KNN_multi_TABELA_grupowana$ACCT),]
Kroswalidacja_KNN_multi_najlepsze_W <- Kroswalidacja_KNN_multi_TABELA_grupowana[which.max(Kroswalidacja_KNN_multi_TABELA_grupowana$ACCW),]

print(Kroswalidacja_KNN_multi_TABELA_grupowana)
print(Kroswalidacja_KNN_multi_najlepsze_T)
print(Kroswalidacja_KNN_multi_najlepsze_W)

ggplot(Kroswalidacja_KNN_multi_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_multi_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = ACCT, color = 'blue'), size=1,) + 
   geom_line(aes(y = ACCW, color = 'red'), size=1,) + 
   labs(title='KNN - klasyfikacja wieloklasowa', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")


save(hiper_parametry_KNN_multi, Kroswalidacja_KNN_multi, Kroswalidacja_KNN_multi_TABELA_grupowana, file = '2_KNN_MULTI.RData')




hiper_parametry_KNN_reg <- expand.grid(k=c(2,4,6,8,10,12))  
Kroswalidacja_KNN_reg <- CrossValidTune(data_regresja, data_regresja_X, data_regresja_Y, kFold = 8, hiper_parametry_KNN_reg, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_reg)

Kroswalidacja_KNN_reg_TABELA <- Kroswalidacja_KNN_reg
Kroswalidacja_KNN_reg_TABELA[is.na(Kroswalidacja_KNN_reg_TABELA)] <- 0.0
Kroswalidacja_KNN_reg_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_reg_TABELA 
                                                        %>% group_by(k) 
                                                        %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Kroswalidacja_KNN_reg_najlepsze_T_MAE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAET),]
Kroswalidacja_KNN_reg_najlepsze_W_MAE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAEW),]

print(Kroswalidacja_KNN_reg_TABELA_grupowana)
print(Kroswalidacja_KNN_reg_najlepsze_T_MAE)
print(Kroswalidacja_KNN_reg_najlepsze_W_MAE)


ggplot(Kroswalidacja_KNN_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_reg_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = MAET, color = 'blue'), size=1,) + 
   geom_line(aes(y = MAEW, color = 'red'), size=1,) + 
   labs(title='KNN - regresja', x='Nr modelu', y='MAE') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("MAE_Trening", "MAE_Walidacja")) + theme(legend.position = "bottom")


save(hiper_parametry_KNN_reg, Kroswalidacja_KNN_reg, Kroswalidacja_KNN_reg_TABELA_grupowana, file = '3_KNN_REG.RData')



# Tree

print("Tree - bin")
hiper_parametry_Tree_bin <- expand.grid(depth=c(4,7,9), minobs=c(3,6,9), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.4))
Kroswalidacja_Tree_bin <- CrossValidTune(data_klasyfikacja_binarna, data_klasyfikacja_binarna_X, data_klasyfikacja_binarna_Y, kFold = 8, hiper_parametry_Tree_bin, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_bin)

Kroswalidacja_Tree_bin_TABELA <- Kroswalidacja_Tree_bin
Kroswalidacja_Tree_bin_TABELA[is.na(Kroswalidacja_Tree_bin_TABELA)] <- 0.0
Kroswalidacja_Tree_bin_TABELA_grupowana <- as.data.frame(Kroswalidacja_Tree_bin_TABELA 
                                                         %>% group_by(depth, minobs, type, overfit, cf) 
                                                         %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Kroswalidacja_Tree_bin_najlepsze_T <- Kroswalidacja_Tree_bin_TABELA_grupowana[which.max(Kroswalidacja_Tree_bin_TABELA_grupowana$JakoscT),]
Kroswalidacja_Tree_bin_najlepsze_W <- Kroswalidacja_Tree_bin_TABELA_grupowana[which.max(Kroswalidacja_Tree_bin_TABELA_grupowana$JakoscW),]

print("Tabela z wynikami Drzew decyzyjnych (klasyfikacja binarna) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_Tree_bin_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_Tree_bin_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_Tree_bin_najlepsze_W)

ggplot(Kroswalidacja_Tree_bin_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_Tree_bin_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = JakoscT, color = 'blue'), size=1,) + 
   geom_line(aes(y = JakoscW, color = 'red'), size=1,) + 
   labs(title='Drzewa Decyzyjne - klasyfikacja binarna', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")



print("Tree - multi")
hiper_parametry_Tree_multi <- expand.grid(depth=c(3,5,10), minobs=c(2,10), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.4))
Kroswalidacja_Tree_multi <- CrossValidTune(data_klasyfikacja_wieloklasowa, data_klasyfikacja_wieloklasowa_X, data_klasyfikacja_wieloklasowa_Y, kFold = 8, hiper_parametry_Tree_multi, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_multi)

Kroswalidacja_Tree_multi_TABELA <- Kroswalidacja_Tree_multi
Kroswalidacja_Tree_multi_TABELA[is.na(Kroswalidacja_Tree_multi_TABELA)] <- 0.0
Kroswalidacja_Tree_multi_TABELA_grupowana <- as.data.frame(Kroswalidacja_Tree_multi_TABELA 
                                                           %>% group_by(depth, minobs, type, overfit, cf) 
                                                           %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Kroswalidacja_Tree_multi_najlepsze_T <- Kroswalidacja_Tree_multi_TABELA_grupowana[which.max(Kroswalidacja_Tree_multi_TABELA_grupowana$ACCT),]
Kroswalidacja_Tree_multi_najlepsze_W <- Kroswalidacja_Tree_multi_TABELA_grupowana[which.max(Kroswalidacja_Tree_multi_TABELA_grupowana$ACCW),]

print("Tabela z wynikami Drzew decyzyjnych (klasyfikacja wieloklasowa) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_Tree_multi_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_Tree_multi_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_Tree_multi_najlepsze_W)

ggplot(Kroswalidacja_Tree_multi_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_Tree_multi_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = ACCT, color = 'blue'), size=1,) + 
   geom_line(aes(y = ACCW, color = 'red'), size=1,) + 
   labs(title='Drzewa Decyzyjne - klasyfikacja wieloklasowa', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")




print("Tree - reg")
hiper_parametry_Tree_reg <- expand.grid(depth=c(3,5,10), minobs=c(2,10), type=c('SS'), overfit = c('none'), cf=c(0.4))
Kroswalidacja_Tree_reg <- CrossValidTune(data_regresja, data_regresja_X, data_regresja_Y, kFold = 8, hiper_parametry_Tree_reg, algorytm="Tree", seed = 399)
print(Kroswalidacja_Tree_reg)

Kroswalidacja_Tree_reg_TABELA <- Kroswalidacja_Tree_reg
Kroswalidacja_Tree_reg_TABELA[is.na(Kroswalidacja_Tree_reg_TABELA)] <- 0.0
Kroswalidacja_Tree_reg_TABELA_grupowana <- as.data.frame(Kroswalidacja_Tree_reg_TABELA 
                                                         %>% group_by(depth, minobs, type, overfit, cf) 
                                                         %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Kroswalidacja_Tree_reg_najlepsze_T_MAE <- Kroswalidacja_Tree_reg_TABELA_grupowana[which.min(Kroswalidacja_Tree_reg_TABELA_grupowana$MAET),]
Kroswalidacja_Tree_reg_najlepsze_W_MAE <- Kroswalidacja_Tree_reg_TABELA_grupowana[which.min(Kroswalidacja_Tree_reg_TABELA_grupowana$MAEW),]
Kroswalidacja_Tree_reg_najlepsze_T_MAPE <- Kroswalidacja_Tree_reg_TABELA_grupowana[which.min(Kroswalidacja_Tree_reg_TABELA_grupowana$MAPET),]
Kroswalidacja_Tree_reg_najlepsze_W_MAPE <- Kroswalidacja_Tree_reg_TABELA_grupowana[which.min(Kroswalidacja_Tree_reg_TABELA_grupowana$MAPEW),]

print("Tabela z wynikami Drzew decyzyjnych (regresja) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_Tree_reg_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_Tree_reg_najlepsze_T_MAE)
print(Kroswalidacja_Tree_reg_najlepsze_T_MAPE)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_Tree_reg_najlepsze_W_MAE)
print(Kroswalidacja_Tree_reg_najlepsze_W_MAPE)

ggplot(Kroswalidacja_Tree_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_Tree_reg_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = MAET, color = 'blue'), size=1,) + 
   geom_line(aes(y = MAEW, color = 'red'), size=1,) + 
   labs(title='Drzewa Decyzyjne - regresja', x='Nr modelu', y='MAE') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("MAE_Trening", "MAE_Walidacja")) + theme(legend.position = "bottom")

ggplot(Kroswalidacja_Tree_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_Tree_reg_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = MAPET, color = 'blue'), size=1,) + 
   geom_line(aes(y = MAPEW, color = 'red'), size=1,) + 
   labs(title='Drzewa Decyzyjne - regresja', x='Nr modelu', y='MAPE') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("MAPE_Trening", "MAPE_Walidacja")) + theme(legend.position = "bottom")




# NN

print("Sieci NN - bin")
hiper_parametry_NN_bin <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_bin <- CrossValidTune(data_klasyfikacja_binarna, data_klasyfikacja_binarna_X, data_klasyfikacja_binarna_Y, kFold = 8, hiper_parametry_NN_bin, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_bin)

Kroswalidacja_NN_bin_TABELA <- Kroswalidacja_NN_bin
Kroswalidacja_NN_bin_TABELA[is.na(Kroswalidacja_NN_bin_TABELA)] <- 0.0
Kroswalidacja_NN_bin_TABELA_grupowana <- as.data.frame(Kroswalidacja_NN_bin_TABELA 
                                                       %>% group_by( h, lr, iter)
                                                       %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Kroswalidacja_NN_bin_najlepsze_T <- Kroswalidacja_NN_bin_TABELA_grupowana[which.max(Kroswalidacja_NN_bin_TABELA_grupowana$JakoscT),]
Kroswalidacja_NN_bin_najlepsze_W <- Kroswalidacja_NN_bin_TABELA_grupowana[which.max(Kroswalidacja_NN_bin_TABELA_grupowana$JakoscW),]

print("Tabela z wynikami Sieci Neuronowych (klasyfikacja binarna) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_NN_bin_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_NN_bin_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_NN_bin_najlepsze_W)

ggplot(Kroswalidacja_NN_bin_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_NN_bin_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = JakoscT, color = 'blue'), size=1,) + 
   geom_line(aes(y = JakoscW, color = 'red'), size=1,) + 
   labs(title='Sieci Neuronowe - klasyfikacja binarna', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")




print("Sieci NN - multi")
hiper_parametry_NN_multi <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_multi <- CrossValidTune(data_klasyfikacja_wieloklasowa, data_klasyfikacja_wieloklasowa_X, data_klasyfikacja_wieloklasowa_Y, kFold = 8, hiper_parametry_NN_multi, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_multi)

Kroswalidacja_NN_multi_TABELA <- Kroswalidacja_NN_multi
Kroswalidacja_NN_multi_TABELA[is.na(Kroswalidacja_NN_multi_TABELA)] <- 0.0
Kroswalidacja_NN_multi_TABELA_grupowana <- as.data.frame(Kroswalidacja_NN_multi_TABELA 
                                                         %>% group_by( h, lr, iter)
                                                         %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Kroswalidacja_NN_multi_najlepsze_T <- Kroswalidacja_NN_multi_TABELA_grupowana[which.max(Kroswalidacja_NN_multi_TABELA_grupowana$ACCT),]
Kroswalidacja_NN_multi_najlepsze_W <- Kroswalidacja_NN_multi_TABELA_grupowana[which.max(Kroswalidacja_NN_multi_TABELA_grupowana$ACCW),]

print("Tabela z wynikami Sieci Neuronowych (klasyfikacja wieloklasowa) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_NN_multi_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_NN_multi_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_NN_multi_najlepsze_W)

ggplot(Kroswalidacja_NN_multi_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_NN_multi_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = ACCT, color = 'blue'), size=1,) + 
   geom_line(aes(y = ACCW, color = 'red'), size=1,) + 
   labs(title='Sieci Neuronowe - klasyfikacja wieloklasowa', x='Nr modelu', y='Jakosc') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")




print("Sieci NN - reg")
hiper_parametry_NN_reg <- expand.grid(h=list(c(2,2), c(3,6), c(6,3), c(6,6)), lr = c(0.01), iter = c(30000, 90000))
Kroswalidacja_NN_reg <- CrossValidTune(data_regresja, data_regresja_X, data_regresja_Y, kFold = 8, hiper_parametry_NN_reg, algorytm="NN", seed = 399)
print(Kroswalidacja_NN_reg)

Kroswalidacja_NN_reg_TABELA <- Kroswalidacja_NN_reg
Kroswalidacja_NN_reg_TABELA[is.na(Kroswalidacja_NN_reg_TABELA)] <- 0.0
Kroswalidacja_NN_reg_TABELA_grupowana <- as.data.frame(Kroswalidacja_NN_reg_TABELA 
                                                       %>% group_by( h, lr, iter)
                                                       %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Kroswalidacja_NN_reg_najlepsze_T_MAE <- Kroswalidacja_NN_reg_TABELA_grupowana[which.min(Kroswalidacja_NN_reg_TABELA_grupowana$MAET),]
Kroswalidacja_NN_reg_najlepsze_W_MAE <- Kroswalidacja_NN_reg_TABELA_grupowana[which.min(Kroswalidacja_NN_reg_TABELA_grupowana$MAEW),]
Kroswalidacja_NN_reg_najlepsze_T_MAPE <- Kroswalidacja_NN_reg_TABELA_grupowana[which.min(Kroswalidacja_NN_reg_TABELA_grupowana$MAPET),]
Kroswalidacja_NN_reg_najlepsze_W_MAPE <- Kroswalidacja_NN_reg_TABELA_grupowana[which.min(Kroswalidacja_NN_reg_TABELA_grupowana$MAPEW),]

print("Tabela z wynikami Sieci Neuronowych (regresja) w zaleznosci od parametrow (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_NN_reg_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_NN_reg_najlepsze_T_MAE)
print(Kroswalidacja_NN_reg_najlepsze_T_MAPE)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_NN_reg_najlepsze_W_MAE)
print(Kroswalidacja_NN_reg_najlepsze_W_MAPE)

ggplot(Kroswalidacja_NN_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_NN_reg_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = MAET, color = 'blue'), size=1,) + 
   geom_line(aes(y = MAEW, color = 'red'), size=1,) + 
   labs(title='Sieci Neuronowe - regresja', x='Nr modelu', y='MAE') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("MAE_Trening", "MAE_Walidacja")) + theme(legend.position = "bottom")

ggplot(Kroswalidacja_NN_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_NN_reg_TABELA_grupowana)), size = 1)) + 
   geom_line(aes(y = MAPET, color = 'blue'), size=1,) + 
   geom_line(aes(y = MAPEW, color = 'red'), size=1,) + 
   labs(title='Sieci Neuronowe - regresja', x='Nr modelu', y='MAPE') + 
   scale_color_discrete(name = "Wyniki: ", labels = c("MAPE_Trening", "MAPE_Walidacja")) + theme(legend.position = "bottom")




# Wyniki dla modeli wbudowanych w biblioteki R

kontrola_kroswalidacji <- trainControl(method="cv", number = 8)

print("KNN - R - bin")
R_GRID_KNN_bin = expand.grid(k=2:20)
R_CV_KNN_binarna = train(x=data_klasyfikacja_binarna[,data_klasyfikacja_binarna_X], y=data_klasyfikacja_binarna[,data_klasyfikacja_binarna_Y], tuneGrid=R_GRID_KNN_bin, method='knn', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_KNN_binarna_Wynik = R_CV_KNN_binarna$results

print("KNN - R - multi")
R_GRID_KNN_multi = expand.grid(k=2:20)
R_CV_KNN_wieloklasowa = train(x=data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_X], y=data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_Y], tuneGrid=R_GRID_KNN_multi, method='knn', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_KNN_wieloklasowa_Wynik = R_CV_KNN_wieloklasowa$results

print("KNN - R - reg")
R_GRID_KNN_reg = expand.grid(k=2:20)
R_CV_KNN_regresja = train(x=data_regresja[,data_regresja_X], y=data_regresja[,data_regresja_Y], tuneGrid=R_GRID_KNN_reg, method='knn', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_KNN_regresja_Wynik = R_CV_KNN_regresja$results


print(paste("KNN w R - klasyfikacja binarna: k = ", R_CV_KNN_binarna$finalModel$k, " | Jakosc = " ,R_CV_KNN_binarna_Wynik$Accuracy[R_CV_KNN_binarna_Wynik$k == R_CV_KNN_binarna$finalModel$k]))
print(paste("KNN w R - klasyfikacja wieloklasowa: k = ", R_CV_KNN_wieloklasowa$finalModel$k, " | Jakosc = " ,R_CV_KNN_wieloklasowa_Wynik$Accuracy[R_CV_KNN_wieloklasowa_Wynik$k == R_CV_KNN_wieloklasowa$finalModel$k]))
print(paste("KNN w R - regresja: k = ", R_CV_KNN_regresja$finalModel$k, " | MAE = " ,R_CV_KNN_regresja_Wynik$MAE[R_CV_KNN_regresja_Wynik$k == R_CV_KNN_regresja$finalModel$k]))



print("TREE - R - bin")
R_GRID_TREE_bin = expand.grid(maxdepth=2:20)
R_CV_TREE_binarna = train(x=data_klasyfikacja_binarna[,data_klasyfikacja_binarna_X], y=data_klasyfikacja_binarna[,data_klasyfikacja_binarna_Y], tuneGrid=R_GRID_TREE_bin, method='rpart2', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_TREE_binarna_Wynik = R_CV_TREE_binarna$results

print("TREE - R - multi")
R_GRID_TREE_wieloklasowa = expand.grid(maxdepth=2:20)
R_CV_TREE_wieloklasowa = train(x=data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_X], y=data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_Y], tuneGrid=R_GRID_TREE_wieloklasowa, method='rpart2', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_TREE_wieloklasowa_Wynik = R_CV_TREE_wieloklasowa$results

print("TREE - R - reg")
R_GRID_TREE_regresja = expand.grid(maxdepth=2:20)
R_CV_TREE_regresja = train(x=data_regresja[,data_regresja_X], y=data_regresja[,data_regresja_Y], tuneGrid=R_GRID_TREE_regresja, method='rpart2', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_TREE_regresja_Wynik = R_CV_TREE_regresja$results

print(paste("Drzewa Decyzyjne w R - klasyfikacja binarna: glebokosc = ", R_CV_TREE_binarna[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Jakosc = " , R_CV_TREE_binarna_Wynik$Accuracy[R_CV_TREE_binarna_Wynik$maxdepth == R_CV_TREE_binarna[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Drzewa Decyzyjne w R - klasyfikacja wieloklasowa: glebokosc = ", R_CV_TREE_wieloklasowa[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Jakosc = " , R_CV_TREE_wieloklasowa_Wynik$Accuracy[R_CV_TREE_wieloklasowa_Wynik$maxdepth == R_CV_TREE_wieloklasowa[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Drzewa Decyzyjne w R - regresja: glebokosc = ", R_CV_TREE_regresja[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , R_CV_TREE_regresja_Wynik$MAE[R_CV_TREE_regresja_Wynik$maxdepth == R_CV_TREE_regresja[["finalModel"]][["tuneValue"]][["maxdepth"]]]))



print("Neural Network - R - bin")
data_klasyfikacja_binarna_norm <- MinMax(data_klasyfikacja_binarna[,data_klasyfikacja_binarna_X])
R_GRID_NN_binarna = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_binarna = train(x=data_klasyfikacja_binarna_norm, y=data_klasyfikacja_binarna[,data_klasyfikacja_binarna_Y], tuneGrid=R_GRID_NN_binarna, method='nnet', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_NN_binarna_Wynik = R_CV_NN_binarna$results

print("Neural Network - R - multi")
data_klasyfikacja_wieloklasowa_norm <- MinMax(data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_X])
R_GRID_NN_wieloklasowa = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_wieloklasowa = train(x=data_klasyfikacja_wieloklasowa_norm, y=data_klasyfikacja_wieloklasowa[,data_klasyfikacja_wieloklasowa_Y], tuneGrid=R_GRID_NN_wieloklasowa, method='nnet', metric='Accuracy', trControl=kontrola_kroswalidacji)
R_CV_NN_wieloklasowa_Wynik = R_CV_NN_wieloklasowa$results

print("Neural Network - R - reg")
data_regresja_norm <- MinMax(data_regresja)
R_GRID_NN_regresja = expand.grid(size=1:12, decay = c(0.0002, 0.02))
R_CV_NN_regresja = train(x=data_regresja_norm[,data_regresja_X], y=data_regresja_norm[,data_regresja_Y], tuneGrid=R_GRID_NN_regresja, method='nnet', metric='MAE', trControl=kontrola_kroswalidacji)
R_CV_NN_regresja_Wynik = R_CV_NN_regresja$results

print(paste("Sieci Neuronowe w R - klasyfikacja binarna: h = ", R_CV_NN_binarna[["finalModel"]][["tuneValue"]][["size"]], " | Jakosc = " , R_CV_NN_binarna_Wynik$Accuracy[R_CV_NN_binarna_Wynik$size == R_CV_NN_binarna[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("Sieci Neuronowe w R - klasyfikacja wieloklasowa: h = ", R_CV_NN_wieloklasowa[["finalModel"]][["tuneValue"]][["size"]], " | Jakosc = " , R_CV_NN_wieloklasowa_Wynik$Accuracy[R_CV_NN_wieloklasowa_Wynik$size == R_CV_NN_wieloklasowa[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("Sieci Neuronowe w R - regresja : h = ", R_CV_NN_regresja[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , R_CV_NN_regresja_Wynik$MAE[R_CV_NN_regresja_Wynik$size == R_CV_NN_regresja[["finalModel"]][["tuneValue"]][["size"]]][1]))


