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
bin_cancer <- bin_cancer[! bin_cancer$V9 == "?",]       # Usuniecie wierszy z wartosciami '?'
bin_cancer$V2 <- as.factor(bin_cancer$V2)
bin_cancer$V3 <- as.factor(bin_cancer$V3)
bin_cancer$V4 <- as.factor(bin_cancer$V4)
bin_cancer$V5 <- as.factor(bin_cancer$V5)
bin_cancer$V6 <- as.factor(as.character(bin_cancer$V6))
bin_cancer$V7 <- as.factor(bin_cancer$V7)
bin_cancer$V8 <- as.factor(bin_cancer$V8)
bin_cancer$V9 <- as.factor(as.character(bin_cancer$V9))
bin_cancer$V10 <- as.factor(bin_cancer$V10)
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
multi_abalone_X <- colnames(multi_abalone)[-9]
multi_abalone_Y <- colnames(multi_abalone)[9]
multi_abalone[,1] <- as.numeric(as.factor(multi_abalone[,1]))
multi_abalone[,9] <- as.factor(multi_abalone[,9])
summary(multi_abalone)
summary(multi_abalone[,9])


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

Kroswalidacja_KNN_bin_TABELA <- Kroswalidacja_KNN_bin
Kroswalidacja_KNN_bin_TABELA[is.na(Kroswalidacja_KNN_bin_TABELA)] <- 0.0
Kroswalidacja_KNN_bin_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_bin_TABELA 
                                                        %>% group_by(k) 
                                                        %>% summarise(AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Kroswalidacja_KNN_bin_najlepsze_T <- Kroswalidacja_KNN_bin_TABELA_grupowana[which.max(Kroswalidacja_KNN_bin_TABELA_grupowana$JakoscT),]
Kroswalidacja_KNN_bin_najlepsze_W <- Kroswalidacja_KNN_bin_TABELA_grupowana[which.max(Kroswalidacja_KNN_bin_TABELA_grupowana$JakoscW),]

print("Tabela z wynikami Drzew decyzyjnych (klasyfikacja binarna) w zaleznosci od parametu k (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_KNN_bin_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_KNN_bin_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_KNN_bin_najlepsze_W)

ggplot(Kroswalidacja_KNN_bin_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_bin_TABELA_grupowana)), size = 1)) + 
  geom_line(aes(y = JakoscT, color = 'blue'), size=1,) + 
  geom_line(aes(y = JakoscW, color = 'red'), size=1,) + 
  labs(title='KNN - klasyfikacja binarna', x='Nr modelu', y='Jakosc') + 
  scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")



print("KNN - multi")
hiper_parametry_KNN_multi <- expand.grid(k=c(2:8))
Kroswalidacja_KNN_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_KNN_multi, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_multi)

Kroswalidacja_KNN_multi_TABELA <- Kroswalidacja_KNN_multi
Kroswalidacja_KNN_multi_TABELA[is.na(Kroswalidacja_KNN_multi_TABELA)] <- 0.0
Kroswalidacja_KNN_multi_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_multi_TABELA 
                                                        %>% group_by(k) 
                                                        %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Kroswalidacja_KNN_multi_najlepsze_T <- Kroswalidacja_KNN_multi_TABELA_grupowana[which.max(Kroswalidacja_KNN_multi_TABELA_grupowana$ACCT),]
Kroswalidacja_KNN_multi_najlepsze_W <- Kroswalidacja_KNN_multi_TABELA_grupowana[which.max(Kroswalidacja_KNN_multi_TABELA_grupowana$ACCW),]

print("Tabela z wynikami KNN (klasyfikacja wieloklasowa) w zaleznosci od parametu k (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_KNN_multi_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_KNN_multi_najlepsze_T)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_KNN_multi_najlepsze_W)

ggplot(Kroswalidacja_KNN_multi_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_multi_TABELA_grupowana)), size = 1)) + 
  geom_line(aes(y = ACCT, color = 'blue'), size=1,) + 
  geom_line(aes(y = ACCW, color = 'red'), size=1,) + 
  labs(title='KNN - klasyfikacja wieloklasowa', x='Nr modelu', y='Jakosc') + 
  scale_color_discrete(name = "Wyniki: ", labels = c("Jakosc_Trening", "Jakosc_Walidacja")) + theme(legend.position = "bottom")



print("KNN - reg")
hiper_parametry_KNN_reg <- expand.grid(k=c(2:12))
Kroswalidacja_KNN_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_KNN_reg, algorytm="KNN", seed = 399)
print(Kroswalidacja_KNN_reg)

Kroswalidacja_KNN_reg_TABELA <- Kroswalidacja_KNN_reg
Kroswalidacja_KNN_reg_TABELA[is.na(Kroswalidacja_KNN_reg_TABELA)] <- 0.0
Kroswalidacja_KNN_reg_TABELA_grupowana <- as.data.frame(Kroswalidacja_KNN_reg_TABELA 
                                                          %>% group_by(k) 
                                                          %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Kroswalidacja_KNN_reg_najlepsze_T_MAE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAET),]
Kroswalidacja_KNN_reg_najlepsze_W_MAE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAEW),]
Kroswalidacja_KNN_reg_najlepsze_T_MAPE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAPET),]
Kroswalidacja_KNN_reg_najlepsze_W_MAPE <- Kroswalidacja_KNN_reg_TABELA_grupowana[which.min(Kroswalidacja_KNN_reg_TABELA_grupowana$MAPEW),]

print("Tabela z wynikami KNN (regresja) w zaleznosci od parametru k (usrednione wyniki z kroswalidacji)")
print(Kroswalidacja_KNN_reg_TABELA_grupowana)
print("Najlepszy model pod katem dokladnosci (dane treningowe): ")
print(Kroswalidacja_KNN_reg_najlepsze_T_MAE)
print(Kroswalidacja_KNN_reg_najlepsze_T_MAPE)
print("Najlepszy model pod katem dokladnosci (dane walidacyjne): ")
print(Kroswalidacja_KNN_reg_najlepsze_W_MAE)
print(Kroswalidacja_KNN_reg_najlepsze_W_MAPE)

ggplot(Kroswalidacja_KNN_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_reg_TABELA_grupowana)), size = 1)) + 
  geom_line(aes(y = MAET, color = 'blue'), size=1,) + 
  geom_line(aes(y = MAEW, color = 'red'), size=1,) + 
  labs(title='KNN - regresja', x='Nr modelu', y='MAE') + 
  scale_color_discrete(name = "Wyniki: ", labels = c("MAE_Trening", "MAE_Walidacja")) + theme(legend.position = "bottom")

ggplot(Kroswalidacja_KNN_reg_TABELA_grupowana , aes(x=c(1:nrow(Kroswalidacja_KNN_reg_TABELA_grupowana)), size = 1)) + 
  geom_line(aes(y = MAPET, color = 'blue'), size=1,) + 
  geom_line(aes(y = MAPEW, color = 'red'), size=1,) + 
  labs(title='KNN - regresja', x='Nr modelu', y='MAPE') + 
  scale_color_discrete(name = "Wyniki: ", labels = c("MAPE_Trening", "MAPE_Walidacja")) + theme(legend.position = "bottom")



# Tree

print("Tree - bin")
hiper_parametry_Tree_bin <- expand.grid(depth=c(3,5,10), minobs=c(2,10), type=c('Entropy', 'Gini'), overfit = c('none', 'prune'), cf=c(0.4))
Kroswalidacja_Tree_bin <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 9, hiper_parametry_Tree_bin, algorytm="Tree", seed = 399)
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
Kroswalidacja_Tree_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_Tree_multi, algorytm="Tree", seed = 399)
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
Kroswalidacja_Tree_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_Tree_reg, algorytm="Tree", seed = 399)
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
Kroswalidacja_NN_bin <- CrossValidTune(bin_cancer, bin_cancer_X, bin_cancer_Y, kFold = 9, hiper_parametry_NN_bin, algorytm="NN", seed = 399)
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
Kroswalidacja_NN_multi <- CrossValidTune(multi_abalone, multi_abalone_X, multi_abalone_Y, kFold = 9, hiper_parametry_NN_multi, algorytm="NN", seed = 399)
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
Kroswalidacja_NN_reg <- CrossValidTune(reg_automobile, reg_automobile_X, reg_automobile_Y, kFold = 9, hiper_parametry_NN_reg, algorytm="NN", seed = 399)
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


print(paste("KNN w R - klasyfikacja binarna: k = ", R_CV_KNN_binarna$finalModel$k, " | Jakosc = " ,R_CV_KNN_binarna_Wynik$Accuracy[R_CV_KNN_binarna_Wynik$k == R_CV_KNN_binarna$finalModel$k]))
print(paste("KNN w R - klasyfikacja wieloklasowa: k = ", R_CV_KNN_wieloklasowa$finalModel$k, " | Jakosc = " ,R_CV_KNN_wieloklasowa_Wynik$Accuracy[R_CV_KNN_wieloklasowa_Wynik$k == R_CV_KNN_wieloklasowa$finalModel$k]))
print(paste("KNN w R - regresja: k = ", R_CV_KNN_regresja$finalModel$k, " | MAE = " ,R_CV_KNN_regresja_Wynik$MAE[R_CV_KNN_regresja_Wynik$k == R_CV_KNN_regresja$finalModel$k]))



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

print(paste("Drzewa Decyzyjne w R - klasyfikacja binarna: glebokosc = ", R_CV_TREE_binarna[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Jakosc = " , R_CV_TREE_binarna_Wynik$Accuracy[R_CV_TREE_binarna_Wynik$maxdepth == R_CV_TREE_binarna[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Drzewa Decyzyjne w R - klasyfikacja wieloklasowa: glebokosc = ", R_CV_TREE_wieloklasowa[["finalModel"]][["tuneValue"]][["maxdepth"]], " | Jakosc = " , R_CV_TREE_wieloklasowa_Wynik$Accuracy[R_CV_TREE_wieloklasowa_Wynik$maxdepth == R_CV_TREE_wieloklasowa[["finalModel"]][["tuneValue"]][["maxdepth"]]]))
print(paste("Drzewa Decyzyjne w R - regresja: glebokosc = ", R_CV_TREE_regresja[["finalModel"]][["tuneValue"]][["maxdepth"]], " | MAE = " , R_CV_TREE_regresja_Wynik$MAE[R_CV_TREE_regresja_Wynik$maxdepth == R_CV_TREE_regresja[["finalModel"]][["tuneValue"]][["maxdepth"]]]))



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

print(paste("Sieci Neuronowe w R - klasyfikacja binarna: h = ", R_CV_NN_binarna[["finalModel"]][["tuneValue"]][["size"]], " | Jakosc = " , R_CV_NN_binarna_Wynik$Accuracy[R_CV_NN_binarna_Wynik$size == R_CV_NN_binarna[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("Sieci Neuronowe w R - klasyfikacja wieloklasowa: h = ", R_CV_NN_wieloklasowa[["finalModel"]][["tuneValue"]][["size"]], " | Jakosc = " , R_CV_NN_wieloklasowa_Wynik$Accuracy[R_CV_NN_wieloklasowa_Wynik$size == R_CV_NN_wieloklasowa[["finalModel"]][["tuneValue"]][["size"]]]))
print(paste("Sieci Neuronowe w R - regresja : h = ", R_CV_NN_regresja[["finalModel"]][["tuneValue"]][["size"]], " | MAE = " , R_CV_NN_regresja_Wynik$MAE[R_CV_NN_regresja_Wynik$size == R_CV_NN_regresja[["finalModel"]][["tuneValue"]][["size"]]][1]))


