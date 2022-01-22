rm(list=ls())

library(data.tree)
library(rpart.plot)
library(tidyverse)
library(pROC)
library(openxlsx)

source("funkcje.R")


### DANE ###

# Klasyfikacja Binarna      = https://archive.ics.uci.edu/ml/datasets/Wholesale+customers         # Dane przez prowadzacego
# Klasyfikacja Wieloklasowa = https://archive.ics.uci.edu/ml/datasets/seeds
# Regresja                  = https://archive.ics.uci.edu/ml/datasets/Energy+efficiency


dane_bin <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
dane_bin <- dane_bin[,-2]
bin_kolumny <- colnames(dane_bin)               # glupi blad - przy zebraniu nazwy "dane_bin[,1]" jest NULL
dane_bin_X <- bin_kolumny[-1]
dane_bin_Y <- bin_kolumny[1]
dane_bin[,1] <- as.factor(dane_bin[,1])


dane_multi <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", header = FALSE, sep = "\t")
dane_multi <- drop_na(dane_multi)
multi_kolumny <- colnames(dane_multi)               
dane_multi_X <- multi_kolumny[-8]
dane_multi_Y <- multi_kolumny[8]
dane_multi[,8] <- as.factor(dane_multi[,8])


dane_reg <- read.xlsx("https://archive.ics.uci.edu/ml/machine-learning-databases/00242/ENB2012_data.xlsx")
dane_reg <- dane_reg[,-10]
reg_kolumny <- colnames(dane_reg)               
dane_reg_X <- reg_kolumny[-9]
dane_reg_Y <- reg_kolumny[9]




### KNN ###
parTune_KNN_bin <- expand.grid(k=3:15)
KNN_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 5, parTune_KNN_bin, algorytm="KNN", seed = 123)
KNN_bin_CrossValid




parTune_KNN_multi <- expand.grid(k=3:15)
KNN_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 5, parTune_KNN_multi, algorytm="KNN", seed = 123)
KNN_multi_CrossValid

KNN_multi_CrossValid_gr <- KNN_multi_CrossValid %>% group_by(k)
KNN_multi_CrossValid_gr <- as.data.frame(KNN_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
KNN_multi_CrossValid_gr
KNN_multi_best_T <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCT),]
KNN_multi_best_W <- KNN_multi_CrossValid_gr[which.max(KNN_multi_CrossValid_gr$ACCW),]
KNN_multi_best_T
KNN_multi_best_W



parTune_KNN_reg <- expand.grid(k=1:12)
KNN_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 5, parTune_KNN_reg, algorytm="KNN", seed = 123)
KNN_reg_CrossValid



### Drzewa Decyzyjne ###
parTune_Tree_bin <- expand.grid(depth=c(5:6), minobs=c(2:3), type=c('Entropy'), overfit = c('none', 'prune'), cf=0.2)
Tree_bin_CrossValid <- CrossValidTune(dane_bin, dane_bin_X, dane_bin_Y, kFold = 5, parTune_Tree_bin, algorytm="Tree", seed = 123)

Tree_bin_CrossValid_gr <- Tree_bin_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_bin_CrossValid_gr <- as.data.frame(Tree_bin_CrossValid_gr %>% summarise(
  AUCT = mean(AUCT), CzuloscT = mean(CzuloscT), SpecyficznoscT = mean(SpecyficznoscT), JakoscT = mean(JakoscT),
  AUCW = mean(AUCW), CzuloscW = mean(CzuloscW), SpecyficznoscW = mean(SpecyficznoscW), JakoscW = mean(JakoscW), ))
Tree_bin_CrossValid_gr
Tree_bin_best_T <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscT),]
Tree_bin_best_W <- Tree_bin_CrossValid_gr[which.max(Tree_bin_CrossValid_gr$JakoscW),]
Tree_bin_best_T
Tree_bin_best_W


parTune_Tree_multi <- expand.grid(depth=c(5:6), minobs=c(2:3), type=c('Entropy'), overfit = c('none', 'prune'), cf=0.2)
Tree_multi_CrossValid <- CrossValidTune(dane_multi, dane_multi_X, dane_multi_Y, kFold = 5, parTune_Tree_multi, algorytm="Tree", seed = 123)

Tree_multi_CrossValid_gr <- Tree_multi_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_multi_CrossValid_gr <- as.data.frame(Tree_multi_CrossValid_gr %>% summarise(ACCT = mean(ACCT), ACCW = mean(ACCW)))
Tree_multi_CrossValid_gr
Tree_multi_best_T <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCT),]
Tree_multi_best_W <- Tree_multi_CrossValid_gr[which.max(Tree_multi_CrossValid_gr$ACCW),]
Tree_multi_best_T
Tree_multi_best_W


parTune_Tree_reg <- expand.grid(depth=c(5:6), minobs=c(2:3), type=c('SS'), overfit = c('none'), cf=0.2)
Tree_reg_CrossValid <- CrossValidTune(dane_reg, dane_reg_X, dane_reg_Y, kFold = 5, parTune_Tree_reg, algorytm="Tree", seed = 123)

Tree_reg_CrossValid_gr <- Tree_reg_CrossValid %>% group_by(depth, minobs, type, overfit, cf)
Tree_reg_CrossValid_gr <- as.data.frame(Tree_reg_CrossValid_gr %>% summarise(MAET = mean(MAET), MSET = mean(MSET), MAPET = mean(MAPET), MAEW = mean(MAEW), MSEW = mean(MSEW), MAPEW = mean(MAPEW)))
Tree_reg_CrossValid_gr
Tree_reg_best_T <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAPET),]
Tree_reg_best_W <- Tree_reg_CrossValid_gr[which.min(Tree_reg_CrossValid_gr$MAPEW),]
Tree_reg_best_T
Tree_reg_best_W



### Sieci Neuronowe ###





