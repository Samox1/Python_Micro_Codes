rm(list=ls())

library(data.tree)
library(rpart.plot)
library(tidyverse)

source("funkcje.R")

dane <- iris
nazwy_kolumn <- colnames(dane)
X <- nazwy_kolumn[-5]
Y <- nazwy_kolumn[5]

parTune <- expand.grid(depth=c(3:4), minobs=c(2:3))


CrossValidTune <- function(dane, X, Y, kFold = 3, parTune, algorytm="KNN", seed = 123)
{
  set.seed(seed)
  
  dl_wektora = nrow(dane)
  podzial_zbioru <- data.frame()
  
  ramka <- as.data.frame(expand_grid(k=c(1:kFold), parTune))
  
  for(i in 1:kFold)
  { 
    indxTest <- sample( 1:dl_wektora, size = 1/kFold * dl_wektora, replace = F )
    podzial_zbioru[indxTest,i] <- 1
    podzial_zbioru[-indxTest,i] <- 2
  }
  
  
  
  for(id_modele in 1:nrow(ramka))
  {
    Drzewo1 <- Tree(Y, X, dane, 'Gini', parTune$depth, parTune$minobs, 'none', 0.2)
  }
  
  
  
  if(is.numeric(dane[,Y]))
  {
    regresja <- data.frame(parTune, MAET=0, MSET=0, MAPET=0, MAEW=0, MSEW=0, MAPEW=0  )
    # regresja %>% group_by(colnames(parTune)) %>% summarise_at(vars(contains("T", "W")), mean)
    return(regresja)
  }
  else if(is.factor(dane[,Y]))
  {
    klasyfikacja_bin <- data.frame(parTune, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0, AUCW=0, CzuloscW=0, SpecyficznoscW=0, JakoscW=0)
    # klasyfikacja_bin %>% group_by(colnames(parTune)) %>% summarise_at(vars(contains("T", "W")), mean)
    return(klasyfikacja_bin)
  }
  else
  {
    print("Niepoprawne dane")
  }
}


kappa <- CrossValidTune(dane, X, Y, kFold = 3, parTune, algorytm="KNN", seed = 123)


