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
  
  ramka <- as.data.frame(expand_grid(k=c(1:kFold), parTune))
  
  podzial_zbioru <- data.frame()
  
  for(i in 1:kFold)
  { 
    indxTest <- sample( 1:nrow(dane), size = 1/kFold * nrow(dane), replace = F )
    podzial_zbioru[indxTest,i] <- 1
    podzial_zbioru[-indxTest,i] <- 2
    
  }
  
  
  if(is.numeric(dane[,Y]))
  {
    regresja <- data.frame(parTune, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0  )
    return(regresja)
  }
  else if(is.factor(dane[,Y]))
  {
    klasyfikacja_bin <- data.frame(parTune, AUCT=0, CzuloscT=0, SpecyficznoscT=0, JakoscT=0,
                                   AUCW=0, CzuloscT=0, SpecyficznoscW=0, JakoscW=0)
    return(klasyfikacja_bin)
  }
  else
  {
    print("Niepoprawne dane")
  }
}


kappa <- CrossValidTune(dane, X, Y, kFold = 3, parTune, algorytm="KNN", seed = 123)


