# Zad 1

library("pROC")


#Regresja
MAE <- function(a, b){
  return(mean(abs(a - b)))
}

MSE <- function(a, b){
  return(mean((a - b)^2))
}

MAPE <- function(a, b){
  return( mean(abs((a - b)/a) ))
}


#Klasyfikacja
AUC <- function(a, b){
  roc_krzywa <- roc(a, b)
  TPR <- rev(roc_krzywa$sensitivities)
  FPR <- rev(1 - roc_krzywa$specificities)
  dFPR <- c(diff(FPR), 0)
  dTPR <- c(diff(TPR), 0)
  AUC_wynik <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(AUC_wynik)
}

Youden <- function(a, b){
  roc_krzywa <- roc(a, b)
  TPR <- roc_krzywa$sensitivities
  FPR <- roc_krzywa$specificities
  max_Y <- 0
  for (i in 1:length(TPR)) {
    Youden <- (TPR[i] + FPR[i]-1)
    if(Youden > max_Y){
      max_Y=Youden
    }
  }
  return(max_Y)
}

Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
Mat

Czulosc <- function(Mat){
  return((Mat[1] / (Mat[1] + Mat[3])))
}

Specyficznosc <- function(Mat){
  return((Mat[4] / (Mat[4] + Mat[2])))
}

Jakosc <- function(Mat){
  return(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])))
}

ModelOcena <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar)){
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czu³oœæ" = Czulosc(Mat), "Specyficznoœæ" = Specyficznosc(Mat), "Jakoœæ" = Jakosc(Mat))
    klasyfikacja <- list(Mat, Youden(y_tar, y_hat), miary)
    return(klasyfikacja)
  }
  else{
    c("Dane niepoprawne")
  }
}

#Regresja
y_tar <- rnorm(20)
y_hat <- y_tar + 0.3
ModelOcena(y_tar,y_hat)

#Klasyfikacja
y_tar <- c(0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,0)
y_hat <- (length(y_tar):1) / length(y_tar)
y_tar<-as.factor(c(y_tar))
ModelOcena(y_tar,y_hat)



# Zad 2

CrossValidTune <- function(dane, kFold, parTune, seed){
  set.seed(seed)
  k = nrow(dane)
  wektor = c()
  lista = list()
  
  for (j in 1:kFold){
    indxT <- sample( x = 1:k, size = round((1-1/kFold) * k), replace = F )
    s_indxT = sort(indxT)
    p <- 1
    while (p<=length(s_indxT)){
      for (i in 1:k){
        if (s_indxT[p] == i){
          wektor[i] == 1
          p = p+1
        }
        else{
          wektor[i] == 2
        }
      }
    }
    
    lista[[j]] = c(wektor)
  }
  
  ramka_r <- data.frame( kFold_par =c(1,2,1,2))
  for (i in 1:(length(parTune))){
    ramka_r[i+1] = parTune[i]
  }
  
  if(is.numeric(y_tar)){
    f_reg <- ramka_r
    f_reg['MAEt'] = 0
    f_reg['MSEt'] = 0
    f_reg['MAPEt'] = 0
    f_reg['MAEw'] = 0
    f_reg['MSEw'] = 0
    f_reg['MAPEw'] = 0
    return(f_reg)
  }
  else if(is.factor(y_tar)){
    f_klas <- ramka_r
    f_klas['AUCT'] = 0
    f_klas['Czu³oœæ'] = 0
    f_klas['Specyficznoœæ'] = 0
    f_klas['Jakoœæ'] = 0
    f_klas['AUCW'] = 0
    f_klas['Specyficznoœæ'] = 0
    f_klas['MAPEW'] = 0
    f_klas['Jakoœæ'] = 0
    return(f_klas)
  }
}

CrossValidTune(data.frame(c(1:10),c(3:12)), 5, data.frame( a = c(1,2), b = c(1,1) ), 555)
