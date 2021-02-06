#zadanie 1
library("pROC")

MAE <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

MSE <- function(y_tar, y_hat){
  return(mean((y_tar - y_hat)^2))
}

MAPE <- function(y_tar, y_hat){
  return( mean(abs((y_tar - y_hat)/y_tar) ))
}

AUC_MT <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat)
  TPR <- rev(roc_obj$sensitivities) #czu³oœæ posortowana rosn¹co
  FPR <- rev(1 - roc_obj$specificities) #1 - specyficznoœæ posortowana rosn¹co
  dFPR <- c(diff(FPR), 0) #ró¿nica pomiêdzy kolejnymi wartoœciami czu³oœci
  dTPR <- c(diff(TPR), 0)
  AUC_wynik <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2
  return(AUC_wynik)
}

J <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat)
  TPR <- roc_obj$sensitivities #czu³oœæ
  FPR <- roc_obj$specificities #specyficznoœæ
  max_J <- 0
  for (i in 1:length(TPR)) {
    J <- (TPR[i] + FPR[i]-1)
    if(J > max_J){
      max_J=J
    }
  }
  return(max_J)
}

Mat <- table(y_tar, y_hat = ifelse(y_hat <= J(y_tar, y_hat), 0, 1))


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
    miary <- c( "AUC" = AUC_MT(y_tar, y_hat), "Czu³oœæ" = Czulosc(Mat), "Specyficznoœæ" = Specyficznosc(Mat), "Jakoœæ" = Jakosc(Mat))
    klasyfikacja <- list(Mat, J(y_tar, y_hat), miary)
    return(klasyfikacja)
  }
  else{
    c("Niepoprawne dane!")
  }
}

y_tar <- rnorm(10)
y_hat <- y_tar + 0.1
ModelOcena(y_tar,y_hat)

y_tar <- as.factor(c(1,1,0,1,0,1,0,0,0,0))
y_hat <- (length(y_tar):1) / length(y_tar)
ModelOcena(y_tar,y_hat)


#zadanie 2

CrossValidTune <- function(dane, kFold,parTune, seed){
  set.seed(seed)
  lista <- list()
  for (i in 1:kFold){
    indxT <- sample( x = 1:nrow(dane), size = (1-1/kFold) * nrow(dane), replace = F )
    indV <- (1:nrow(dane))[-indxT]
    lista[[i]] <- sample(1:nrow(dane) , size = nrow(dane), replace = F)
    for (j in 1:nrow(dane)) {
      for(k in 1:length(indV)){
        if(j==indV[k]){
          lista[[i]][[j]] = 2
          break
        }
        else{
          lista[[i]][[j]] = 1
        }
      }
    }
  }
  #return(lista)
  
  #zak³adamy, ze pierwsza kolumna w danych przekazanych do fukcji to y-zmienna objaœniana
  #w ten sposób mo¿liwe bêdzie rozpoznanie czy mamy do czynienia z regresj¹ czy klasyfikacj¹
  
  k <- rep(c(1:kFold), times=length(parTune))
  wyniki <- data.frame(k, parTune)
  
  if(is.numeric(dane[,1])){
    regresja_wyniki <- data.frame(wyniki, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0  )
    return(regresja_wyniki)
  }
  else if(is.factor(dane[,1])){
    klasyfikacja_wyniki <- data.frame(wyniki, AUCT=0, Czu³oœæT=0, SpecyficznoœæT=0, JakoœæT=0,
                                      AUCW=0, SpecyficznoœæW=0, MAPEW=0, JakoœæW=0)
    return(klasyfikacja_wyniki)
  }
  else{
    c("Niepoprawne dane!")
  }
  
  ## krok 3 
  
}

dane = iris
kFold=5
parTune = data.frame( a = c(1,2), b = c(1,1) )
seed=555
CrossValidTune(dane, kFold,parTune, seed)






