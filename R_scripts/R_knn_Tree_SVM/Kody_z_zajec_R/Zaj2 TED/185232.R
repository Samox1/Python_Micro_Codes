# Zadanie 0:
B <- matrix(1,1000,1001)
dim(B)
colnames(B) <- c("Y",paste("x",1:1000,sep = "_"))
set.seed(555)
B[,"Y"]<- sample(x = 1:100,size = 1000,replace = T)
for(i in 2:1001){
  B[,i] <-   B[,"Y"]+rnorm(1)
}
B
# Zadanie 1:
ModelParallel <-
  function(dane, Ynazwa, XnazwyList, Nrdzeni, metoda) {
    lista_wynik <- list()
    dane = data.frame(dane)
    if (metoda == 'for') {
      wynik <-
        foreach(i = 1:length(XnazwyList),
                .export = c("lista_wynik")) %dopar% {
                  model <- paste("Y ~", XnazwyList[[i]][1])
                  j = 1
                  while (j < length(XnazwyList[[i]])) {
                    model <- paste(model, "+", XnazwyList[[i]][j + 1])
                    j = j + 1
                  }
                  model_wyn <- lm(model, data = dane)
                  lista_wynik[[i]] = cbind(names(coef(model_wyn)), coef(model_wyn))
                }
      wynik
    }
    
    if (metoda == 'lapply') {
      parLapply(klaster, 1:Nrdzeni, function() {
        model <- lm(dane[, Ynazwa] ~ dane[, XnazwyList[klaster]])
        tabela <- cbind(names(coef(model)), coef(model))
        lista_wynik[klaster] <- tabela
      })
      lista_wynik
    }
  }
ModelParallel(macierz, 'Y', list(x_1, c(x_1, x_5, x_7), x_2, x_3), 4, 'for')


# Zadanie 2:
#Regresja
MAE <- function(a, b){
  return(mean(abs(a - b)))
}

MSE <- function(a, b){
  return(mean((a - b)^2))
}

MAPE <- function(a, b){
  return(mean(abs((a - b)/a) ))
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

Youden <- function(y_tar, y_hat){
  roc_obj <- roc(y_tar, y_hat)
  TPR <- roc_obj$sensitivities #czu?o??
  FPR <- roc_obj$specificities #specyficzno??
  max_J <- 0
  for (i in 1:length(TPR)) {
    J <- (TPR[i] + FPR[i]-1)
    if(J > max_J){
      max_J=J
    }
  }
  return(max_J)
}

Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))

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
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
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

# Zadanie 3:
# a) Stwórz funkcję "CrossValidTune" przyjmującą nastęujące parametry: "dane", "kFold", "parTune", "seed". 
#    W skrócie: funkcja powinna krosswalidacyjnie tunować parametry danego algorytu. 
# b) Funkcja powinna w pierwszym kroku stworzyć listę przechowującą informację, 
#które obserwacje posłużą jako zbiór treningowy,
#    a które wejdą do zbioru walidacyjnego. Ilość elementów listy zdefiniowana jest 
#przez parametr "kFold" (liczba podzbiorów walidacyjnych).
#    Każdy element listy jest wektorem o tej samej długości, równej nrow("dane").
#    Każdy z wektorów zawiera albo liczbę 1 (obserwacja jest w zbiorze treningowym) albo 2 (obserwacja jest w zbiorze walidacyjnym). 
#    Przykład: list( c(1,2,1,1), c(2,1,1,2) ) - oznacza, że mamy doczynienia z 2-krotną walidacją na zbiorze z 4 obserwacjami, 
#    gdzie dla pierwszej iteracji tylko jeden element jest w podzbiorze walidacyjnym.
#    Losowanie rozpoczyna się od ustawienia ziarna na wartość "seed".
# c) W kolejnym kroku funkcja powinna stworzyć ramkę danych, w której przechowywane będą wyniki oraz kombinacje parametrów.
#    Liczba wierszy i kolumn zależy od zagadnienia (klasyfikacja, regresja) oraz od liczby tunowanych parametrów "parTune" i "kFold":
#    Przykład: "parTune" = data.frame( a = c(1,2), b = c(1,1) ) - oznacza, że algorytm ma 2 parametry do tunowania,
#              Dla "kFold" = 2 oraz "parTune", kombinacja parametrów to data.frame( "kFold" = c(1,2,1,2), a = c(1,2), b = c(1,1) ).
#              Kolejne kolumny tabeli wynikowej powinny stanowić miary uzyskane dzięki funkcji "ModelOcena".
#              Regresja: MAEt, MSEt, MAPEt, MAEw, MSEw, MAPEw - ozanczają miary dla zbioru treningowego i walidacyjnego.
#                        Finalnie tabele jest rozmiaru 4x9.
#              Klasyfikacja: AUCT, CzułośćT, SpecyficznośćT, JakośćT, AUCW, SpecyficznośćW, MAPEW, JakośćW - j.w.
#                            Finalnie tabele jest rozmiaru 4x11.
# d) W ostatnim kroku funkcja powinna budować w pętli model predykcyjny dla danej kombincaji parametrów i uzupełniać tabelę wynikową.      
#    Z racji tego, że nie stworzyliśmy na razie żadnego algorytmu ta część powinna działać następująco:
#    Każda pętla tworzy dwa podzbiory zdefiniowane przez wektor znajdujący się w liście z pkt b) dla danej kombinacji.
#    Do kolumn z miarami jakości wstawiane są wartości równe 0.
# e) Funkcja zwraca tabelę wynikową.
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
    f_klas['Czulosc'] = 0
    f_klas['Specyficznosc'] = 0
    f_klas['Jakosc?'] = 0
    f_klas['AUCW'] = 0
    f_klas['Specyficznosc'] = 0
    f_klas['MAPEW'] = 0
    f_klas['Jakosc'] = 0
    return(f_klas)
  }
}

CrossValidTune(data.frame(c(1:10),c(3:12)), 5, data.frame( a = c(1,2), b = c(1,1) ), 555)
