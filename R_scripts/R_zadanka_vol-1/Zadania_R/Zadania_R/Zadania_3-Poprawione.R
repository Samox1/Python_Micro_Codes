# Plik proszę nazwać numerem swojego indeksu.
# 
# Zadanie 1:
# a) Stwórz funkcję "ModelOcena" przyjmującą nastęujące parametry: "y_tar" (rzeczywista), "y_hat" (prognoza).
# b) Funkcja w pierwszym kroku powinna rozpoznawać czy mamy do czynienia z problemem regresji czy klasyfikacji. 
#    y_tar: regresja -> numeric, klasyfikacja -> factor.
# c) W zależności od problemu funkcja szacować powinna różnego rodzaju błędy:
#    Regresja: MAE, MSE, MAPE.
#    Klasyfikacja: AUC (szacowanie metodą trapezów), macierz klasyfikacji (punkt odcięcia wyznaczany jest poprzez index Youdena), 
#                  Czułość, Specyficzność, Jakość.
# d) Dla czytelności kodu, wszystkie miary powinny być liczone w oparciu o zewnętrzne funkcje (dowolne nazwy), 
#    których definicje znajdować się powinny przed definicją funkcji "ModelOcena". 
# e) Funkja powinna zwracać wyniki w formie: 
#    Regresja: nazwany wektor (MAE, MSE, MAPE) o trzech elementach.
#    Klasyfikacja: nazwana lista (Mat, J, Miary) o trzech elementach:
#                  Mat = macierz klasyfikacji, w wierszach znajdują się wartości "y_tar" a w kolumnach wartości "y_hat",
#                        nazwy wierszy i kolumn muszą być zgodne z dostępnymi etykietami klas.
#                  J = wartość indexu Youdena,
#                  Miary = nazwany wektor o elementach AUC, Czułość, Specyficzność, Jakość.
# f) Funkcja będzie testowana tylko dla klasyfikacji binarnej i regresji.
# 
# Zadanie 2:
# a) Stwórz funkcję "CrossValidTune" przyjmującą nastęujące parametry: "dane", "kFold", "parTune", "seed". 
#    W skrócie: funkcja powinna krosswalidacyjnie tunować parametry danego algorytu. 
# b) Funkcja powinna w pierwszym kroku stworzyć listę przechowującą informację, które obserwacje posłużą jako zbiór treningowy,
#    a które wejdą do zbioru walidacyjnego. Ilość elementów listy zdefiniowana jest przez parametr "kFold" (liczba podzbiorów walidacyjnych).
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


### OCENA - 38%


### Zad. 1 ###

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


Czulosc <- function(Mat){
  return((Mat[1] / (Mat[1] + Mat[3])))
}

Specyficznosc <- function(Mat){
  return((Mat[4] / (Mat[4] + Mat[2])))
}

Jakosc <- function(Mat){
  return(((Mat[1] + Mat[4]) / (Mat[1] + Mat[2] + Mat[3] + Mat[4])))
}


ModelOcena1 <- function(y_tar, y_hat){
  if(is.numeric(y_tar)){
    regresja <- c("MAE" = MAE(y_tar, y_hat), "MSE" = MSE(y_tar, y_hat), "MAPE" = MAPE(y_tar, y_hat))
    return(regresja)
  }
  else if(is.factor(y_tar)){
    Mat <- table(y_tar, y_hat = ifelse(y_hat <= Youden(y_tar, y_hat), 0, 1))
    miary <- c( "AUC" = AUC(y_tar, y_hat), "Czulosc" = Czulosc(Mat), "Specyficznosc" = Specyficznosc(Mat), "Jakosc" = Jakosc(Mat))
    klasyfikacja <- list("Mat" = Mat, "J" = Youden(y_tar, y_hat), "Miary" = miary)
    return(klasyfikacja)
  }
  else{
    c("Dane niepoprawne")
  }
}


### Test na malych zbiorach

#Regresja
set.seed(123)
y_tar_reg <- rnorm(20)
y_hat_reg <- y_tar_reg + 0.3
ocena_regresja <- ModelOcena1(y_tar_reg,y_hat_reg)
print(ocena_regresja)

#Klasyfikacja
y_tar_klas <- as.factor(c(0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,0))
y_hat_klas <- (length(y_tar_klas):1) / length(y_tar_klas)
ocena_klasyfikacja <- ModelOcena1(y_tar_klas,y_hat_klas)
print(ocena_klasyfikacja)



### Zad. 2 ###

CrossValidTune <- function(dane, kFold, parTune, seed){
  set.seed(seed)
  k = nrow(dane)
  wektor = c()
  lista = list()
  
  # Podzial na podzbior treningowy i walidacyjny
  for (j in 1:kFold){
    indxT <- sample( x = 1:k, size = round((1-1/kFold) * k), replace = F )
    
    wektor[indxT] <- 1
    wektor[-indxT] <- 2
    lista <- append(lista, list(wektor))
  }
  #print(lista)
  
  
  # Przygotowanie ramki na wyniki i kombinacje
  ramka_r <- data.frame( kFold_par = c(1,2,1,2))
  
  for (i in 1:(length(parTune)))
  {
    ramka_r[i+1] = parTune[i]
  }
  
  model_miary_trening <- ModelOcena(dane[,1], dane[,2])
  model_miary_walidacja <- ModelOcena(dane[,1], dane[,2])
  
  if(is.numeric(dane[,1])){
    f_reg <- ramka_r
    
    #f_reg['MAEt'] = model_miary_trening[1]
    #f_reg['MSEt'] = model_miary_trening[2]
    #f_reg['MAPEt'] = model_miary_trening[3]
    #f_reg['MAEw'] = model_miary_walidacja[1]
    #f_reg['MSEw'] = model_miary_walidacja[2]
    #f_reg['MAPEw'] = model_miary_walidacja[3]
    
    f_reg['MAEt'] = 0
    f_reg['MSEt'] = 0
    f_reg['MAPEt'] = 0
    f_reg['MAEw'] = 0
    f_reg['MSEw'] = 0
    f_reg['MAPEw'] = 0
    return(f_reg)
  }
  else if(is.factor(dane[,1])){
    f_klas <- ramka_r
    
    #f_klas['AUC_T'] = model_miary_trening$Miary["AUC"]
    #f_klas['Czulosc_T'] = model_miary_trening$Miary["Czulosc"]
    #f_klas['Specyficznosc_T'] = model_miary_trening$Miary["Specyficznosc"]
    #f_klas['Jakosc_T'] = model_miary_trening$Miary["Jakosc"]
    #f_klas['AUC_W'] = model_miary_walidacja$Miary["AUC"]
    #f_klas['Czulosc_W'] = model_miary_walidacja$Miary["Czulosc"]
    #f_klas['Specyficznosc_W'] = model_miary_walidacja$Miary["Specyficznosc"]
    #f_klas['Jakosc_W'] = model_miary_walidacja$Miary["Jakosc"]
    
    f_klas['AUC_T'] = 0
    f_klas['Czulosc_T'] = 0
    f_klas['Specyficznosc_T'] = 0
    f_klas['Jakosc_T'] = 0
    f_klas['AUC_W'] = 0
    f_klas['Czulosc_W'] = 0
    f_klas['Specyficznosc_W'] = 0
    f_klas['Jakosc_W'] = 0
    
    return(f_klas)
  }
}



### Test funkcji 
CrossValidTune(data.frame(c(1:10),c(3:12)), 2, data.frame( a = c(1,2), b = c(1,1) ), 555)
CrossValidTune(data.frame(as.factor(c(0,1,0,1,0,1,0,1,0,0)),c(1.00,0.95,0.90,0.85,0.30,0.25,0.20,0.15,0.10,0.05)), 2, data.frame( a = c(1,2), b = c(1,1) ), 555)






