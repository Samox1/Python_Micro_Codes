# Plik prosz? nazwa? numerem swojego indeksu.
# 
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

install.packages("pROC")
library("pROC")

MAE <- function(y_t, y_h){
  return (mean( abs(y_t - y_h)))
}
#MAE(y_hat, y_hat)
#MAE(y_tar, y_hat)

MSE <- function(y_t, y_h){
  return (mean((y_t - y_h)^2))
}
#MSE(y_hat, y_hat)
#MSE(y_tar, y_hat)


MAPE <- function(y_t, y_h){
  return (mean(abs((y_t - y_h)/y_t)))
}
#MAPE(y_hat, y_hat)
#MAPE(y_tar, y_hat)



#c Klasyfikacja: AUC (szacowanie metod? trapez?w), macierz klasyfikacji (punkt odci?cia wyznaczany jest poprzez index Youdena),  Czu?o??, Specyficzno??, Jako??

#AUC = -(1/2)*suma X od i=2 do n (Czu?o?? i-1 * Specyficzno?? i - Czu?o?? i * Specyficzno?? i-1)
AUC  <- function(y_t, y_h){
  krzywa_roc <- roc(y_t, y_h)
  
  y <- rev(krzywa_roc$sensitivities)
  x <- rev(1 - krzywa_roc$specificities)
  
  # diff difference czyli r??niczka [0,1] dla ROC(t)
  dy <- c(diff(y), 0)
  dx <- c(diff(x), 0)
  
  # pole trapezu ( a + b) * h) / 2
  suma_trapezow <- sum(y * dx) + sum(dy * dx)/2
  return(suma_trapezow)
}


install.packages("ROCR")
library("ROCR")
library("dplyr")

Youden_index <- function(y_tar, y_hat){
  roc_wyzn <- roc(y_tar, y_hat)
  
  y <- roc_wyzn$sensitivities #czułość
  x <- roc_wyzn$specificities #specyficzność
  
  # J = argt Czu?o?? + Specyficzno?? - 1
  J_max <- 0
  for ( i in 1:length(y)){
    J <- y[i] + x[i] -1
    if(J > J_max) {
      J_max = J
    }
  }
  return(J_max)
}
#Youden_index(y_tar, y_hat)


y_tar <- rnorm( 30 )
y_hat <- y_tar + 0.3
table(y_tar, y_tar)

Mat <- table(y_tar, 
             y_hat = ifelse(y_hat <= Youden_index(y_tar, y_hat), 0,1)) 
#Mat

Czulosc <- function(Mat){# TP / (TP+FN)
  wynik_cz <- (Mat[1] / (Mat [ 1 ] + Mat[3]))
  return (wynik_cz)
}

Specyficznosc <- function (Mat){ # TN / (FP + TN)
  wynik_s <- Mat[4] / (Mat[2] + Mat[4])
  return(wynik_s)
}

Jakosc <- function(Mat){ # TP + TN / WSZYSTKO
  wynik_j <- sum(diag(Mat))/sum(Mat)  
  return(wynik_j)
}


ModelOcena <- function (y_tar, y_hat){
  if (is.numeric(y_tar))
  { #b
    regresja_wynik <- c("MAE" = MAE(y_tar, y_hat),
                        "MSE" = MSE(y_tar, y_hat),
                        "MAPE" = MAPE( y_tar, y_hat)) #nazwany wektor (MAE, MSE, MAPE) o trzech elementach
    return(regresja_wynik)
    
  }
  else if(is.factor(y_tar)){ #b
    klasyfikacja_wynik <- list( Mat,
                                Youden_index(y_tar, y_hat),
                                c("AUC" = AUC(y_tar, y_hat),
                                  "Czulosc" = Czulosc(Mat),
                                  "Specyficznosc" = Specyficznosc( Mat),
                                  "Jakosc" = Jakosc(Mat))) #nazwana lista (Mat, J, Miary)
    return(klasyfikacja_wynik)
    
  }
  else
  { #b
    return("Niepoprawne wprowadzenie danych, problem nie jest problemem regresi lub klasyfikacji. Wybierz inny zbior danych.")
  }
  
}#ModelOcena

# Zadanie 2:
# a) Stw?rz funkcj? "CrossValidTune" przyjmuj?c? nast?uj?ce parametry: "dane", "kFold", "parTune", "seed". 
#    W skr?cie: funkcja powinna krosswalidacyjnie tunowa? parametry danego algorytu. 


# b) Funkcja powinna w pierwszym kroku stworzy? list? przechowuj?c? informacj?, kt?re obserwacje pos?u?? jako zbi?r treningowy,
#    a kt?re wejd? do zbioru walidacyjnego. 

#    Ilo?? element?w listy zdefiniowana jest przez parametr "kFold" (liczba podzbior?w walidacyjnych).
#    Ka?dy element listy jest wektorem o tej samej d?ugo?ci, r?wnej nrow("dane").

#    Ka?dy z wektor?w zawiera albo liczb? 1 (obserwacja jest w zbiorze treningowym) albo 2 (obserwacja jest w zbiorze walidacyjnym). 
#    Przyk?ad: list( c(1,2,1,1), c(2,1,1,2) ) - oznacza, ?e mamy doczynienia z 2-krotn? walidacj? na zbiorze z 4 obserwacjami, 
#    gdzie dla pierwszej iteracji tylko jeden element jest w podzbiorze walidacyjnym.

#    Losowanie rozpoczyna si? od ustawienia ziarna na warto?? "seed".



# c) W kolejnym kroku funkcja powinna stworzy? ramk? danych, w kt?rej przechowywane b?d? wyniki oraz kombinacje parametr?w.
#    Liczba wierszy i kolumn zale?y od zagadnienia (klasyfikacja, regresja) oraz od liczby tunowanych parametr?w "parTune" i "kFold":
#    Przyk?ad: "parTune" = data.frame( a = c(1,2), b = c(1,1) ) - oznacza, ?e algorytm ma 2 parametry do tunowania,
#              Dla "kFold" = 2 oraz "parTune", kombinacja parametr?w to data.frame( "kFold" = c(1,2,1,2), a = c(1,2), b = c(1,1) ).
#              Kolejne kolumny tabeli wynikowej powinny stanowi? miary uzyskane dzi?ki funkcji "ModelOcena".
#              Regresja: MAEt, MSEt, MAPEt, MAEw, MSEw, MAPEw - ozanczaj? miary dla zbioru treningowego i walidacyjnego.
#                        Finalnie tabele jest rozmiaru 4x9.
#              Klasyfikacja: AUCT, Czu?o??T, Specyficzno??T, Jako??T, AUCW, Specyficzno??W, MAPEW, Jako??W - j.w.
#                            Finalnie tabele jest rozmiaru 4x11.


# d) W ostatnim kroku funkcja powinna budowa? w p?tli model predykcyjny dla danej kombincaji parametr?w i uzupe?nia? tabel? wynikow?.      
#    Z racji tego, ?e nie stworzyli?my na razie ?adnego algorytmu ta cz??? powinna dzia?a? nast?puj?co:
#    Ka?da p?tla tworzy dwa podzbiory zdefiniowane przez wektor znajduj?cy si? w li?cie z pkt b) dla danej kombinacji.
#    Do kolumn z miarami jako?ci wstawiane s? warto?ci r?wne 0


# e) Funkcja zwraca tabel? wynikow?.

# PIERWSZY ODDANY KOD Z BŁĘDEM, POD NIM NOWY
# CrossValidTune <- function(dane, kFold, parTune, seed)
# {
#     set.seed(seed) #losowe
#     n = nrow(dane) # dlugosc wektora wchodzacego w sklad listy
#     wektor <- c() #pusty wektor przypisywania
#     lista <- list()
#
#
#     for (j in 1:kFold)
#     {
#           index <- sample( x = 1:n,
#                            size = round((1-1/kFold) * n),
#                            replace = F )#bez powtorzen, tylko raz wybieramy obiekt
#           index_sort = sort(index, decreasing = FALSE) #sort niemalej?co
#
#           licznik <- 1
#
#
#           while (licznik <= length(index_sort))
#           {
#
#             for (i in 1:n)
#             {
#               if (is.na(index_sort[licznik]))  #oznaczam NA
#               {
#                 print('NA')
#               }
#               else if (index_sort[licznik] == i)
#               {
#                 wektor[i] = 1
#                 licznik = licznik + 1
#               }
#               else i
#               {
#                 wektor[i] == 2
#               }
#             }
#             # ifelse(index_sort[licznik] == i,
#             #        wektor[i] = 1
#             #        licznik = licznik + 1,
#             #        wektor[i] == 2
#             #        )
#
#           }
#
#           lista[[j]] = c(wektor) #przypisanie wektora do miejsca w li?cie
#     }


    # indxT <- sample(1:n)
    # indxT <-sample(x= 1:n,
    #                size= (1-(1/kFold))*n,
    #                replace = F)
    # indxT
#
#
#       ramka_danych <- data.frame(parTune)
#
#       for( i in 1:length(parTune))
#       {
#         ramka_danych[ i+1 ] = parTune [ i ]
#       }
#
#
#       if(is.numeric(ramka_danych))
#       {
#         wynik_regresja <- data.frame(parTune,
#                                      MAEt = 0, MSEt = 0, MAPEt = 0,
#                                      MAEw = 0, MSEw = 0, MAPEw = 0)
#         return(wynik_regresja)
#       }
#
#
#       else if(is.factor(ramka_danych))
#       {
#         wynik_klasyfikacja <- data.frame(parTune,
#                                          AUCT = 0, CzuloscT = 0, SpecyficznoscT = 0, JakoscT = 0,
#                                          AUCW = 0, MAPEW = 0, SpecyficznoscW = 0, JakoscW = 0)
#         return(wynik_klasyfikacja)
#       }
#
#       else
#       {
#         return("Niepoprawne wprowadzenie danych, problem nie jest problemem regresi lub klasyfikacji. Wybierz inny zbior danych.")
#       }
#
# }

CrossValidTune <- function(dane, kFold, parTune, seed)
  {
      set.seed(seed) #losowe
      n = nrow(dane) # dlugosc wektora wchodzacego w sklad listy
      wektor <- c() #pusty wektor przypisywania
      lista <- list()
      
      for ( i in 1:kFold){
        #dziele probe na zbior testpwy i walidacyjny
        index <- sample( x = 1:n, size = round((1-1/kFold) * n), replace = F )#bez powtorzen, tylko raz wybieramy obiekt
        
        index_sort = sort(index, decreasing = FALSE) #sort niemalej?co
        lista[[i]] <- sample(1:nrow(dane),size = nrow(dane),replace = F)
        
        for(j in 1:n){
          for(k in 1:length(index_sort)){
            if(j == index_sort[k]){
              lista[[i]][[j]] = 2
              break
            }
            else{
              lista[[i]][[j]] = 1
            }
          }
        }
   
      }
      
      k <- rep(c(1:kFold), times=length(parTune))
      wyniki <- data.frame(k, parTune)
      
      if(is.numeric(dane[,1])){
        wyniki_regresja <- data.frame(wyniki, MAEt=0, MSEt=0, MAPEt=0, MAEw=0, MSEw=0, MAPEw=0  )
        return(wyniki_regresja)
      }
      else if(is.factor(dane[,1])){
        wyniki_klasyfikacja <- data.frame(wyniki, AUCT=0, CzułośćT=0, SpecyficznośćT=0, JakośćT=0,
                                          AUCW=0, SpecyficznośćW=0, MAPEW=0, JakośćW=0)
        return(wyniki_klasyfikacja)
      }
      else{
        return("WPROWADZENIE NIEPOPRAWNYCH DANYCH!")
      }

      
}#funkcja glownaCrossValidTune

