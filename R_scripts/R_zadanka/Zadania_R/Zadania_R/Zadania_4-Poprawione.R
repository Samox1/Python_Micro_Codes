# Plik proszę nazwać numerem swojego indeksu.
# 
# Zadanie 1:
# a) Stwórz funkcję "KNNtrain" przyjmującą nastęujące parametry: "X", "y_tar", "k", "XminNew", "XmaxNew".
# b) Funkcja w pierwszym kroku powinna sprawdzać czy analiza jest możliwa do wykonania tj:
#    czy "X" oraz "y_tar" nie mają braków danych, czy "k" jest większe od 0, czy "X" jest macierzą lub ramką danych.
# c) W drugim kroku funkcja dokonuje normalizacji zmiennych z "X" do przedziału "XminNew", "XmaxNew". Jest to możliwe dla zmiennych na skali ilorazowej.
#    Nowa tabela powinna posiadać 3 atrubuty informujące o (każdy jest wektorem):
#      - wartościach minimalnych attr(*,"minOrg") dla każdej zmiennej,
#      - wartościach maksymalnych attr(*,"maxOrg") dla każdej zmiennej,
#      - nowych wartościch minimalnych i maksymalnych attr(*,"minmaxNew").
# d) W kolejnym kroku w obiekcie typu lista o trzech elementach "X","y","k", funkcja umieszcza odpowiednie obiekty.
# e) Funkcja powinna zwracać listę z pkt d).
# f) Funkcja będzie testowana tylko dla klasyfikacji binarnej i regresji.
# ***) Dla ambitnych: Funkcja może przyjmować dodatkowo parametr "metoda = brute". Oznacza to, że domyślnie nie następuje faza treningowa, 
#      lecz w przypadku przekazania argumentu "kdtree", funkcja przeorganizuję tabelę "X" w pomocniczą strukturę k-d tree. 
#      Taki zabieg będzie wpływał na sposób wykonania Zadania 2. Ta część może być napisana w C/C++. 
#      Struktura k-d tree jest możliwa do stworzenia dla zmiennych na skali ilorazowej.
# 
# Zadanie 2:
# a) Stwórz funkcję "KNNpred" przyjmującą nastęujące parametry: "KNNmodel", "X".
# b) Funkcja w pierwszym kroku powinna sprawdzać czy predykcja jest możliwa do wykonania tj:
#    czy "X" nie ma braków danych, czy wszystkie potrzebne zmienne/kolumny istnieją zarówno w "KNNmodel$X" jak i w "X".
# c) Następnie funkcja powinna normalizaowć dane z "X" uwzględniając informacje zawarte w atrybutach obiektu "KNNmodel$X".
# d) W zależności od typu zmiennych w obiektach "KNNmodel$X" i "X" odległość pomiędzy obserwacjami powinna być liczona odległością:
#    Euklidesa, Hamminga lub Gowera.
# e) Funkcja powinna następnie rozpoznawać czy mamy do czynienia z problemem regresji czy klasyfikacji, 
#    a następnie dokonywać odpowiedniej agregacji wyników (średnia lub głosowanie większościowe).
# f) Funkcja powinna zwracać:
#    - dla regresji: wektor z wartościami przewidywanymi.
#    - dla klasyfikacji: nazwaną ramkę danych o rozmiarze "n x k+1", np. dla wersji binarnej o etykietach "P", "N",
#      tabela wygląda następująco: data.frame( P = c(0.3,0.6), N = c(0.7,0.4), Klasa = c("N","P") ), 
#      tj. pierwsze dwie kolumny zawierają prawdopodobieństwo przynależności danej obserwacji do danej klasy (nazwy kolumn są ważne),
#      ostatnia kolumna wskazuję klasę - jest to factor o poziomach takich jak originalna zmienna "y_tar" z Zadania 1. 
# g) Funkcja będzie testowana dla problemu regresji i klasyfikacji binarnej na zbiorze danych zawierającym:
#    - tylko zmienne na skali ilorazowej.
#    - tylko zmienne na skali porządkowej.
#    - tylko zmienne na skali nominalnej.
#    - zmienne na skali mieszanej.
# ***) Dla ambitnych: W zależności od sposoby wykonania Zadania 1, funkcja wyszukuje najbliższych sąsiadów na podstawie struktury k-d tree.



#### OCENA - 38%

library(caret)


### Zad. 1 ###

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) {
  if (any(is.na(X) == TRUE || is.na(y_tar) == TRUE)) {
    stop("Niekompletne dane!")
  }
  else if(k <= 0)
  {
    stop("Za male k")
  }
  else if(XminNew == XmaxNew)
  {
    stop("Nowe granice sa takie same!")
  }
  else if((is.matrix(X) == FALSE & is.data.frame(X) == FALSE))
  {
    stop("Dane nie sa macierza lub ramka danych!")
  }
  else{
  {
    if(is.matrix(X))
    {
      X <- data.frame(X)
    }
    
    # Sprawdzic KOD z Projekt_R_knn_Tree_SVM -> funkcje.R
    
    nazwa <- vector()
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    column_names <- colnames(X)
      
      for (name in column_names) {
        if (is.numeric(X[name])) {
          nazwa <- append(nazwa, name)
          minOrg <- append(minOrg, min(X[name]))
          maxOrg <- append(maxOrg, max(X[name]))
          X[name] <- ((X[name] - min(X[name])) / (max(X[name]) - min(X[name]))) * (XmaxNew - XminNew) + XminNew
        }
        else if(is.factor(X[name]) & is.ordered(X[name]) | is.factor(X[name])){
          X_norm[,i] <- X[,i]
          minOrg[i] <- append(minOrg, NA)
          maxOrg[i] <- append(maxOrg, NA)
        }
        else{
          message(paste0("Wartosci niepoprawne w X, kolumna: ", name))
        }
      }
      
      names(maxOrg) <- nazwa
      names(minOrg) <- nazwa
      attr(X, 'minOrg') <- minOrg
      attr(X, 'maxOrg') <- maxOrg
      attr(X, 'minmaxNew') <- minmaxNew
      
      knn <- list()
      knn[["X"]] <- X
      knn[["y"]] <- y_tar
      knn[["k"]] <- k
      
    }
    return(knn)
  }
}


# - test 1
fac1 <- sample(1:6, 10, replace = TRUE)
fac2 <- sample(2:4, 10, replace = TRUE)
mata <- cbind(fac1,fac2)
X <- matrix(mata, ncol = ncol(mata), nrow = nrow(mata))
# X[4,2] <- 'a'
y_tar <- sample(0:1, 10, replace = TRUE)
k = 2
XminNew <- 0
XmaxNew <- 1

tabelka <- KNNtrain(X, y_tar, k, XminNew, XmaxNew)
tabelka
attributes(tabelka[["X"]])
# - koniec testu 1




### Zad. 2 ###

d_euklides <- function(x_i, x_n) {
  return(sqrt(sum((x_i - x_n) ^ 2)))
}

d_hamming <- function(x_i, x_n, p) {
  suma <- 0
  for (j in 1:p) {
    if (x_i[j] == x_n[j]) {
      a[j] = 1
    }
    else{
      a[j] = 0
    }
    suma <- suma + a[j]
  }
  return(suma / p)
}

d_gower <- function(x_i, x_n, p, skala) {
  G <- 0
  suma_G <- 0
  if (skala == 'ilorazowa') {
    for (j in 1:p) {
      G[j] <- (abs(x_i[j] - x_n[j])) / (max(x_i) - min(x_i))
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
  else if (skala == 'porz?dkowa') {
    z <- 0
    for (j in 1:p) {
      z[j] <- (x_i[j] - 1) / (max(x_i - 1))
      G[j] <- (abs(z[j] - p)) / (max(z[j]) - min(z[j]))
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
  else if (skala == 'nominalna') {
    for (j in 1:p) {
      if (x_i[j] == x_n[j]) {
        G[j] = 1
      }
      else{
        G[j] = 0
      }
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
}


KNNpred <- function(KNNmodel, X) {
  if (is.na(KNNmodel) == TRUE || is.na(X) == TRUE) {
    stop("Niekompletne dane")
  }
  else
  {
    dFrame <- as.data.frame(KNNmodel$X)
    if (nrow(dFrame) == nrow(X)) {
      #regresja
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      odl_eukl <- matrix(0, nTrain, nPred)
      odl_hamming <- matrix(0, nTrain, nPred)
      for (i in 1:nTrain) {
        for (j in 1:nPred) {
          odl_eukl[i, j] <- d_euklides(KNNmodel$X[i, ], X[j, ])
          odl_hamming[i, j] <- d_hamming(KNNmodel$X[i, ], X[j, ])
          if (ncol(Filter(is.numeric, X)) != 0 &
              ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'ilorazowa'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'nominalna'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'porz?dkowa'
          }
          odl_gower[i, j] <- d_gower(KNNmodel$X[i, ], X[j, ], skala)
        }
      }
      pred <- double(nPred)
      for (i in 1:nPred) {
        kNaj_eukl <- order(odl_eukl[, i])
        kNaj_hamming <- order(odl_hamming[, i])
        kNaj_gower <- order(odl_gower[, i])
        kNaj_eukl <- kNaj_eukl[1:KNNmodel$k]
        kNaj_hamming <- kNaj_hamming[1:KNNmodel$k]
        kNaj_gower <- kNaj_gower[1:KNNmodel$k]
        y_hat_eukl <- mean(KNNmodel$y[kNaj_eukl])
        y_hat_hamming <- mean(KNNmodel$y[kNaj_hamming])
        y_hat_gower <- mean(KNNmodel$y[kNaj_gower])
        pred_eukl[i] <- y_hat_eukl
        pred_hamming[i] <- y_hat_hamming
        pred_gower[i] <- y_hat_gower
      }
      
      if (ncol(Filter(is.numeric, X)) != 0 &
          ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_eukl)
      }
      else if (ncol(Filter(is.numeric, X)) == 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_hamming)
      }
      else if (ncol(Filter(is.numeric, X)) != 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) != 0) {
        return(pred_gower)
      }
    }
  }
}




#### --------------- ####
#### - NOWA WERSJA - ####
#### --------------- ####


KNNpred <- function(KNNmodel, X) {
  if (is.na(KNNmodel) == TRUE || is.na(X) == TRUE) 
  {
    stop("Niekompletne dane!")
  }
  else if((nrow(KNNmodel$X) != nrow(X)) || ncol(KNNmodel&X) != ncol(X)) 
  {
    stop("Dane z modelu nie zgadzaja sie z danymi wejsciowymi!")
  }
  else
  {
        
    
      #regresja
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      odl_eukl <- matrix(0, nTrain, nPred)
      odl_hamming <- matrix(0, nTrain, nPred)
      for (i in 1:nTrain) {
        for (j in 1:nPred) {
          odl_eukl[i, j] <- d_euklides(KNNmodel$X[i, ], X[j, ])
          odl_hamming[i, j] <- d_hamming(KNNmodel$X[i, ], X[j, ])
          if (ncol(Filter(is.numeric, X)) != 0 &
              ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'ilorazowa'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'nominalna'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'porz?dkowa'
          }
          odl_gower[i, j] <- d_gower(KNNmodel$X[i, ], X[j, ], skala)
        }
      }
      pred <- double(nPred)
      for (i in 1:nPred) {
        kNaj_eukl <- order(odl_eukl[, i])
        kNaj_hamming <- order(odl_hamming[, i])
        kNaj_gower <- order(odl_gower[, i])
        kNaj_eukl <- kNaj_eukl[1:KNNmodel$k]
        kNaj_hamming <- kNaj_hamming[1:KNNmodel$k]
        kNaj_gower <- kNaj_gower[1:KNNmodel$k]
        y_hat_eukl <- mean(KNNmodel$y[kNaj_eukl])
        y_hat_hamming <- mean(KNNmodel$y[kNaj_hamming])
        y_hat_gower <- mean(KNNmodel$y[kNaj_gower])
        pred_eukl[i] <- y_hat_eukl
        pred_hamming[i] <- y_hat_hamming
        pred_gower[i] <- y_hat_gower
      }
      
      if (ncol(Filter(is.numeric, X)) != 0 &
          ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_eukl)
      }
      else if (ncol(Filter(is.numeric, X)) == 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_hamming)
      }
      else if (ncol(Filter(is.numeric, X)) != 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) != 0) {
        return(pred_gower)
      }
    }
  }
}


