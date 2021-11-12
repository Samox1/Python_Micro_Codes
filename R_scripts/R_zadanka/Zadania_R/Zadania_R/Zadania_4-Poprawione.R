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

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) 
{
  if (any(is.na(X) == TRUE || is.na(y_tar) == TRUE)) 
  {
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
  else
  {
    X <- data.frame(X)
    X_norm <- data.frame(X)
    X_norm[,] <- 0
    
    nazwa <- vector()
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    column_names <- colnames(X)
      
    for(i in 1:ncol(X)) 
    {
      #message(typeof(X[,i]))
      
      nazwa <- append(nazwa, i)
      
      if (is.numeric(X[,i])) 
      {
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
        X_norm[,i] <- ((X[,i] - min(X[,i])) / (max(X[,i]) - min(X[,i]))) * (XmaxNew - XminNew) + XminNew
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) | is.factor(X[,i]))
      {
        X_norm[,i] <- X[,i]
        minOrg <- append(minOrg, NA)
        maxOrg <- append(maxOrg, NA)
      }
      else
      {
          message(paste0("Wartosci niepoprawne w X, kolumna: ", name))
      }
    }
      
    names(maxOrg) <- nazwa
    names(minOrg) <- nazwa
    attr(X_norm, 'minOrg') <- minOrg
    attr(X_norm, 'maxOrg') <- maxOrg
    attr(X_norm, 'minmaxNew') <- minmaxNew
      
    knn <- list()
    knn[["X"]] <- X_norm
    knn[["y"]] <- y_tar
    knn[["k"]] <- k
      
    }
  
  return(knn)
}


# --- test 1
fac1 <- 1:10
fac2 <- 11:20
fac3 <- 21:30

X1 <- cbind(fac1,fac2)
X1 <- as.matrix(X1)

X2 <- data.frame(as.factor(fac1), as.factor(fac2), as.factor(fac3))

y_tar1 <- 1:10
y_tar2 <- as.factor(c(0,0,0,0,0,1,1,1,1,1))
y_tar3 <- as.factor(c(0,0,0,1,1,1,2,2,2,2))
k = 2
XminNew <- 0
XmaxNew <- 1

tabelka1 <- KNNtrain(X1, y_tar1, k, XminNew, XmaxNew)
tabelka2 <- KNNtrain(X2, y_tar2, k, XminNew, XmaxNew)
tabelka3 <- KNNtrain(X2, y_tar3, k, XminNew, XmaxNew)
tabelka1
tabelka2
attributes(tabelka1$X)
attributes(tabelka2$X)
# --- koniec testu 1





### Zad. 2 ###


#### --------------- ####
#### - NOWA WERSJA - ####
#### --------------- ####


KNNpred <- function(KNNmodel, X) 
{
  
  if (is.na(KNNmodel) == TRUE || is.na(X) == TRUE) 
  {
    stop("Niekompletne dane!")
  }
  else if(ncol(KNNmodel$X) != ncol(X)) 
  {
    stop("Dane uczace z modelu nie zgadzaja sie z danymi wejsciowymi!")
  }
  else
  {
    # c) Normalizacja "X" przy pomocy atrybutów z "KNNmodel$X"
    if(!is.data.frame(X))
    {
      X <- data.frame(X)
    }
    
    X_znormalizowane <- matrix(0,nrow(X), ncol(X))
    X_znormalizowane <- data.frame(X_znormalizowane)
    
    kolumny_numeryczne <- 0
    kolumny_factor_order <- 0
    kolumny_factor <- 0
    
    for (i in 1:ncol(X)) 
    {
      if(is.numeric(X[,i])){
        kolumny_numeryczne <- kolumny_numeryczne + 1
        X_znormalizowane[,i] <- ((X[,i] - as.numeric(attributes(KNNmodel$X)$minOrg[i])) / (as.numeric(attributes(KNNmodel$X)$maxOrg[i]) - as.numeric(attributes(KNNmodel$X)$minOrg[i]))) * (as.numeric(attributes(KNNmodel$X)$minmaxNew[2]) - as.numeric(attributes(KNNmodel$X)$minmaxNew[1])) + as.numeric(attributes(KNNmodel$X)$minmaxNew[1])
      }
      else if(is.factor(X[,i]))
      {
        kolumny_factor <- kolumny_factor + 1
        X_znormalizowane[,i] <- X[,i]
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]))
      {
        kolumny_factor_order <- kolumny_factor_order + 1
        X_znormalizowane[,i] <- X[,i]
      }
    }
    
    # return(X_znormalizowane)
    
    
    n_wierszy_model = nrow(KNNmodel$X)
    n_kolumn_model = ncol(KNNmodel$X)

    n_wierszy_znorm = nrow(X_znormalizowane)
    n_kolumn_znorm = ncol(X_znormalizowane)

    odleglosc <- matrix(0, n_wierszy_model, n_wierszy_znorm)

    if(kolumny_numeryczne == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[ i, j ] <- sqrt( sum( (KNNmodel$X[i,] - X_znormalizowane[j,])^2 ) )
        }
      }
    }
    else if(kolumny_factor == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[i, j] <- ( (sum(KNNmodel$X[i,] != X_znormalizowane[j,])) / n_kolumn_znorm )
          # odleglosc[i, j] <- ( (sum(KNNmodel$X[i,] == X_znormalizowane[j,])) / n_kolumn_znorm )
        }
      }
    }
    else if(kolumny_factor_order == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          for(k in 1:n_kolumn_znorm)
          {
            unikalne <- length(unique(X_znormalizowane[,k]))
            odleglosc[i, j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_znormalizowane[i,]))  / (unikalne - 1)) )
          }
        }
      }
    }
    else
    {
      c("odległość Gowera")
    }

    # return(odleglosc)
    

    if(is.numeric(KNNmodel$y))
    {
      predykcja <- double(n_kolumn_znorm)

      for(i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order(odleglosc[,i])
        k_najblizej <- k_najblizej[1:KNNmodel$k]
        y_predykcja <- mean(KNNmodel$y[k_najblizej])
        predykcja[i] <- y_predykcja
      }

      return(predykcja)
    }
    else if(is.factor(KNNmodel$y))
    {
      predykcja <- as.data.frame(matrix(nrow = n_wierszy_znorm, ncol = length(unique(KNNmodel$y))+1))

      for (i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order( odleglosc[,i])
        k_najblizej <- k_najblizej[1:KNNmodel$k]

        if (length(unique(KNNmodel$y)) == 2)
        {
          names(predykcja) <- c('P', 'N', 'Klasa')
          pozytywna <- sum(KNNmodel$y[k_najblizej] == 1) / KNNmodel$k
          negatywna <- sum(KNNmodel$y[k_najblizej] == 0) / KNNmodel$k
          predykcja_klasy <- ifelse(pozytywna >= 0.5, 'P', 'N')
          predykcja[i, 1] <- pozytywna
          predykcja[i, 2] <- negatywna
          predykcja[i, 3] <- predykcja_klasy
        }
        else if (length(unique(KNNmodel$y)) > 2)
        {
          etykiety <- sort(unique(KNNmodel$y))
          names(predykcja) <- etykiety
          names(predykcja)[length(unique(KNNmodel$y))+1] <- 'Klasa'
          for (j in 1:length(etykiety))
          {
            pozytywna <- sum(KNNmodel$y[k_najblizej] == as.character(etykiety[j])) / KNNmodel$k
            predykcja[i, j] <- pozytywna
          }
          predykcja_klasy <- etykiety[which.max(predykcja[i,])]
          predykcja[i,'Klasa'] <- as.character(predykcja_klasy)
        }
      }
      return(predykcja)
    }
    else
    {
      stop("Dane y modelu sa niepoprawne!")
    }
  }
}

f1 <- sample(1:10, 10, replace = FALSE)
f2 <- f1 + 10
f3 <- f2 + 10
X1n <- cbind(f1,f2)
X1n <- as.matrix(X1n)
X2n <- data.frame(as.factor(f1), as.factor(f2), as.factor(f3))

KNNpred(tabelka1, X1n)
KNNpred(tabelka2, X2n)
KNNpred(tabelka3, X2n)
tabelka1$X
tabelka2$X

cbind(fac1, tabelka1$y, f1, KNNpred(tabelka1, X1n))

cbind(f1,KNNpred(tabelka2, X2n)[,3])

