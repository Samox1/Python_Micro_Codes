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


### Zad. 1 ###

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) 
{
  if (any(is.na(X) == TRUE) || any(is.na(y_tar) == TRUE)) 
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
      
    for(i in 1:ncol(X)) 
    {
      nazwa <- append(nazwa, i)
      
      if (is.numeric(X[,i])) 
      {
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
        X_norm[,i] <- ((X[,i] - min(X[,i])) / (max(X[,i]) - min(X[,i]))) * (XmaxNew - XminNew) + XminNew
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) | is.factor(X[,i]))
      {
        minOrg <- append(minOrg, NA)
        maxOrg <- append(maxOrg, NA)
        X_norm[,i] <- X[,i]
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
      
    return(knn)
  }
}


# --- test 1
fac1 <- sample(1:10, 10, replace = FALSE)
liczby <- fac1 + 10
fac2 <- sample(1:10, 10, replace = FALSE)

X <- data.frame(as.factor(fac1), liczby, as.ordered(as.factor(fac2)))
y_tar <- as.factor(c(0,0,0,0,0,1,1,1,1,1))

tabelka <- KNNtrain(X, y_tar, 2, 0, 1)
tabelka
attributes(tabelka$X)
# --- koniec testu 1





### Zad. 2 ###

d_euklides <- function(x_i, x_n)
{
  return(sqrt(sum((x_i - x_n)^2)))
}

d_hamming <- function(x_i, x_n, p)
{
  return((sum(x_i != x_n)) / p)
}

d_porzadkowa <- function(x_i, x_n, unikalne)
{
  return(sum(abs(as.numeric(x_i) - as.numeric(x_n))  / (unikalne - 1)))
}



KNNpred <- function(KNNmodel, X) 
{
  if (any(is.na(KNNmodel) == TRUE) || any(is.na(X) == TRUE)) 
  {
    stop("Niekompletne dane!")
  }
  else if((is.matrix(X) == FALSE & is.data.frame(X) == FALSE))
  {
    stop("Dane testowe nie sa typu MATRIX czy DATA.FRAME!")
  }
  else if(ncol(KNNmodel$X) != ncol(X) || colnames(KNNmodel$X) != colnames(X)) 
  {
    stop("Dane uczace z modelu nie zgadzaja sie z danymi wejsciowymi!")
  }
  else
  {
    if(!is.data.frame(X))
    {
      X <- data.frame(X)
    }
    
    X_znormalizowane <- matrix(0,nrow(X), ncol(X))
    X_znormalizowane <- data.frame(X_znormalizowane)
    
    n_wierszy_model = nrow(KNNmodel$X)
    n_kolumn_model = ncol(KNNmodel$X)
    
    n_wierszy_znorm = nrow(X_znormalizowane)
    n_kolumn_znorm = ncol(X_znormalizowane)
    
    kolumny_numeryczne <- 0
    kolumny_factor_order <- 0
    kolumny_factor <- 0
    
    for (i in 1:n_kolumn_znorm) 
    {
      if(is.numeric(X[,i]))
      {
        X_znormalizowane[,i] <- ((X[,i] - as.numeric(attributes(KNNmodel$X)$minOrg[i])) / (as.numeric(attributes(KNNmodel$X)$maxOrg[i]) - as.numeric(attributes(KNNmodel$X)$minOrg[i]))) * (as.numeric(attributes(KNNmodel$X)$minmaxNew[2]) - as.numeric(attributes(KNNmodel$X)$minmaxNew[1])) + as.numeric(attributes(KNNmodel$X)$minmaxNew[1])
        kolumny_numeryczne <- kolumny_numeryczne + 1
      }
      else if(is.factor(X[,i]))
      {
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor <- kolumny_factor + 1
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]))
      {
        X_znormalizowane[,i] <- X[,i]
        kolumny_factor_order <- kolumny_factor_order + 1
      }
    }
    
    
    odleglosc <- matrix(0, n_wierszy_model, n_wierszy_znorm)

    if(kolumny_numeryczne == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[i,j] <- d_euklides(KNNmodel$X[i,], X_znormalizowane[j,])
        }
      }
    }
    else if(kolumny_factor == n_kolumn_znorm)
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          odleglosc[i,j] <- d_hamming(KNNmodel$X[i,], X_znormalizowane[j,], n_kolumn_znorm)
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
            unikalne <- nlevels(X_znormalizowane[,k])
            odleglosc[i,j] <- d_porzadkowa(KNNmodel$X[i,], X_znormalizowane[j,], unikalne)
          }
        }
      }
    }
    else
    {
      for(i in 1:n_wierszy_model)
      {
        for(j in 1:n_wierszy_znorm)
        {
          temp <- 0
          
          for(k in 1:n_kolumn_znorm)
          {
            if(is.numeric(X_znormalizowane[,k]))
            {
              max_k <- as.numeric(attributes(KNNmodel$X)$maxOrg[k])
              min_k <- as.numeric(attributes(KNNmodel$X)$minOrg[k])
              temp <- temp + (abs(KNNmodel$X[i,k] - X_znormalizowane[j,k]) / (max_k -  min_k)) 
            }
            else if(is.factor(X_znormalizowane[,k]))
            {
              if(KNNmodel$X[i,k] != X_znormalizowane[j,k])
              {
                temp <- temp + 1
              }
            }
            else if(is.factor(X_znormalizowane[,k]) & is.ordered(X_znormalizowane[,k]))
            {
              z_i <- (i - 1) / (n_wierszy_model - 1)
              z_n <- (j - 1) / (n_wierszy_znorm - 1)
              temp <- temp + (abs(z_i - z_n) / (n_wierszy_model - 1))
            }
          }
          odleglosc[i, j] <- temp / n_kolumn_znorm
        }
      }
    }


    if(is.numeric(KNNmodel$y))
    {
      predykcja <- double(n_kolumn_znorm)

      for(i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]
        
        y_predykcja <- mean(KNNmodel$y[k_najblizej])
        
        predykcja[i] <- y_predykcja
      }
      return(predykcja)
    }
    else if(is.factor(KNNmodel$y))
    {
      predykcja <- as.data.frame(matrix(nrow = n_wierszy_znorm, ncol = nlevels(KNNmodel$y)+1))

      for(i in 1:n_wierszy_znorm)
      {
        k_najblizej <- order(odleglosc[,i])[1:KNNmodel$k]

        if(nlevels(KNNmodel$y) == 2)
        {
          pozytywna <- sum(KNNmodel$y[k_najblizej] == 1) / KNNmodel$k
          negatywna <- sum(KNNmodel$y[k_najblizej] == 0) / KNNmodel$k
          
          predykcja_klasy <- ifelse(pozytywna >= 0.5, 'P', 'N')
          
          names(predykcja) <- c('P', 'N', 'Klasa')
          predykcja[i, 1] <- pozytywna
          predykcja[i, 2] <- negatywna
          predykcja[i, 3] <- predykcja_klasy
        }
        else if(nlevels(KNNmodel$y) > 2)
        {
          etykiety <- sort(unique(KNNmodel$y))
          names(predykcja) <- etykiety
          names(predykcja)[nlevels(KNNmodel$y)+1] <- 'Klasa'
          
          for (j in 1:length(etykiety))
          {
            pozytywna <- sum(KNNmodel$y[k_najblizej] == as.character(etykiety[j])) / KNNmodel$k
            predykcja[i,j] <- pozytywna
          }
          
          predykcja_klasy <- etykiety[which.max(predykcja[i,])]
          predykcja[i,'Klasa'] <- as.factor(predykcja_klasy)
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



# --- test 2
library(caret)

iris_test <- as.data.frame(iris)
iris_test$Species <- as.factor(iris_test$Species)
wiersze <- sample(nrow(iris_test), 125, replace = FALSE)
iris_train <- iris_test[wiersze,]
iris_pred <- iris_test[-wiersze,]

KNN_model_pakiet <- knn3( iris_train[,-5], iris_train[,5], k = 2 )
test_1 <- predict( KNN_model_pakiet, iris_pred[,-5] )
test_1 <- cbind(test_1, '|')

KNN_model <- KNNtrain(iris_train[,-5], iris_train[,5], k = 2, 0, 1)
test_1 <- cbind(test_1, KNNpred(KNN_model, iris_pred[,-5]))
test_1 <- cbind(test_1, TRUE_Y = iris_pred[,5])
test_1
# --- koniec test 2



# --- test 3
#install.packages("mlbench")
library(mlbench)

data("BostonHousing")
# head(BostonHousing)
ncol_BH <- ncol(BostonHousing)    # 14
nrow_BH <- nrow(BostonHousing)    # 506

wiersze_train <- sample(nrow(BostonHousing), 480, replace = FALSE)
BH_train <- BostonHousing[wiersze_train,]
BH_pred <- BostonHousing[-wiersze_train,]

KNN_model_pakiet <- knnreg(BH_train[,-ncol_BH], BH_train[,ncol_BH], k = 2)
test_2 <- predict(KNN_model_pakiet, BH_pred[,-ncol_BH] )
test_2 <- cbind(KNN_PAKIET = test_2)

KNN_model <- KNNtrain(BH_train[,-ncol_BH], BH_train[,ncol_BH], k = 2, 0, 1)
test_2 <- cbind(test_2, KNN_WLASNE = KNNpred(KNN_model, BH_pred[,-ncol_BH]))
test_2 <- cbind(test_2, TRUE_Y = BH_pred[,ncol_BH])
test_2
# --- koniec test 3


