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
#   



# Zadanie 1 #

MinMax <- function( x, new_min = 0, new_max = 1 ){
  return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
}


KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) 
{
  if (any(is.na(X) == TRUE) || any(is.na(y_tar) == TRUE) || k <= 0 || (is.data.frame(X) == FALSE & is.matrix(X) == FALSE) ){
    stop("Bledne dane lub parametry")
  }
  else{
    X <- data.frame(X)
    X_new <- data.frame(matrix(0,ncol = ncol(X), nrow = nrow(X)))
    colnames(X_new) <- colnames(X)
    
    kolumny <- vector()
    minOrg <- vector()
    maxOrg <- vector()
    minmaxNew <- c(XminNew, XmaxNew)
    
    for(i in 1:ncol(X)) 
    {
      kolumny <- append(kolumny, i)
      
      if (is.numeric(X[,i])) {
        X_new[,i] <- MinMax(X[,i], new_min = XminNew, new_max = XmaxNew)
        #X_new[,i] <- ((X[,i] - min(X[,i])) / (max(X[,i]) - min(X[,i]))) * (XmaxNew - XminNew) + XminNew
        minOrg <- append(minOrg, min(X[,i]))
        maxOrg <- append(maxOrg, max(X[,i]))
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) || is.factor(X[,i])){
        X_new[,i] <- X[,i]
        minOrg <- append(minOrg, NA)
        maxOrg <- append(maxOrg, NA)
      }
      else{
        stop("Bledne kolumny")
      }
    }
    
    names(maxOrg) <- kolumny
    names(minOrg) <- kolumny
    attr(X_new, 'minOrg') <- minOrg
    attr(X_new, 'maxOrg') <- maxOrg
    attr(X_new, 'minmaxNew') <- minmaxNew
    
    model_treningowy <- list()
    model_treningowy[["X"]] <- X_new
    model_treningowy[["y"]] <- y_tar
    model_treningowy[["k"]] <- k
    
    return(model_treningowy)
  }
}


# test
dane1 <- c(1:5,11:15)
dane2 <- dane1 + 10
dane3 <- c(1:5,11:15)

X <- data.frame(X1 = dane1, X2 = as.ordered(as.factor(dane2)), X3 = as.factor(dane3))
y <- as.factor(c(0,0,0,0,0,1,1,1,1,1))
k = 2
XminNew <- 0
XmaxNew <- 1

KNN_model <- KNNtrain(X, y, k, XminNew, XmaxNew)
KNN_model
attributes(KNN_model$X)






# Zadanie 2 #

KNNpred <- function(KNNmodel, X) 
{
  if (is.na(KNNmodel) == TRUE || is.na(X) == TRUE || ncol(KNNmodel$X) != ncol(X) || colnames(KNNmodel$X) != colnames(X)){
    stop("Niekompletne dane!")
  }
  else{
    X <- data.frame(X)
    X_new <- data.frame(matrix(0,nrow(X), ncol(X)))
    
    col_num <- 0
    col_fac_ord <- 0
    col_fac <- 0
    
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        col_num <- col_num + 1
        X_new[,i] <- ((X[,i] - as.numeric(attributes(KNNmodel$X)$minOrg[i])) / (as.numeric(attributes(KNNmodel$X)$maxOrg[i]) - as.numeric(attributes(KNNmodel$X)$minOrg[i]))) * (as.numeric(attributes(KNNmodel$X)$minmaxNew[2]) - as.numeric(attributes(KNNmodel$X)$minmaxNew[1])) + as.numeric(attributes(KNNmodel$X)$minmaxNew[1])
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i])){
        col_fac_ord <- col_fac_ord + 1
        X_new[,i] <- X[,i]
      }
      else if(is.factor(X[,i])){
        col_fac <- col_fac + 1
        X_new[,i] <- X[,i]
      }
    }
    
    n_row_KNNmodel = nrow(KNNmodel$X)
    n_col_KNNmodel = ncol(KNNmodel$X)
    
    n_row_Xnew = nrow(X_new)
    n_col_Xnew = ncol(X_new)
    
    distance <- matrix(0, n_row_KNNmodel, n_row_Xnew)
    
    if(col_num == n_col_Xnew){
      for(i in 1:n_row_KNNmodel){
        for(j in 1:n_row_Xnew){
          distance[i,j] <- sqrt(sum((KNNmodel$X[i,] - X_new[j,])^2))
        }
      }
    }
    else if(col_fac_ord == n_col_Xnew){
      for(i in 1:n_row_KNNmodel){
        for(j in 1:n_row_Xnew){
          for(k in 1:n_col_Xnew){
            distance[i,j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_new[j,]))  / (length(unique(X_new[,k])) - 1)) )
          }
        }
      }
    }
    else if(col_fac == n_col_Xnew){
      for(i in 1:n_row_KNNmodel){
        for(j in 1:n_row_Xnew){
          distance[i,j] <- ((sum(KNNmodel$X[i,] != X_new[j,])) / n_col_Xnew)
        }
      }
    }
    else{
      for(i in 1:n_row_KNNmodel){
        for(j in 1:n_row_Xnew){
          suma_pomocnicza <- 0
          
          for(k in 1:n_col_Xnew){
            if(is.numeric(X_new[,k])){
              suma_pomocnicza <- suma_pomocnicza + (abs(KNNmodel$X[i,k] - X_new[j,k]) / (as.numeric(attributes(KNNmodel$X)$maxOrg[k]) - as.numeric(attributes(KNNmodel$X)$minOrg[k]) )) 
            }
            else if(is.factor(X_new[,k])){
              if(KNNmodel$X[i,k] != X_new[j,k]){
                suma_pomocnicza <- suma_pomocnicza + 1
              }
            }
            else if(is.factor(X_new[,k]) & is.ordered(X_new[,k])){
              z_i <- (i - 1) / (n_row_KNNmodel - 1)
              z_n <- (j - 1) / (n_row_Xnew - 1)
              suma_pomocnicza <- suma_pomocnicza + (abs(z_i - z_n) / (n_row_KNNmodel - 1))
            }
          }
          distance[i,j] <- suma_pomocnicza / n_col_Xnew
        }
      }
    }
    
    
    if(is.numeric(KNNmodel$y)){
      prognoza <- double(n_col_Xnew)
      
      for(i in 1:n_row_Xnew){
        knn_best <- order(distance[,i])
        knn_best <- knn_best[1:KNNmodel$k]
        prognoza[i] <- mean(KNNmodel$y[knn_best])
      }
      return(prognoza)
    }
    else if(is.factor(KNNmodel$y)){
      prognoza <- as.data.frame(matrix(nrow = n_row_Xnew, ncol = nlevels(KNNmodel$y)+1))
      
      for(i in 1:n_row_Xnew){
        knn_best <- order(distance[,i])
        knn_best <- knn_best[1:KNNmodel$k]
        
        if(nlevels(KNNmodel$y) == 2){
          names(prognoza) <- c('P', 'N', 'Klasa')
          
          positive <- sum(KNNmodel$y[knn_best] == 1) / KNNmodel$k
          negative <- sum(KNNmodel$y[knn_best] == 0) / KNNmodel$k
          class_predict <- ifelse(positive >= 0.5, 'P', 'N')
          
          prognoza[i,1] <- positive
          prognoza[i,2] <- negative
          prognoza[i,3] <- class_predict
        }
        else if(nlevels(KNNmodel$y) > 2){
          unikalne_klasy <- sort(unique(KNNmodel$y))
          
          names(prognoza) <- unikalne_klasy
          names(prognoza)[nlevels(KNNmodel$y)+1] <- 'Klasa'
          
          for(j in 1:length(unikalne_klasy)){
            positive <- sum(KNNmodel$y[knn_best] == as.character(unikalne_klasy[j])) / KNNmodel$k
            prognoza[i,j] <- positive
          }
          class_predict <- unikalne_klasy[which.max(prognoza[i,])]
          prognoza[i,'Klasa'] <- as.factor(class_predict)
        }
      }
      return(prognoza)
    }
    else{
      stop("Blad z danymi docelowymi")
    }
    
  }
}



# test
library(caret)
set.seed(555)

glass <- as.data.frame(read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data", header = FALSE)[,-1])
glass[,10] <- as.factor(glass[,10])
losowe <- sample(nrow(glass), 200, replace = FALSE)
glass_train <- glass[losowe,]
glass_pred <- glass[-losowe,]

KNN_model <- KNNtrain(glass_train[,-10], glass_train[,10], k = 2, 0, 1)
test <- KNNpred(KNN_model, glass_pred[,-10])
test <- cbind(test, Y_true = glass_pred[,10])
test <- cbind(test, '|')
KNN_model_pakiet <- knn3( glass_train[,-10], glass_train[,10], k = 2 )
test <- cbind(test, predict(KNN_model_pakiet, glass_pred[,-10]))
test


