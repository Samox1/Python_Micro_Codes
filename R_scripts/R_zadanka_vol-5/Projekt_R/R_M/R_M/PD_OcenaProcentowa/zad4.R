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
# ***) Dla ambitnych: Funkcja może przyjmować dodatkowo parametr "metoda = brute". 
#      Oznacza to, że domyślnie nie następuje faza treningowa, 
#      lecz w przypadku przekazania argumentu "kdtree", funkcja przeorganizuję tabelę "X" w pomocniczą strukturę k-d tree. 
#      Taki zabieg będzie wpływał na sposób wykonania Zadania 2. Ta część może być napisana w C/C++. 
#      Struktura k-d tree jest możliwa do stworzenia dla zmiennych na skali ilorazowej.
#


#NORMALIZACJA DO 0,1
  MinMax <- function( x, new_min = 0, new_max = 1 ){
    return( ( ( x - min(x) ) / ( max(x) - min(x) ) ) * ( new_max - new_min ) + new_min )
  }
  # set.seed(123)
  # y <- runif(100, min = 1, max = 2)
  # summary(y) # wywołuje informacje statystki dla wszystkich wartości 
  # summary( MinMax(y)) # sprawdzamy MinMax pracując na y
  # summary( MinMax(y, 10, 25)) # sprawdzamy MinMax pracując na innej skali od 10 do 25
  
#NORMALIZACJA ZScore
  ZScore <- function(x) {
    return ((x - mean(x))/sd(x))  
  }
  
  
  KNNtrain <- function(X, y_tar, k = 2, XminNew = 0, XmaxNew = 1){
  if(k <=0 ){
     stop("Analiza nie jest możliwa do wykonania. Liczba sasiadow niepoprawnie wprowadzona, mniejsza lub rowna 0")
   }
  else if(!is.matrix(X) & !is.data.frame(X)){
    stop("Analiza nie jest mozliwa do wykonania. Wektor wejsciowy nie jest ani macierza, ani ramka danych.")
   }
  else if(anyNA(X) | anyNA(y_tar)){
    stop("Analiza nie jest mozliwa do wykonania. Brakujace dane. Uzupelnij zbior danych" )
  }
    
  # else if(nrow(X) != length(y_tar)){
  #   stop("Ilosc Wartosci prognozowanych jest rozna od ilosci zmiennych objasniajacych")
  # }
    
  else{
    macierz <- matrix(0, nrow = nrow(X), ncol = ncol(X))
    ramka_danych <- data.frame(macierz)
    
    minOrg <- c()
    maxOrg <- c()
    minmaxNew <- c()
    
    for (i in 1:length(X)) {
      if (all(sapply(X[i], class) == "numeric")) {
        minOrg <- append(minOrg, min(X[i]))
        maxOrg <- append(maxOrg, max(X[i]))
        X[i] <- as.vector(unlist(MinMax(X[i], XminNew,XmaxNew)))
        minmax <- c(c(min(X[i]),max(X[i])))
        names(minmax) <- c( paste( colnames(X[i]), "min", sep="_"), paste(colnames(X[i]), "max", sep="_"))
        minmaxNew <- append(minmaxNew, minmax)
      }
      else if (all(sapply(X[i], class) == "factor")) {
        maxOrg <- append(maxOrg,max(as.vector(as.numeric(unlist(X[i])))))
        minOrg <- append(minOrg,min(as.vector(as.numeric(unlist(X[i])))))
        X[i] <- factor(as.vector(unlist(X[i])))
      }
      else {
        maxOrg <- append(maxOrg,max(as.vector(unlist(X[i]))))
        minOrg <- append(minOrg,min(as.vector(unlist(X[i]))))
        X[i] <- (X[i])
      }
    }
    
    names(maxOrg) <- colnames(X)
    names(minOrg) <- colnames(X)
    attr(X,paste("minOrg")) <- minOrg
    attr(X,paste("maxOrg")) <- maxOrg
    attr(X,paste("minmaxNew")) <- minmaxNew
    knn_lista <- list("X" = X, "y_tar" = y_tar, "k" = k)
    
    return (knn_lista)
  }

}#KNNtrain



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
  install.packages("Parallel")
  library("parallel") # do ncores, clusters
  library("foreach")
  library("iterators")
  library("doParallel")

#wyszukanie mody dla zbioru, dominanta
getMode <- function(x) {
   distinct <- unique(x)
   distinct_max <- distinct[which.max(tabulate(match(x,distinct)))]
   return (distinct_max)
}
  
#wyszukanie unikat
getDistinct <- function(x) {
  return (unique(x))
}
  
  
#wzory wyklad
#skala ilorazowa
d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}

#skala porzadkowa
d_czebyszew <- function(x_i, x_n) {
  return (max(abs(x_i - x_n)))
}

d_interval <- function(moc, x_i, x_n) {
  return (sum(abs(x_i - x_n)/(moc - 1)))
}

#skala nominalna
d_hamming <- function(x_i, x_n) {
  return (sum(x_i != x_n)/length(x_i))
}

#Gower <- MIESZANE
d_gower <- function(dane,x_i,x_n) {
  if (length(x_i) != length(x_n)) {
    stop("Nieprawidlowe dlugosci wektorow ")
  }
  else {
    wynik <- 0
    for (i in 1:length(x_i)) {
      if (all(sapply(x_i[i],class) == "numeric") | any(sapply(x_i[i],class) == "ordered")) {
        odl <- abs(as.numeric(paste(x_i[i])) - as.numeric(paste(x_n[i])))/(max(as.numeric(paste(dane[[names(x_i[i])]]))) - as.numeric(paste(min(dane[[names(x_n[i])]]))))
        wynik <- wynik + odl
      }
      else {
        odl <- sum(x_i[i] != x_n[i])
        out <- out + odl
        #=+ nie dziala
      }
    }
    return (as.numeric(out)/length(x_i))
  }
}



KNNpred <- function(KNNmodel, X, Ncores = 1){
  nazwyKolumn <- colnames(KNNmodel) == colnames(X)
  if(any(is.na(X))){
    stop("Brak danych! Przewidywanie niemozliwe do wykonania,")
  }
#  Dodanie warunku
  else if(all(nazwyKolumn) == FALSE){
    stop("Kolumny niepoprawnie")
  }
  else{#poprawne wartosci
    colTypes <- unique(unlist(lapply(X, class)))
    
    cores <- detectCores()
    if(Ncores > cores){
      stop("Wprawdza liczbe rdzeni jeszcze raz, jest za duza dla tego komputera!")
    }
    
    cl <- makeCluster(Ncores, outfile = "")
    registerDoParallel(cl)
    
    clusterExport(cl,"d_euklides")
    clusterExport(cl,"getMode")
    clusterExport(cl,"getDistinct")
    clusterExport(cl,"d_hamming")
    clusterExport(cl,"d_gower")
    clusterExport(cl,"d_czebyszew")
    clusterExport(cl,"d_interval")
    clusterExport(cl,"KNNmodel",envir = environment())
    clusterExport(cl,"X",envir = environment())
    
    
    # SKALA PORZADKOWA
    if(isTRUE(all.equal(c("ordered", "factor"), colTypes))) {
      #porzadkowa
      KNNmodel$X <- sapply(KNNmodel$X, as.numeric)
      X <- sapply(X, as.numeric)
      y <- KNNmodel$y
      
      
     if (is.factor(y)) { 
      print("KLASYFIKACJA dla skali porzadkowej")
      nTrain <- nrow(KNNmodel$X)        
      nPred <- nrow(X)
      
      clusterExport(cl, "nTrain", envir = environment())
      clusterExport(cl, "nPred", envir = environment())
      
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j) d_czebyszew(x, X[j,])))
      
      pred <- double(nPred)
      out <- c()##################################
      clusterExport(cl,"pred",envir = environment())
      clusterExport(cl,"out",envir = environment())
        
      out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
      out <- as.data.frame(t(out))
      out <- out[1:length(unique(KNNmodel$y))]
      colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) 
        paste(sort(unique(KNNmodel$y))[i],sep = "_"))
      out$max <- apply(out, 1, max)
      pred <- colnames(out)[max.col(out, ties.method = "first")]
      drops <- c("max")
      out <- out[ , !(names(out) %in% drops)]
        
      return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }#is factor     
      
      
    else {
      print("REGRESJA dla skali porzadkowej")
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
        
      clusterExport(cl,"nTrain",envir = environment())
      clusterExport(cl,"nPred",envir = environment())
        
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j) d_czebyszew(x, X[j,])))
        
       out <- (parLapply(cl, 1:nPred, function(i) {
        kNaj <- order(odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }#porzadkowa, nie facto,r KLASYFIKACJA
    }#SKALA PORZADKOWA
    
    #SKALA ILORAZOWA
    else if(isTRUE(all.equal("numeric",colTypes))) {
      
      y <- KNNmodel$y
      clusterExport(cl,"y",envir = environment())
      
      #KLASYFIKACJA SKALI ILORAZOWEJ
      if (is.factor(y)) {
        print("Dzialania dla skali ilorazowej, odlegloscia euklidesa, zagadneinie KLASYFIKACJI")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl, "nTrain", envir = environment())
        clusterExport(cl, "nPred", envir = environment())
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_euklides(x, X[j,])))
        
        pred <- double(nPred)
        out <- c()
        clusterExport(cl,"pred",envir = environment())
        clusterExport(cl,"out",envir = environment())
        
        out <- (parSapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred[i] <- getMode(KNNmodel$y[kNaj])
          tab <- as.data.frame(table(KNNmodel$y[kNaj]))
          p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
          pr <- rbind(p$p)
          out <- rbind(out,as.vector(pr))
        }))
        
        
        out <- as.data.frame(t(out))
        out <- out[1:length(unique(KNNmodel$y))]
        colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
        out$max <- apply(out, 1, max)
        pred <- colnames(out)[max.col(out, ties.method = "first")]
        drops <- c("max")
        out <- out[ , !(names(out) %in% drops)]
        
        return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
      }# KLASYFIKACJA DLA SKALI ILORAZOWEJ
      
      #REGRESJA na skali ilorazowej
      else{
        print("Dla skali ilorazowej, zagadnienie regresji, odleglosc euklidesa")
        nTrain <- nrow(KNNmodel$X)
        nPred <- nrow(X)
        
        clusterExport(cl,"nTrain",envir = environment())
        clusterExport(cl,"nPred",envir = environment())
        
        
        odl <- parApply(cl, KNNmodel$X, 1, function(x) 
          sapply(1:nPred, function(j) d_euklides(x, X[j,])))
        
        out <- (parLapply(cl, 1:nPred, function(i) {
          kNaj <- order(odl[,i])
          kNaj <- kNaj[1:KNNmodel$k]
          pred <- mean(KNNmodel$y[kNaj])
        }))
        
        return (unlist(out))
      }#REGRESJA na skali ilorazowej
    }# SKALA ILORAZOWA
  
  #SKALA NOMINALNA
  else if(isTRUE(all.equal("factor",colTypes))) {
    y <- KNNmodel$y
    
    #KLASYFIKACJA skali nominalnej
    if (is.factor(y)) {
      print("Klasyfikacja skali nominalnej, odleglosc Hamminga")
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      
      clusterExport(cl,"nTrain",envir = environment())
      clusterExport(cl,"nPred",envir = environment())
      
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j) d_hamming(x, X[j,])))
      
      pred <- double(nPred)
      out <- c()
      clusterExport(cl,"pred",envir = environment())
      clusterExport(cl,"out",envir = environment())
      
      out <- (parSapply(cl, 1:nPred, function(i) {
        kNaj <- order(odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        pred[i] <- getMode(KNNmodel$y[kNaj])
        tab <- as.data.frame(table(KNNmodel$y[kNaj]))
        p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
        pr <- rbind(p$p)
        out <- rbind(out,as.vector(pr))
      }))
      
      out <- as.data.frame(t(out))
      out <- out[1:length(unique(KNNmodel$y))]
      colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
      out$max <- apply(out, 1, max)
      pred <- colnames(out)[max.col(out, ties.method = "first")]
      drops <- c("max")
      out <- out[ , !(names(out) %in% drops)]
      
      return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
    }# KLASYFIKACJA SKALI NOMINALNEJ
    
    #REGRESJA skala nominalna
    else {
      print("Regresja dla skali nominalnej, odleglosc Hamminga")
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      
      clusterExport(cl,"nTrain",envir = environment())
      clusterExport(cl,"nPred",envir = environment())
      
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j) d_hamming(x, X[j,])))
      
      out <- (parLapply(cl, 1:nPred, function(i) {
        kNaj <- order(odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        pred <- mean(KNNmodel$y[kNaj])
      }))
      
      return (unlist(out))
    }#REGRESJA skala nominalna
  }#SKALA NOMINALNA
  
  #SKALA MIESZANA
  else{
    y <- KNNmodel$y
    
    #KLASYFIKACJA mieszana
    if (is.factor(y)) {
      print("Zagadnienie KLASYFIKACJI dla skali mieszanej przy uzyciu odleglosci GOWERA")
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
     
      clusterExport(cl,"nTrain",envir = environment())
      clusterExport(cl,"nPred",envir = environment())
      clusterExport(cl,"y",envir = environment())
      
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j)
          d_gower(KNNmodel$X, x, X[j,])))
      
      pred <- double(nPred)
      out <- c()
      clusterExport(cl,"pred",envir = environment())
      clusterExport(cl,"out",envir = environment())
      
      out <- (parSapply(cl, 1:nPred, function(i) {
        kNaj <- order(odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        pred[i] <- getMode(KNNmodel$y[kNaj])
        tab <- as.data.frame(table(KNNmodel$y[kNaj]))
        p <- data.frame(tab, "p" = tab$Freq/sum(tab$Freq))
        pr <- rbind(p$p)
        out <- rbind(out,as.vector(pr))
      }))
      
      out <- as.data.frame(t(out))
      out <- out[1:length(unique(KNNmodel$y))]
      colnames(out) <- lapply(1:length(unique(KNNmodel$y)), function(i) paste(sort(unique(KNNmodel$y))[i],sep = "_"))
      out$max <- apply(out, 1, max)
      pred <- colnames(out)[max.col(out, ties.method = "first")]
      drops <- c("max")
      out <- out[ , !(names(out) %in% drops)]
      
      return (data.frame("y_tar" = KNNmodel$y,out,y_hat = pred))
    }# KLASYFIKACJA skala mieszana
    
    #REGRESJA MIESZANA
    else {
      print("Zagadnienie REGRESJI dla skali mieszanej przy uzyciu odleglosci GOWERA")
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      
      clusterExport(cl,"nTrain",envir = environment())
      clusterExport(cl,"nPred",envir = environment())
      
      odl <- parApply(cl, KNNmodel$X, 1, function(x) 
        sapply(1:nPred, function(j) d_gower(KNNmodel$X, x, X[j,])))
      
      out <- (parLapply(cl, 1:nPred, function(i) {
        kNaj <- order(odl[,i])
        kNaj <- kNaj[1:KNNmodel$k]
        pred <- mean(KNNmodel$y[kNaj])
      }))
      
      return (unlist(out))
    }# Regresja dla mieszanej
  }# SKALA MIESZANA
  }# else zadania
  
  stopCluster(cl)
  
}#KNNpred


