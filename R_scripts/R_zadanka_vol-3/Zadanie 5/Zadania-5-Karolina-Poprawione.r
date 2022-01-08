# Zadanie 1:
# a) Stwórz funkcję "FindBestSplit" przyjmującą nastęujące parametry: "Y", "X", "data", "parentVal", "type", "minobs".
# b) Funkcja powinna zwracać tabelę z wynikami najlepszego możliwego podziału, zawierjącą:
#    - "infGain" - zysk informacyjny dla podziału, 
#    - "lVal" - miarę niejednorodności dla lewego węzła, 
#    - "rVal" - miarę niejednorodności dla prawego węzła,
#    - "point" - punkt (lub zbiór punktów dla zmiennych kategorycznych) podzału,
#    - "Ln" - liczbę obserwacji w lewym węźle, 
#    - "Rn" - liczbę obserwacji w prawym węźle.
# c) Funkcja powinna akceptować zmienne ciagłe, porządkowe oraz nominalne. Dwa ostatnie typy reprezentpwane są jako factor.
# 
# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".
# 
# Zadanie 3:
# a) Stwórz funkcję "PredictTree" przyjmującą nastęujące parametry: "tree", "data".
# b) Funkcja w pierwszym kroku powinna sprawdzać czy przewidywanie zmiennej celu dla nowego zbioru danych jest możliwe do wykonania,
#    tj. czy wszystkie zmienne, które budują strukturę drzewa istnieją w nowym zbiorze danych 
#        oraz czy wszystkie kategorie dla zmiennych porządkowych i nominalnych istnieją w nowym zbiorze danych.
# c) Funkcja powinna rekurencyjnie przechodzić po strukturze drzewa i wykonywać testy w każdym węźle dla danego atrybutu i punktu podziału.
#    Przechodząc do finalnego liścia funkcja powinna odczytywać wartość prognozowaną.
# d) Funkcja powinna zwracać:
#    - dla regresji: wektor z wartościami przewidywanymi.
#    - dla klasyfikacji: nazwaną ramkę danych o rozmiarze "n x k+1", np. dla wersji binarnej o etykietach "P", "N",
#      tabela wygląda następująco: data.frame( P = c(0.3,0.6), N = c(0.7,0.4), Klasa = c("N","P") ), 
#      tj. pierwsze dwie kolumny zawierają prawdopodobieństwo przynależności danej obserwacji do danej klasy (nazwy kolumn są ważne),



### Funkcje potrzebne do dzialania drzewa

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  error_liczba <- 0
  
  if( !is.data.frame(data)){
    warning("Dane to nie ramka danych")
    error_liczba <- error_liczba + 1
  }
  
  if(all(Y %in% colnames(data)) == FALSE || all(X %in% colnames(data)) == FALSE ){
    warning("kolumny 'Y' lub 'X' nie wystepuja w tabeli")
    error_liczba <- error_liczba + 1 
  }
  else{
    
    if(any(is.na(data[,X])) || any(is.na(data[,Y]))){
      warning("Y lub X ma braki danych")
      error_liczba <- error_liczba + 1
    }
    
    if( (is.factor(data[,Y]) && type=="SS") || (is.numeric(data[,Y]) && type=="Gini") || (is.numeric(data[,Y]) && type == "Entropy") == TRUE){
      warning("kombinacja parametrow Y i type nie ma sensu")
      error_liczba <- error_liczba + 1
    }
  }
  
  if(depth <= 0 || minobs <= 0){
    warning("depth lub minobs nie jest wieksze od 0")
    error_liczba <- error_liczba + 1
  }
  
  if((type == "Gini" || type == "SS" || type=="Entropy") == FALSE){
    warning("type nie jest:  Gini,  Entropy , SS")
    error_liczba <- error_liczba + 1
  }
  
  if((overfit == "none" || overfit =="prune") == FALSE){
    warning("overfit nie jest 'none' lub  'prune'")
    error_liczba <- error_liczba + 1
  }
  
  if((cf >0 && cf <=0.5) == FALSE){
    warning("cf nie nalezy do przedzialu (0,0.5]")
    error_liczba <- error_liczba + 1
  }
  
  if(error_liczba > 0){
    print(paste0("Liczba problemow: ", error_liczba))
    return(FALSE)
  }
  else{
    return(TRUE)
  }
  
}


Prob <- function( y ){
  res <- unname( table( y ) )
  res <- res / sum( res )
  return( res )
}

Entropy <- function( prob ){
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
}

SS <- function(n,Y){
  res <- (Y-(1/n) * sum (Y))^2
  res <- sum (res)
  return(res)
}

Gini <- function(prob){
  res <- prob^2
  res <- sum(res)
  res <- 1-res
  return (res)
}


AssignInitialMeasures <- function(tree, Y, data, type, depth){
  tree$Depth <- depth
  wynik <- 0
  
  if (type=="Gini"){
    wynik = Gini(Prob(data[,Y]))
    tree$Val <- wynik
  }
  else if (type=='Entropy'){
    wynik = Entropy(Prob(data[,Y]))
    tree$Val <- wynik
  }
  else if (type=='SS'){
    wynik = SS(nrow(data[,Y]), data[,Y])
    tree$Val <- wynik
  }
  return (tree)
}


AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf){
  attr(tree, "Y") <- Y
  attr(tree, "X") <- X
  attr(tree, "data") <- data
  attr(tree, "type") <- type
  attr(tree, "depth") <- depth
  attr(tree, "minobs") <- minobs
  attr(tree, "overfit") <- overfit
  attr(tree, "cf") <- cf
  return(tree)
}


SplitNum <- function( Y, X, parentVal, splits, type, minobs ){
  n <- length(X)
  res <- data.frame(matrix(0, length(splits), 6))

  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")

  for( i in 1:length(splits)){

    partition <- X <= splits[i]
    ln <- sum(partition)
    rn <- n - ln

    if(any(c(ln,rn) < minobs)){
      res[i,] <- 0
    }else{
      if(type=="Entropy"){
        lVal <- Entropy(Prob(Y[partition]))
        rVal <- Entropy(Prob(Y[!partition]))
      }else if(type=="Gini"){
        lVal <- Gini(Prob(Y[partition]))
        rVal <- Gini(Prob(Y[!partition]))
      }else if(type=="SS"){
        lVal <- SS(nrow(Y[partition]),Y[partition])
        rVal <- SS(nrow(Y[!partition]),Y[!partition])
      }

      InfGain <- parentVal - ( ln/n * lVal + rn/n * rVal )

      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- ifelse(is.numeric(splits[i]), splits[i], as.character(splits[i]))
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return( res )
}


SplitVar <- function( Y, X, parentVal, type, minobs ){
  
  s <- unique(X)
  
  if(length(X) == 1){
    splits <- s
  }else{
    splits <- head(sort(s), -1)
  }
  
  res <- SplitNum(Y, X, parentVal, splits, type, minobs )
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[incl, , drop = F]
  best <- which.max( res$InfGain )
  res <- res[best, , drop = F]
  return( res )
}




### Zadanie 1:

FindBestSplit <- function( Y, Xnames, data, parentVal, type, minobs ){
  
  if(is.numeric(data[,Y])){ 
    for(col_var in Xnames){
      if(!is.numeric(data[,col_var]) & !is.ordered(data[,col_var])){ 
        tmp <- tapply(data[,Y], data[,col_var], mean)                                  
        tmp <- sort(tmp)
        data[,col_var] <- factor(data[,col_var], levels = names(tmp), ordered = TRUE)
      }
    }
  }else{ 
    for(col_var in Xnames){
      if(!is.numeric(data[,col_var]) & !is.ordered(data[,col_var])){ 
        positive_class <- levels(data[,Y])[-1]                                          # klasa pozytywna - klasyfikacja binarna
        positive_rows <- data[data[,Y]==positive_class,] 
        tmp <- prop.table(table(positive_rows[,col_var]))
        tmp <- sort(tmp)
        data[,col_var] <- factor(data[,col_var], levels = names(tmp), ordered = TRUE)
      }
    }
  }
  
  res <- sapply( Xnames, function(i){
    SplitVar(Y = data[,Y] , X = data[,i], parentVal = parentVal, type = type, minobs = minobs)
  }, simplify = F)
  
  res <- do.call(rbind, res)
  best <- which.max(res$InfGain)
  res <- res[ best, , drop = F]
  
  return(res)
}




### Zadanie 2:

library( data.tree )


BuildTree <- function( node, Y, Xnames, data, type, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]

  bestSplit <- FindBestSplit( Y, Xnames, data, node$Val, type, minobs )

  ifStop <- nrow( bestSplit ) == 0
  if( node$Depth == depth | ifStop | all( node$Prob %in% c(0,1) ) ){
    node$Leaf <- "*"
    return( node )
  }

  splitIndx <- data[, rownames(bestSplit) ] <= bestSplit$point
  
  node$BestAtr <- rownames(bestSplit)
  node$SplitPoint <- bestSplit$point
  
  childFrame <- split( data, splitIndx )

  namel <- sprintf( "%s <= %s",  rownames(bestSplit), bestSplit$point )

  childL <- node$AddChild( namel )
  childL$Depth <- node$Depth + 1
  childL$Val <- bestSplit$lVal
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], type, depth, minobs )

  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point )

  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], type, depth, minobs )
}


Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if(StopIfNot (Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf) == FALSE){
    stop("Zle dane lub parametry wejsciowe!")
  }
  
  tree <- Node$new("Root")
  tree$Count <- nrow(data)
  
  AssignInfo(tree, Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  
  AssignInitialMeasures(tree, Y=Y, data=data, type=type, depth=0)
  
  BuildTree(tree, Y, X, data, type, depth, minobs)
  
  return(tree)
}


### testy na zbiorze - iris

iris = datasets::iris

Drzewko <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                 data = iris, type = "Entropy", depth = 6, minobs = 4, overfit = 'none', cf = 0.3)
print(Drzewko, "Count", "Class", "Prob", "Leaf", "Depth")





### Zadanie 3:

Predykcja <- function(tree, data)
{
  if(tree$isLeaf){
    return(c(tree$Prob, tree$Class))
  }
  
  if(data[,tree$BestAtr] <= tree$SplitPoint){
    return(Predykcja(tree$children[[1]], data))
  }else{
    return(Predykcja(tree$children[[2]], data))
  }
}


PredictTree<-function(tree, new_data)
{ 

  if(all(colnames(new_data) %in% attributes(tree)$X) == FALSE )
  {
    stop("Kolumny 'Y' lub 'X' nie wystepuja w tabeli")
  }
  
  for(i in colnames(new_data)){
    if(is.factor(new_data[,i]) || is.ordered(new_data[,i])){
      if(all(levels(new_data[,i]) %in% levels(attributes(tree)$data[,i])) == FALSE){
        stop("Nieznane poziomy w nowych danych")
      }
    }
  }
  
  
  if(is.numeric(attributes(tree)$data[,attributes(tree)$Y]))
  {
    return("REGRESJA = w toku...")
  
  }else if(is.factor(attributes(tree)$data[,attributes(tree)$Y]))
  {
    Klasy_Nowe <- c()
    
    for(i in 1:nrow(new_data))
    {
      probability <- Predykcja(tree, new_data[i,])
      Klasy_Nowe <- rbind(Klasy_Nowe, probability)
    }
    
    Klasy_Nowe <- data.frame(Klasy_Nowe)
    col_names <- c(levels(attributes(tree)$data[,attributes(tree)$Y]), "Klasa")
    colnames(Klasy_Nowe) <- col_names
    
    return(Klasy_Nowe)
  }
}


# testy na zbiorze losowym

set.seed( 666 )
zbiorD <- data.frame( y = factor( c(rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)
zbiorD

Drzewko_inne <- Tree( Y = "y", X = c("x1","x2"), 
                      data = zbiorD, type = "Gini", depth = 6, minobs = 1, overfit = 'none', cf = 0.3)
print(Drzewko_inne, "Count", "Class", "Prob", "Leaf", "Depth")

punkt_ze_zbioru <- zbiorD[6,]
punkt_ze_zbioru
PredictTree(Drzewko_inne, punkt_ze_zbioru[,-1])

punkt_nowy <- zbiorD[6,]
punkt_nowy$x1 <- punkt_nowy$x1 + 0.25
punkt_nowy$x2 <- punkt_nowy$x2 - 0.25
punkt_nowy
PredictTree(Drzewko_inne, punkt_nowy[,-1])


# predykcja na zbiorze 'iris'

numer1 = 77
iris[numer1,]

PredictTree(Drzewko, iris[numer1,-5])

numer2 = c(70,115,15)
iris[numer2,]

PredictTree(Drzewko, iris[numer2,-5])

print("Porownanie predykcji do klas oryginalnych")
cbind(iris[numer2,-5], PredictTree(Drzewko, iris[numer2,-5]))


print("CALY ZBIOR IRIS = Porownanie predykcji do klas oryginalnych")
(cbind(iris, PredictTree(Drzewko, iris[,-5])))

print("Blednie dopasowane klasy:")
sum(!(iris[,5] == PredictTree(Drzewko, iris[,-5])$Klasa))

