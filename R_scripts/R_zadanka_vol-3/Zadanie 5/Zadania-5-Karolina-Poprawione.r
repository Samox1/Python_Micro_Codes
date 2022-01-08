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
      res[i,"point"] <- splits[i]
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
        positive_class <- 1 
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



# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".

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
  
  AssignInitialMeasures(tree, Y=Y, data=data, type=type, depth=0)
  
  BuildTree(tree, Y, X, data, type, depth, minobs)
  
  AssignInfo(tree, Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  
  return(tree)
}


### testy na zbiorze - iris

library(rpart)

rm(iris)
iris = datasets::iris

Drzewko <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                 data = iris, type = "Entropy", depth = 6, minobs = 5, overfit = 'none', cf = 0.3 )
print(Drzewko, "Count", "Class", "Prob", "Leaf" )

rpart( formula = Species~., data = iris, minsplit = 5, maxdepth = 6, cp = 0 )


# modyfikacja 'iris' = jedna z kolumn jest 'factor'
iris_zmodyfikowane = iris[,]
iris_zmodyfikowane[(iris_zmodyfikowane$Petal.Width > 1.6 & iris_zmodyfikowane$Petal.Width <= 3.0),'Petal.Width'] = '3-3'
iris_zmodyfikowane[(iris_zmodyfikowane$Petal.Width > 1.0 & iris_zmodyfikowane$Petal.Width <= 1.6),'Petal.Width'] = '2-2'
iris_zmodyfikowane[iris_zmodyfikowane$Petal.Width <= 1.0,'Petal.Width'] = '1-1'
iris_zmodyfikowane$Petal.Width <- factor(iris_zmodyfikowane$Petal.Width, ordered = FALSE)

Drzewko_zmodyfikowane <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                 data = iris_zmodyfikowane, type = "Entropy", depth = 6, minobs = 5, overfit = 'none', cf = 0.3 )
print(Drzewko, "Count", "Class", "Prob", "Leaf" )

rpart( formula = Species~., data = iris_zmodyfikowane, minsplit = 5, maxdepth = 6, cp = 0 )



# iris_y_reg = iris[,]
# iris_y_reg$Species = as.numeric(as.character(as.numeric(iris_y_reg$Species)))
# Drzewko_y_reg <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
#                                data = iris_y_reg, type = "SS", depth = 6, minobs = 5, overfit = 'none', cf = 0.3 )
# print(Drzewko_y_reg, "Count", "Class", "Prob", "Leaf" )
# 
# Drzewko_RPART <- rpart( formula = Species~., data = iris_y_reg, minsplit = 5, maxdepth = 6, cp = 0 )
# numer = 120
# strzal = iris[numer,-5]
# predict(Drzewko_RPART, strzal, type = "vector")




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


PredictTree<-function(tree, new_data)
{ 

  if(all(attributes(tree)$Y %in% colnames(data)) == FALSE || all(attributes(tree)$X %in% colnames(data)) == FALSE )
  {
    warning("kolumny 'Y' lub 'X' nie wystepuja w tabeli")
  }
  
  if(all(levels(attributes(tree)$data$y) %in% tree$Class)==FALSE)
  {
    warning("Brakuje kategori w danych")
  }
  else
  {
    TRUE
  }
  
  if(is.numeric(data$y))
  {
  
  }
   
  if(is.factor(data$y))
  {
     
  }
  else 
  {
    return(1)
  }
}



PredictTree(Drzewko,iris[51:100,])

# Drzewko <- Tree( Y = "y", Xnames = c("x1","x2"), data = zbiorD, depth = 3, minobs = 1)





Drzewko <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                 data = iris, type = "Entropy", depth = 6, minobs = 5, overfit = 'none', cf = 0.3 )
print(Drzewko, "Count", "Class", "Prob", "Leaf" )

# Proby z predykcja

Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (c(tree$children[[1]]$name, tree$children[[1]]$Class))
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}

numer = 120
strzal = iris[numer,-5]

Predict(Drzewko, strzal)

eval(parse(Drzewko$children[[1]]))


