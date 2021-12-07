# Plik proszę nazwać numerem swojego indeksu.
# 
# Zadanie 1:
# a) Stwórz funkcję "StopIfNot" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna sprawdzać czy nauka modelu jest możliwa do wykonania, tj:
#    - czy "data" jest ramką danych,
#    - czy wszystkie wymienione zmienne ("Y", "X") istnieją w "data",
#    - czy zmienna "Y" oraz zmienne "X" w tabeli "data" nie ma braków danych,
#    - czy "depth" oraz "minobs" są większe od 0,
#    - czy "type" przyjmuje watrtość "Gini", "Entropy", "SS",
#    - czy "overfit" przyjmuje watrtość "none" lub "prune",
#    - czy "cf" jest w przedziale (0,0.5],
#    - czy możliwe kombinacje parametrów mają sens, np. "type = SS" kiedy "Y" jest faktorem.
# c) W przypadku niespełniania któregoś z warunków, funkcja powinna wyświetlić w konsoli, czego dotyczy problem.
# d) Funkcja zwraca "TRUE", jeżeli nauka jest możliwa, w przeciwnym wypadku "FALSE". 


# Zadanie 1:

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){

  war1 <- is.data.frame(data)
  war2 <- all(is.element(Y, colnames(data)))
  war3 <- all(is.element(X, colnames(data)))
  
  if(war2 && war3)
  { 
    war4 <- !any(is.na(data[,Y]))
    war5 <- !any(is.na(data[,X]))
    war11 <- (is.numeric(data[,Y]) && type=="SS") || (is.factor(data[,Y]) && type=="Gini") || (is.factor(data[,Y]) && type == "Entropy")
  }
  else{
    warning("Nie ma wskazanych kolumn")
    return(FALSE)
  }
  
  war6 <- depth > 0
  war7 <- minobs > 0
  war8 <- (type == "Gini" || type == "SS" || type == "Entropy")
  war9 <- (overfit == "none" || overfit =="prune")
  war10 <- cf > 0 && cf <= 0.5
  
  
  if(war1 == FALSE){
    warning("ramka danych nie jest typu 'data.frame'")
  }
  if(war4 == FALSE){
    warning("kolumny 'Y' -> braki danych")
  }
  if(war5 == FALSE){
    warning("kolumny 'X' -> braki danych")
  }
  if(war6 == FALSE){
    warning("parametr 'depth' powinien byc wiekszy od 0")
  }
  if(war7 == FALSE){
    warning("parametr 'minobs' powinien byc wiekszy od 0")
  }
  if(war8 == FALSE){
    warning("parametr 'type' moze przyjmowac wartosci: 'Gini', 'Entropy', 'SS'")
  }
  if(war9 == FALSE){
    warning("parametr 'overfit' powinien byc: 'none' lub 'prune'")
  }
  if(war10 == FALSE){
    warning("parametr 'cf' zawiera sie w przedziale: (0,0.5]")
  }
  if(war11 == FALSE){
    warning("kombinacja 'Y' i parametru 'type' nie ma sensu")
  }
  
  if(war1 && war2 && war3 && war4 && war5 && war6 && war7 && war8 && war9 && war10 && war11)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
  
}

# --- Zadanie 1 - test --- #
data(mtcars)
mtcars[c(5,20),c("qsec","hp")] <- NA
mtcars

StopIfNot("qsec", c("hp","vs","gear"), data=mtcars, type = "SS", depth = -2, minobs = 1, overfit = "nne", cf = 2)
# --- Zadanie 1 - test --- #



# Zadanie 2:
# a) Stwórz funkcję "AssignInitialMeasures" przyjmującą nastęujące parametry: "tree", "Y", "data", "type", "depth".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (czyli korzenia) wartości początkowe:
#    - "depth" = 0.
#    - w zależności od "type" wartość miary Gini, Entropy, SS dla calej populacji (bo to korzeń).
 
library(data.tree)

# Zadanie 2:

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

Gini <- function(prob){
  res <- prob*prob
  res <- sum(res)
  res <- 1-res
  return(res)
}

SS <- function(y){
  n<-length(y)
  y_hat <- sum(y)/n
  res <- y-y_hat
  res <- res*res
  res <- sum(res)
  return(res)
}


AssignInitialMeasures <- function(tree, Y, data, type, depth){
  tree$Depth <- 0
  
  if(type == "Gini")
  {
    pr <- Prob(data[,Y])
    value <- Gini(pr)
    tree$Val <- value
  }
  else if(type == "SS")
  {  
    value <- SS(data[,Y])
    tree$Val <- value
  }
  else if(type == "Entropy")
  {
    pr <- Prob(data[,Y])
    value <-Entropy(pr)
    tree$Val <- value
  }
  return(tree)
}

# --- Zadanie 2 - test --- #
test1 <- data.frame(Y=as.factor(c(0,0,0,1,1,1,1)), X=c(10:16))
tree_test <- Node$new("Root")                                           # Struktura testowa
tree_test <- AssignInitialMeasures(tree_test, "Y", test1, "Gini", 3)
print(tree_test$Depth)
print(tree_test$Val)
# --- Zadanie 2 - test --- #


# Zadanie 3:
# a) Stwórz funkcję "AssignInfo" przyjmującą nastęujące parametry: "tree", "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (jako attrybuty obiektu) wartości owych parametrów.


# Zadanie 3: 

AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf){
  attr(tree, "X")  <- X
  attr(tree, "Y")  <- Y
  attr(tree, "data")  <- data
  attr(tree, "type")  <- type
  attr(tree, "depth")  <- depth
  attr(tree, "minobs")  <- minobs
  attr(tree, "overfit")  <- overfit
  attr(tree, "cf")  <- cf
  return(tree)
}

# --- Zadanie 3 - test --- #
tree_test <- AssignInfo(tree_test, "Y", "X", test1, "Gini", 3, 3, "none", 0.25)
print(attributes(tree_test))
# --- Zadanie 3 - test --- #



# Zadanie 4:
# a) Stwórz funkcję "FindBestSplit" przyjmującą nastęujące parametry: "Y", "X", "data", "parentVal", "type", "minobs".
# b) Funkcja powinna zwracać tabelę z wynikami najlepszego możliwego podziału, zawierjącą:
#    - "infGain" - zysk informacyjny dla podziału, 
#    - "lVal" - miarę niejednorodności dla lewego węzła, 
#    - "rVal" - miarę niejednorodności dla prawego węzła,
#    - "point" - punkt (lub zbiór punktów dla zmiennych kategorycznych) podzału,
#    - "Ln" - liczbę obserwacji w lewym węźle, 
#    - "Rn" - liczbę obserwacji w prawym węźle.   


# Zadanie 4:

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
        prob1 <- Prob(Y[partition])
        prob2 <- Prob(Y[!partition])
        lVal <- Entropy(prob1) 
        rVal <- Entropy(prob2)
      }else if(type=="Gini"){
        prob1 <- Prob(Y[partition])
        prob2 <- Prob(Y[!partition])
        lVal <- Gini(prob1) 
        rVal <- Gini(prob2)
      }else if(type=="SS"){
        lVal <- SS(Y[partition])
        rVal <- SS(Y[!partition])
      }
      InfGain <- parentVal - ( ln/n * lVal + rn/n * rVal )
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[ i ]
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

FindBestSplit2 <- function( Y, Xnames, data, parentVal, type, minobs ){
  res <- sapply( Xnames, function(i){
    SplitVar(Y = data[,Y] , X = data[,i], parentVal = parentVal, type = type, minobs = minobs)
  }, simplify = F)
  res <- do.call(rbind, res)
  best <- which.max(res$InfGain)
  res <- res[ best, , drop = F]
  return(res)
}


# Zadanie 5:
# a) Stwórz funkcję "Tree" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Jest to rozwinięcie funkcji ze slajdu nr 19. Funckja powinna po kolei wywoływać pozostałe funkcje:
#    - "StopIfNot", jeżeli zwracana wartość to "FALSE" to kończymy działanie całej funkcji (zwracamy obiekt niewidzialny),
#    - tworzenie obiektu "tree",
#    - "AssignInitialMeasures",
#    - "BuildTree",
#    - "PruneTree", na tę chwilę ta funkcja jest pusta PruneTree<-function(){},
#    - "AssignInfo".
# c) Funkcja powwina zwracać obiekt "tree".


# Zadanie 5:

PruneTree<-function(tree){
  return(tree)
}


Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  # Sprawdzenie czy wszystkie dane i parametry sa OK
  war1 <- StopIfNot(Y, X, data, type, depth, minobs, overfit, cf)
  if(war1==FALSE){
    warning("STOP = NIE MOZNA UTWORZYC DRZEWA")
    return(NULL)
  }
  
  # Tworzenie obiektu 'tree'
  tree <- Node$new("Root")
  tree$Count <- nrow(data)
  
  # Inicjowanie 'depth' i rozkladu w zaleznosci od 'type'
  AssignInitialMeasures(tree, Y, data, type, depth)

  # Budowanie drzewa - FindBestSplit
  BuildTree(tree, Y, X, data, type, depth, minobs)
  
  # Prune drzewa - pusta funkcja
  PruneTree(tree)
  
  # Dodanie parametrow jako atrybutow drzewa
  AssignInfo(tree, Y, X, data, type, depth, minobs, overfit, cf)
  
  return(tree)
}



# Zadanie 6:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".

library(data.tree)

# Zadanie 6:

BuildTree <- function( node, Y, Xnames, data, type, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  
  bestSplit <- FindBestSplit2( Y, Xnames, data, node$Val, type, minobs)
  
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
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], type, depth, minobs)
  
  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point)
  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], type,  depth, minobs)
}



# --- Zadanie 6 - test 1 (calosci) --- #
test1 <- data.frame(Y=as.factor(c(0,0,0,1,1,1,1)), X=c(10:16))

Drzewko <- Tree( Y = "Y", X = "X", data = test1, type = "Entropy", depth = 3, minobs = 1, overfit = "none", cf = 0.25)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )
plot(Drzewko)
# --- Zadanie 6 - test 1 (calosci) --- #


# --- Zadanie 6 - porownanie --- #
library(rpart)
library(rpart.plot)
Drzewko_RPART <- rpart( formula = Y~X, data = test1, minsplit = 1, maxdepth = 3, cp = 0 )
Drzewko_RPART
rpart.plot(Drzewko_RPART)
# --- Zadanie 6 - porownanie --- #



# --- Zadanie 6 - test 2 (calosci) --- #
test2 <- data.frame(Y=as.factor(c(rep(1,5),rep(2,5),rep(1,5),rep(2,10))), X1=c(1:25), X2=rnorm(25))

Drzewko <- Tree( Y = "Y", X = c("X1"), data = test2, type = "Gini", depth = 5, minobs = 1, overfit = "none", cf = 0.25)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )
plot(Drzewko)

Drzewko <- Tree( Y = "Y", X = c("X1","X2"), data = test2, type = "Gini", depth = 5, minobs = 1, overfit = "none", cf = 0.25)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )
plot(Drzewko)
# --- Zadanie 6 - test 2 (calosci) --- #


# --- Zadanie 6 - porownanie 2 --- #
Drzewko_RPART <- rpart( formula = Y~X1+X2, data = test2, minsplit = 1, maxdepth = 5, cp = 0 )
Drzewko_RPART
rpart.plot(Drzewko_RPART)
# --- Zadanie 6 - porownanie 2 --- #


