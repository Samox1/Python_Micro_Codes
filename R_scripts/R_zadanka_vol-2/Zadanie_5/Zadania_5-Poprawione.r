
### Funkcje potrzebne do dzialania drzewa


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
        tmp <- tapply(data[,Y], as.numeric(data[,col_var]), mean)
        tmp <- sort(tmp)
        data[,col_var] <- factor(data[,col_var], levels = names(tmp), ordered = TRUE)
      }
    }
  }else{ 
    for(col_var in Xnames){
      if(!is.numeric(data[,col_var]) & !is.ordered(data[,col_var])){ 
        positive_rows <- data[data[,Y]==levels(data[,Y])[1],] 
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
  
  if (is.factor(data[,Y])){
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else{
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
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


############### Dzialajace BUILD TREE od vol.4 ###########################
BuildTree <- function(node, Y, X, data, depth, type , minobs){
  
  node$Count <- nrow( data )
  # za kazdym razem mamy dostep do wszystkich kolumn ale tylko do tych obserwacji kt potrzebujemy 
  
  if (is.factor(data[,Y])) {
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else {
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs) 
  
  ifStop <- nrow(bestSplit) == 0
  
  if( ifStop| node$Depth == depth | all( node$Prob %in% c(0,1) )){
    
    node$Leaf <- "*"
    return( node )
    
  }else{
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name_l <- sprintf( "%s <= %s", rownames(bestSplit), bestSplit$point ) 
    child_l <- node$AddChild( name_l )
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    child_l$feature <- rownames(bestSplit)
    child_l$BestSplit <- bestSplit$point
    
    BuildTree( child_l, Y, X, child_frame[["TRUE"]], depth, type, minobs)
    
    
    name_r <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name_r )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    child_r$feature <- rownames(bestSplit)
    child_r$BestSplit <- bestSplit$point
    
    BuildTree( child_r, Y, X, child_frame[["FALSE"]], depth, type, minobs ) 
    
  }
  
}
############### Dzialajace BUILD TREE od vol.4 ###########################


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



### testy na zbiorze - wine

wine <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data', header = FALSE)
wine$V1 <- as.factor(wine$V1)

Tree_Wina <- Tree( Y = names(wine)[1], X = names(wine)[-1], 
                 data = wine, type = "Entropy", depth = 6, minobs = 4, overfit = 'none', cf = 0.3)
print(Tree_Wina, "Count", "Class", "Prob", "Leaf", "Depth")
plot(Tree_Wina)




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


Prediction <- function(tree, data)
{
  if(tree$isLeaf){
    return(c(tree$Prob, tree$Class))
  }
  
  if(data[,tree$BestAtr] <= tree$SplitPoint){
    return(Prediction(tree$children[[1]], data))
  }else{
    return(Prediction(tree$children[[2]], data))
  }
}


PredictTree<-function(tree, new_data)
{ 
  
  if(all(colnames(new_data) %in% attributes(tree)$X) == FALSE )
  {
    warning("'Y' lub 'X' brakuje w danych")
    return(1)
  }
  
  for(i in colnames(new_data))
  {
    if(is.factor(new_data[,i]) || is.ordered(new_data[,i]))
    {
      if(all(levels(new_data[,i]) %in% levels(attributes(tree)$data[,i])) == FALSE)
      {
        warning("Poziomy w danych do predykcji nie zgadzaja sie z danymi uczacymi")
        return(1)
      }
    }
  }
  
  
  if(is.factor(attributes(tree)$data[,attributes(tree)$Y]))
  {
    Class_new <- c()
    
    for(i in 1:nrow(new_data))
    {
      probability <- Prediction(tree, new_data[i,])
      Class_new <- rbind(Class_new, probability)
    }
    
    Class_new <- data.frame(Class_new)
    col_names <- c(levels(attributes(tree)$data[,attributes(tree)$Y]), "Klasa")
    colnames(Class_new) <- col_names
    
    return(Class_new)
  }
  else if(is.numeric(attributes(tree)$data[,attributes(tree)$Y]))
  {
    return("... regresja ...")
  }
}



# predykcja na zbiorze - wine

wine[50,]
PredictTree(Tree_Wina, wine[50,-1])

Test_Wina = cbind(wine[,1], PredictTree(Tree_Wina, wine[,-1]))
head(Test_Wina)
tail(Test_Wina)

cat("Bledna klasyfikacja - sztuk blednie:")
bledne_wina = (!(wine[,1] == PredictTree(Tree_Wina, wine[,-1])$Klasa))
sum(bledne_wina)
cbind(wine[!(wine[,1] == PredictTree(Tree_Wina, wine[,-1])$Klasa),], Test_Wina[bledne_wina,])




iris
head(iris)
iris[,5] <- as.numeric(iris[,5])
is.factor(iris[,5])
iris

Tree1 <- Tree("Species", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width") , iris, 'SS', 6, 2, 'none', 0.1)
print(Tree1)

PredictTree(Tree1, iris[78,-5])
print(Tree1, "Count", "Class", "Prob", "Leaf", "Depth")
