# Plik proszę nazwać numerem swojego indeksu.
# 
# Zadanie 1:
# a) Stwórz funkcję "FindBestSplit" przyjmującą nastęujące parametry: "Y", "X", "data", "parentVal", "type", "depth", "minobs".
# b) Funkcja powinna zwracać tabelę z wynikami najlepszego możliwego podziału, zawierjącą:
#    - "infGain" - zysk informacyjny dla podziału, 
#    - "lVal" - miarę niejednorodności dla lewego węzła, 
#    - "rVal" - miarę niejednorodności dla prawego węzła,
#    - "point" - punkt (lub zbiór punktów dla zmiennych kategorycznych) podzału,
#    - "Ln" - liczbę obserwacji w lewym węźle, 
#    - "Rn" - liczbę obserwacji w prawym węźle.
# 
# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".




library(data.tree)

StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if( is.data.frame( data ) == FALSE)
  {
    message( "Zmienna 'data' nie jest ramka danych" )
    return ( FALSE )
  }
  else if( all(Y %in% colnames(data)) == FALSE | all(X %in% colnames(data)) == FALSE)
  {
    message( "Brak zmiennej Y lub X" )
    return ( FALSE )
  }
  else if( any( is.na( data[,Y] ) == TRUE | is.na( data[,X] ) == TRUE) )
  {
    message( "Istnieja braki danych" )
    return ( FALSE )
  }
  else if( type != "Gini" && type != "Entropy" && type != "SS" )
  {
    message( "Nieprawidlowa wartosc zmiennej 'type'" )
    return ( FALSE )
  }
  else if( depth <= 0 | minobs <= 0 )
  {
    message( "Glebokosc lub minimalna liczba obserwacji rowna lub mniejsza od 0")
    return ( FALSE )
  }
  else if( overfit != "none" && overfit != "prune")
  {
    message( "Nieprawidlowa wartosc zmiennej 'overfit'" )
    return ( FALSE )
  }
  else if( cf <= 0 | cf > 0.5 )
  {
    message( "Zmienna 'cf' nie zawiera sie w przedziale 0.0 - 0.5")
    return ( FALSE )
  }
  else if( (type == "SS" && is.factor(data[,Y])) || (type == "Gini" && !is.factor(data[,Y])) || (type == "Entropy" && !is.factor(data[,Y])))
  {
    message( "Kombinacja danych i 'type' jest nieprawidlowa" )
    return ( FALSE )
  }
  else 
  {
    return ( TRUE )
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

SS <- function( n, Y ){
  res <- ( Y - (( 1 / n ) * sum ( Y )) ) ^ 2
  res <- sum ( res )
  return( res )
}

Gini <- function( prob ){
  res <- prob^2
  res <- sum( res )
  return ( res )
}


AssignInitialMeasures <- function( tree, Y, data, type, depth ){
  
  depth <- 0
  tree$Depth <- depth
  
  if ( type == "Gini" ){
    probability <- Prob(data[,Y])
    wynik <- Gini( probability )
    tree$inf <- wynik
  }
  else if ( type == 'Entropy' ){
    probability <- Prob(data[,Y])
    wynik <- Entropy( probability )
    tree$inf <- wynik
  }
  else if ( type == 'SS' ){
    wynik <- SS( nrow( data[,Y] ), data[,Y] )
    tree$inf <- wynik
  }
  
  return (tree)
}


AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
  
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'minobs') <- minobs
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
  
  return(tree)
}




### Zad. 1 ###

SplitNum <- function( Y, X, type, parentVal, splits, minobs){
  n <- length(X)
  res <- data.frame(matrix(0, length(splits), 6))
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")

  for( i in 1:length(splits)){
    partition <- X <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    if(any(c(ln,rn) < minobs))
    {
      res[i,] <- 0
    }
    else
    {
      if(type=="Entropy")
      {
        lVal <- Entropy(Prob(Y[partition]))
        rVal <- Entropy(Prob(Y[!partition]))
      }
      else if(type=="Gini")
      {
        lVal <- Gini(Prob(Y[partition]))
        rVal <- Gini(Prob(Y[!partition]))
      }
      else if(type=="SS")
      {
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
  return(res)
}


SplitVar <- function( Y, X, type, parentVal, minobs){
  s <- unique(X)

  if(length(X) == 1)
  {
    splits <- s
  }
  else
  {
    splits <- head(sort(s), -1)
  }

  res <- SplitNum(Y, X, type, parentVal, splits, minobs)

  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[incl, , drop = F]
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]

  return( res )
}


FindBestSplit <- function( Y, X, data, type, parentVal, minobs){

  res <- sapply( X, function(i){
    SplitVar(Y = data[,Y] , X = data[,i], type = type, parentVal = parentVal, minobs = minobs)
  }, simplify = F)

  res <- do.call("rbind", res)
  best <- which.max(res$InfGain)
  res <- res[ best, , drop = F]

  return(res)
}




### Zad. 2 ###


BuildTree <- function( node, Y, X, data, type, depth, minobs ){
  
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  
  bestSplit <- FindBestSplit(Y, X, data, type, node$inf, minobs)
  
  ifStop <- nrow( bestSplit ) == 0 
  
  if(node$Depth == depth | ifStop | all(node$Prob %in% c(0,1))){
    
    node$Leaf <- "*"
    return( node )
    
  }else{
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name <- sprintf( "%s <= %s", rownames(bestSplit), bestSplit$point )
    child_l <- node$AddChild( name )
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree( child_l, Y, X, child_frame[[1]], type, depth, minobs )
    
    name <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree( child_r, Y, X, child_frame[[2]], type, depth, minobs )
    
  }
}


PruneTree_pass <- function(){}


Tree <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  tree <- Node$new("Root")
  
  if(StopIfNot(Y, X, data, type, depth, minobs, overfit, cf))
  {
    #tree <- Node$new("Root")
    
    AssignInitialMeasures(tree = tree, Y, data, type, depth)
    
    BuildTree(tree, Y, X, data, type, depth, minobs)
    
    PruneTree_pass()
    
    AssignInfo(tree, Y, X, data, type, depth, minobs, overfit, cf)
    
    return(tree)
  }
  else{
    warning("Problem z danymi lub parametrami wejsciowymi")
    return(invisible(tree))
  }
  
}



# testy #

set.seed( 666 )
zbiorD <- data.frame( y = factor( c( rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(12,1)

Drzewo <- Tree(Y = "y", X = c("x1","x2"), data = zbiorD, type = "Entropy", depth = 3, minobs = 1, overfit ='none', cf=0.3)
print(Drzewo, "Count", "Class", "Prob", "Leaf")

library(rpart)
Drzewo_rpart <- rpart(formula = y~x1+x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0)
Drzewo_rpart


