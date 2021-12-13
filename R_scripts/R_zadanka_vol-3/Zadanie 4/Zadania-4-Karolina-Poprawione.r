# Zadanie 1

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


dane1 <- data.frame(Y=as.factor(c(1,0,1,0,1,0,1,0,1,0)), X1=c(11,1,13,1.5,0.5,1.8,11.2,0.2,12,1.5), X2=c(3,25,2,22,1,24,2,21,4,20))

StopIfNot( Y= "Y", X=c("X","X2"), data=dane1, type ="Entropy", depth = 4, minobs = 1, overfit = "none", cf = 0.5)
StopIfNot( Y= "Y", X=c("X1","X2"), data=dane1, type ="SS", depth = 4, minobs = 1, overfit = "none", cf = 0.5)
StopIfNot( Y= "Y", X=c("X1","X2"), data=dane1, type ="Entropy", depth = 0, minobs = 1, overfit = "non", cf = 0.7)
StopIfNot( Y= "Y", X=c("X1","X2"), data=dane1, type ="Entropy", depth = 4, minobs = 1, overfit = "none", cf = 0.5)



# Zadanie 2

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


tree_test <- Node$new( "Root" )
AssignInitialMeasures(tree=tree_test, Y="Y", data=dane1, type="Entropy", depth=0)
print(tree_test$Val)
print(tree_test$Depth)



# Zadanie 3

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


AssignInfo(tree=tree_test, Y="Y", X=c("X1","X2"), data=dane1, type="Gini", depth=3, minobs=1, overfit=2, cf=0.3)
print(attributes(tree_test))



# Zadanie 4

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

FindBestSplit <- function( Y, Xnames, data, parentVal, type, minobs ){
  res <- sapply( Xnames, function(i){
    SplitVar(Y = data[,Y] , X = data[,i], parentVal = parentVal, type = type, minobs = minobs)
  }, simplify = F)
  res <- do.call(rbind, res)
  best <- which.max(res$InfGain)
  res <- res[ best, , drop = F]
  return(res)
}



# Zadanie 5

library( data.tree )

PruneTree<-function(){}


Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if(StopIfNot (Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf) == FALSE){
    stop("Zle dane lub parametry wejsciowe!")
  }
  
  tree <- Node$new("Root")
  tree$Count <- nrow(data)
  
  AssignInitialMeasures(tree, Y=Y, data=data, type=type, depth=0)
  
  BuildTree(tree, Y, X, data, type, depth, minobs)
  
  PruneTree()
  
  AssignInfo(tree, Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  
  return(tree)
}



# Zadanie 6

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



### Test wszytkich funkcji jako budowanie drzewa
library(rpart) #CART

Drzewko <- Tree(Y = "Y", X = c("X1"), data = dane1, type = "Entropy", depth = 3, minobs = 1, overfit ='none', cf=0.3)
print(Drzewko, "Count", "Class", "Prob", "Leaf")

Drzewko_rpart <- rpart(formula = Y~X1, data = dane1, minsplit = 1, maxdepth = 3, cp = 0)
Drzewko_rpart



set.seed(666)
zbiorD <- data.frame( y = factor( c(rep(1,5), rep(2,5) ) ), x1 = rnorm(10))
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10)
zbiorD$x2[c(3,8)] <- c(14,2)

Drzewko <- Tree(Y = "y", X = c("x1","x2"), data = zbiorD, type = "Entropy", depth = 3, minobs = 1, overfit ='none', cf=0.3)
print(Drzewko, "Count", "Class", "Prob", "Leaf")

Drzewko_rpart <- rpart(formula = y~x1+x2, data = zbiorD, minsplit = 1, maxdepth = 3, cp = 0)
Drzewko_rpart


