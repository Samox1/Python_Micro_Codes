# Zadanie 1:

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf){
  #    - czy "data" jest ramkÄ… danych,
  war1 <- class(data)=="data.frame"
  #- czy wszystkie wymienione zmienne ("Y", "X") istniejÄ… w "data",
  war2 <- is.element("X", colnames(data))
  war3 <- is.element("Y", colnames(data))
  war4 <- sum(is.na(Y))==0
  war5 <- sum(is.na(X))==0
  war6 <- depth >0
  war7 <- minobs >0
  war8 <- (type == "Gini" || type == "SS" || type=="Entropy")
  war9 <- (overfit == "none" || overfit =="prune")
  war10 <- cf >0 && cf <=0.5
  war11 <- (class(Y)=="numeric" && type=="SS") || (class(Y)=="factor" && type=="Gini") || (class(Y)=="factor" && type == "Entropy")
  
  if(war1 == FALSE){
    cat("data is not a class 'data.frame'")
  }
  if(war2 == FALSE){
    cat("'X' is not in data")
  }
  if(war3 == FALSE){
    cat("'y' is not in data")
  }
  if(war4 == FALSE){
    cat("Y ma braki danych")
  }
  if(war5 == FALSE){
    cat("X ma braki danych")
  }
  if(war6 == FALSE){
    cat("depth should be greater than 0")
  }
  if(war7 == FALSE){
    cat("minobs should be greater than 0")
  }
  if(war8 == FALSE){
    cat("type should be 'Gini', 'Entropy' or 'SS'")
  }
  if(war9 == FALSE){
    cat("overfit should be 'none' or 'prune'")
  }
  if(war10 == FALSE){
    cat("cf should be value from (0,0.5]")
  }
  if(war11 == FALSE){
    cat("combination of Y and type parameters does not make sense")
  }
  
  if(war1 && war2 &&war3 && war4 &&war5&&war6 && war7&&war8&&war9&&war10&&war11){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
  }


data(mtcars)
colnames(mtcars)[1:2] <- c("X", "Y")
StopIfNot(X, Y, data=mtcars, type = "SS", depth = -2,minobs = 1,overfit = "nne", cf = 2)


 
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
  tree$depth <- 0
  pr <- Prob(data$Y)
  if(type =="Gini"){
    
    value <- Gini(pr)
    tree$Gini<-value
  }else if(type=="SS"){
    value <- SS(data$Y)
    tree$SS <- value
  }else if(type=="Entropy"){
    value <-Entropy(pr)
    tree$Entropy <- value
    
  }
  return(tree)
}
  
# Zadanie 3: 

AssignInfo <- function(tree, Y,X, data, type, depth, minobs, overfit, cf){
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


# Zadanie 4:

FindBestSplit <- function(Y, X, data, parentVal, type, minobs){
  s <- unique( data$X )
  if(length(data$X)==1){
    splits <- s
  }else{
    splits <- head(sort(s),-1)
  }
  n <- length(data$X)
  res<- data.frame(matrix(0, length(splits),6))
  colnames( res ) <- c("InfGain","lVal","rVal","point","Ln","Rn")
  for( i in 1:length( splits ) ){
    partition <- data$X <= splits[ i ]
    ln <- sum( partition )
    rn <- n - ln
    if( any( c(ln,rn) < minobs ) ){
      res[i,] <- 0
    }else {
      prob1 <- Prob( Y[partition] )
      prob2 <- Prob( Y[!partition] )
      if(type=="Entropy"){
        lVal <- Entropy(prob1) 
        rVal <- Entropy( prob2)
      }else if(type=="Gini"){
        lVal <- Gini(prob1) 
        rVal <- Gini(prob2)
      }else if(type=="SS"){
        lVal <- Gini(Y[partition]) 
        rVal <- Gini(Y[!partition])
      }
      
      InfGain <- parentVal - ( ln/n * lVal + rn/n * rVal )
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[ i ]
      res[i,"Ln"] <- ln
      res[i,"Rn"] <- rn
    }
  }
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[ incl, , drop = F ]
  best <- which.max( res$InfGain )
  # ten, ktory  daje zbilansowany podzial
  res <- res[ best, , drop = F ]
  
  return(res)
  
}


# Zadanie 5:

#PruneTree<-function(){}

Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  war1 <- StopIfNot(Y, X, data, type, depth, minobs, overfit, cf)
  if(war1==FALSE){
    return(NULL)
  }
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  prob <- Prob(data[,Y]) #opcjonalnie prob <- Prob(data$Y) o ile Y to nazwa kolumny z data
  if(type == "Entropy"){
    tree$Val <- Entropy(prob)
  }else if(type == "Gini"){
    tree$Val <- Gini(prob)
  }else if(type =="SS"){
    tree$Val <- SS(data[,Y])
  }
  
  tree<- AssignInitialMeasures(tree, Y, data, type, depth)
  #buildTree
  tree <-BuildTree(tree, Y, X, data, type, depth, minobs)
  #PruneTree<-function(){}
  
  tree <- AssignInfo(tree, Y,X, data, type, depth, minobs, overfit, cf)
  
  return(tree)
}



# Zadanie 6:


BuildTree <- function( node, Y, Xnames, data,type, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  bestSplit <- FindBestSplit( Y, Xnames, data, type,node$Val, minobs )
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
  BuildTree( childL, Y, Xnames, childFrame[["TRUE"]], depth, minobs )
  namer <- sprintf( "%s >  %s",  rownames(bestSplit), bestSplit$point )
  childR <- node$AddChild( namer )
  childR$Depth <- node$Depth + 1
  childR$Val <- bestSplit$rVal
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]],type,  depth, minobs )
}