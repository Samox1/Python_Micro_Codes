### ------------------------ ###
### --- Drzewo Decyzyjne --- ###
### ------------------------ ###

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


SplitNum <- function(Y, x, parentVal, splits, minobs){
  n <- length(x)
  res <- data.frame(matrix(0,length(splits),6))
  colnames(res) <- c("InfGain","lVal","rVal","point","ln","rn")
  for(i in 1:length(splits)){
    partition <- x <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    print(any(c(ln,rn) < minobs))
    #if(any(c(ln,rn) < minobs)){
    #  res[i,] <- 0
    #}else{
      lVal <- Entropy(Prob(Y[partition]))
      rVal <- Entropy(Prob(Y[!partition]))
      InfGain <- parentVal - (ln/n * lVal + rn/n * rVal)
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[i]
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    #}
  }
  return(res)
}


SplitVar <- function(Y, x, parentVal, minobs){
  s <- unique(x)
  if(length(x) == 1){
    splits <- s
  }else{
    splits <- head(sort(s),-1)
  }
  res <- SplitNum(Y, x, parentVal, splits, minobs)
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[ incl, , drop = F ]
  best <- which.max( res$InfGain )
  
  res <- res[ best, , drop = F ]
  return( res )
}


FindBestSplit <- function( Y, Xnames, data, parentVal, minobs ){
  res <- sapply( Xnames, function( i ){
    SplitVar( Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs )
  }, simplify = F )
  res <- do.call( rbind, res )
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  return( res )
}


BuildTree <- function( node, Y, Xnames, data, depth, minobs ){
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  node$Class <- levels( data[,Y] )[ which.max(node$Prob) ]
  bestSplit <- FindBestSplit( Y, Xnames, data, node$Val, minobs )
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
  BuildTree( childR, Y, Xnames, childFrame[["FALSE"]], depth, minobs )
}


Tree <- function( Y, Xnames, data, depth, minobs ){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  return( tree )
}

getwd()
Transfusion_Bin <- read.csv(file="Transfusion.csv")
Drzewko <- Tree( Y = "Y_out", Xnames = c("Recency", "Frequency","Monetary","Time"), data = Binarna, depth = 3, minobs = 1)



