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


### OCENA - 0%



library( data.tree )

Entropy <- function( prob ){
  
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
  
}

SS <- function( n, Y ){
  
  res <- ( Y - ( 1 / n ) * sum ( Y ) ) ^ 2
  res <- sum ( res )
  return( res )
  
}

Gini <- function( prob ){
  
  res <- prob^2
  res <- sum( res )
  return ( res )
  
}

Prob <- function( y ){
  
  res <- unname( table( y ) )
  res <- res / sum( res )
  
  return( res )
  
}

StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if ( is.data.frame( data ) == FALSE){
    print( "Zmienna 'data' nie jest ramk? danych" )
    return ( FALSE )
  }
  if ( is.null(data$Y) | is.null(data$X) ){
    print( "Brak zmiennej Y lub X" )
    return ( FALSE )
  }
  if ( any( is.na( X ) == TRUE | is.na( Y ) == TRUE) ){
    print( "Istniej? braki danych" )
    return ( FALSE )
  }
  if ( depth <= 0 && minobs <= 0 ){
    print( "G??boko?? lub minimalna liczba obserwacji mniejsza od 0")
    return ( FALSE )
  }
  if ( type != "Gini" && type != "Entropy" && type != "SS" ){
    print( "Nieprawid?owa warto?? zmiennej 'type'" )
    return ( FALSE )
  }
  if ( overfit != "none" && overfit != "prune"){
    print( "Nieprawid?owa warto?? zmiennej 'overfit'" )
    return ( FALSE )
  }
  if( cf  %in% c(0,0.5) == FALSE){
    print( "Zmienna 'cf' ma nieprawid?ow? warto??")
    return ( FALSE )
  }
  if( type == "SS" && is.factor(Y) ){
    print( "Inny warunek nie zosta? spe?niony" )
    return ( FALSE )
  }
  else {
    return ( TRUE )
  }
}

AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
  
  attr(tree, 'Y') <- Y
  attr(tree, 'X') <- X
  attr(tree, 'data') <- data
  attr(tree, 'type') <- type
  attr(tree, 'depth') <- depth
  attr(tree, 'overfit') <- overfit
  attr(tree, 'cf') <- cf
  
}

AssignInitialMeasures <- function( tree, Y, data, type, depth ){
  
  wynik = 0
  depth <- 0
  tree$Depth <- depth
  if ( type == "Gini" ){
    wynik = Gini( data$X )
  }
  else if ( type == 'Entropy' ){
    wynik = Entropy( data$X )
  }
  else if ( type == 'SS' ){
    wynik = SS( nrow( data$X ), data$X )
  }
  
  return ( wynik )
}

SpliNum <- function( Y, x, parentVal, splits, minobs ){
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("infGain","lVal","rVal","point","Ln","Rn")
  
  for( i in 1:length(splits) ){
    
    partition <- x <= splits[i]
    Ln <- sum( partition )
    Rn <- n - Ln
    
    if( any( c(Ln,Rn) < minobs ) ){
      
      res[i,] <- 0
      
    }else{
      
      lVal <- Entropy( Prob( Y[partition] ) )
      rVal <- Entropy( Prob( Y[!partition] ) )
      infGain <- parentVal - ( lVal * Ln/n  + rVal * Rn/n )
      
      res[i,"infGain"] <- infGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[i]
      res[i,"Ln"] <- Ln
      res[i,"Rn"] <- Rn
      
    }
    
  }
  
  return( res )
  
}

SplitVar <- function( Y, x, parentVal, minobs ){
  
  s <- unique( x )
  if( length(x) == 1 ){
    
    splits <- s
    
  }else{
    
    splits <- head( sort( s ), -1 )
    
  }
  
  res <- SpliNum( Y, x, parentVal, splits, minobs )
  
  incl <- res$Ln >= minobs & res$Rn >= minobs & res$infGain > 0
  res <- res[ incl, , drop = F ]
  
  best <- which.max( res$infGain )
  
  res <- res[ best, , drop = F ]
  
  return( res )
  
}

FindBestSplit <- function(Y, X, data, parentVal, type, depth, minobs){
  
  K <- !colnames( data ) %in% Y
  
  res <- sapply( colnames( data )[K], function(i){
    
    SplitVar( data[,Y], data[,i], parentVal, minobs )
    
  }, simplify = F )
  
  res <- do.call( "rbind", res )
  
  best <- which.max( res$infGain )
  res <- res[ best, , drop = F ]
  
  return( res )
  
}

BuildTree <- function( node, Y, X, data, depth, minobs ){
  
  node$Count <- nrow( data )
  node$Prob <- Prob( data[,Y] )
  
  bestSplit <- FindBestSplit( Y, X, data, node$inf, type, depth, minobs )
  
  ifStop <- nrow( bestSplit ) == 0 
  
  if( node$Depth == depth | ifStop | all(  node$Prob %in% c(0,1) ) ){
    
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
    
    BuildTree( child_l, Y, X, child_frame[[1]], depth, minobs )
    
    name <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree( child_r, Y, X, child_frame[[2]], depth, minobs )
    
  }
  
}

Tree <- function( Y, X, data, type, depth, minobs, overfit, cf ){
  
  if (StopIfNot( Y, X, data, type, depth, minobs, overfit, cf ) == FALSE) {
    
    StopIfNot( Y, X, data, type, depth, minobs, overfit, cf )
    return( "Zatrzymanie funkcji - nieprawid?woe dane" )
    
  }
  
  else{
    
    tree <- Node$new( "Root" )
    tree$Depth <- 0
    tree$Count <- nrow( data )
    tree$inf <- Entropy( Prob( data[,Y] ) )
    
    AssignInitialMeasures( tree, Y, data, type, depth )
    BuildTree( tree, Y, X, data, depth, minobs )
    PruneTree<-function(){}
    AssignInfo( tree, Y, X, data, type, depth, minobs, overfit, cf )
    return( tree )
    
  }
  
}

