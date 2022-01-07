# Zadanie 1:
# a) Stwórz funkcjê "FindBestSplit" przyjmuj¹c¹ nastêuj¹ce parametry: "Y", "X", "data", "parentVal", "type", "minobs".
# b) Funkcja powinna zwracaæ tabelê z wynikami najlepszego mo¿liwego podzia³u, zawierj¹c¹:
#    - "infGain" - zysk informacyjny dla podzia³u, 
#    - "lVal" - miarê niejednorodnoœci dla lewego wêz³a, 
#    - "rVal" - miarê niejednorodnoœci dla prawego wêz³a,
#    - "point" - punkt (lub zbiór punktów dla zmiennych kategorycznych) podza³u,
#    - "Ln" - liczbê obserwacji w lewym wêŸle, 
#    - "Rn" - liczbê obserwacji w prawym wêŸle.
# c) Funkcja powinna akceptowaæ zmienne ciag³e, porz¹dkowe oraz nominalne. Dwa ostatnie typy reprezentpwane s¹ jako factor.


# Budowanie zbioru danych testowych
set.seed( 666 )
zbiorD <- data.frame( y = factor( c(rep(1,5), rep(2,5) ) ), x1 = rnorm(10) )
zbiorD$x2 <- ifelse( zbiorD$y == 1, zbiorD$x1 + 1, zbiorD$x1 + 10 )
zbiorD$x2[c(3,8)] <- c(14,2)


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
FindBestSplit( Y = "y", Xnames = c("x1","x2"), data = zbiorD, parentVal = 1,type="Entropy" ,minobs = 2 )




# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".


library( data.tree )

Tree <- function( Y, Xnames, data,type, depth, minobs, overfit, cf){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  AssignInfo(tree, Y=Y, X=X, data=data, type=type, depth=depth, minobs=minobs, overfit=overfit, cf=cf)
  return( tree )
}
# tree
# str( tree )  

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
Drzewko <- Tree(Y = "y", X = c("x1","x2"), data = zbiorD, type = "Entropy", depth = 3, minobs = 1, overfit ='none', cf=0.3)
print(Drzewko, "Count", "Class", "Prob", "Leaf")
print(attributes(Drzewko))
Drzewko <- Tree( Y = "Species", X = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                 data = iris, type = "Entropy", depth = 6, minobs =5, overfit ='none', cf=0.3 )
print( Drzewko, "Count", "Class", "Prob", "Leaf" )
Drzewko$levelName
print(attributes(Drzewko))



  # a) Stwórz funkcjê "PredictTree" przyjmuj¹c¹ nastêuj¹ce parametry: "tree", "data".
# b) Funkcja w pierwszym kroku powinna sprawdzaæ czy przewidywanie zmiennej celu dla nowego zbioru danych jest mo¿liwe do wykonania,
#    tj. czy wszystkie zmienne, które buduj¹ strukturê drzewa istniej¹ w nowym zbiorze danych 
#        oraz czy wszystkie kategorie dla zmiennych porz¹dkowych i nominalnych istniej¹ w nowym zbiorze danych.
# c) Funkcja powinna rekurencyjnie przechodziæ po strukturze drzewa i wykonywaæ testy w ka¿dym wêŸle dla danego atrybutu i punktu podzia³u.
#    Przechodz¹c do finalnego liœcia funkcja powinna odczytywaæ wartoœæ prognozowan¹.
# d) Funkcja powinna zwracaæ:
#    - dla regresji: wektor z wartoœciami przewidywanymi.
#    - dla klasyfikacji: nazwan¹ ramkê danych o rozmiarze "n x k+1", np. dla wersji binarnej o etykietach "P", "N",
#      tabela wygl¹da nastêpuj¹co: data.frame( P = c(0.3,0.6), N = c(0.7,0.4), Klasa = c("N","P") ), 
#      tj. pierwsze dwie kolumny zawieraj¹ prawdopodobieñstwo przynale¿noœci danej obserwacji do danej klasy (nazwy kolumn s¹ wa¿ne),




str(iris)

PredictTree<-function(tree,data)
{ 
  colnames(data)
  print(attributes(Drzewko)$Y)
  
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
   
  if(is.factor(data$y)){
     
  }
  else 
  {
    return(1)
  }
}



PredictTree(Drzewko,iris[51:100,])
Drzewko <- Tree( Y = "y", Xnames = c("x1","x2"), data = zbiorD, depth = 3, minobs = 1)


