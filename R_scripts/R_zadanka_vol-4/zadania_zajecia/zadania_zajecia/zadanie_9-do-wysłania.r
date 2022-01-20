# Plik prosz? nazwa? numerem swojego indeksu.
#


StopIfNot <- function( Y, X, data, type, depth, minobs, overfit, cf){
  
  if (!is.data.frame(data)){
    print("Typem parametru data nie jest ramka danych.")
    return(FALSE)}
  
  if(!all(c(X) %in% names(data))){
    print("Nie wszystkie zmienne X istnieja w data.")
    return(FALSE)}
  
  if(!all(c(Y) %in% names(data))){
    print("Zmienna Y nie istnieja w data.")
    return(FALSE)}
  
  if(any(is.na(data[,X]) == TRUE)){
    print("Wystepuja braki danych w zmiennych X.")
    return(FALSE)}
  
  if(any(is.na(data[,Y]) == TRUE)){
    print("Występują braki danych w zmiennej Y.")
    return(FALSE)}
  
  if(depth  == 0 || depth  < 0){
    print("depth nie jest wieksze od 0")
    return(FALSE)}
  
  if(minobs  == 0 || minobs < 0){
    print("minobs nie jest wieksze od 0")
    return(FALSE)}
  
  if(type != 'Gini' && type != 'Entropy' && type != 'SS'){
    print("Niepoprawna wartosc typu!")
    return(FALSE)}
  
  if(overfit != 'none' && overfit != 'prune' ){
    print("Niepoprawna wartosc overfit!")
    return(FALSE)}
  
  if(cf <= 0 || cf > 0.5 ){
    print("Wartosc cf nie jest z przedzialu (0, 0.5]!")
    return(FALSE)}
  
  if((is.factor(data[,Y])== TRUE && type=="SS") || 
     (is.numeric(data[,Y])==TRUE &&( type == "Entropy" || type == "Gini"))){
    print("Wybrana kombinacja parametrow type i data nie jest mozliwa.")
    return(FALSE)}
  
  return(TRUE)
}

library(rpart)
library(data.tree)

#Prawdopodobieństwo 
Prob <- function( y_tar ){
  res <- unname( table( y_tar ) )
  res <- res / sum( res )
  return(res)
}

#Entropy

Entropy <- function( prob ){
  
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
  
}

#Gini

Gini <- function(prob){
  
  res <- prob^2
  res <- 1-sum(res)
  return(res)
  
}

#SS

SS <- function(y){
  
  res <- (y-mean(y))^2
  res <- sum(res)
  return(res)
  
}


AssignInitialMeasures <- function(tree,Y,data,type,depth) {
  tree$Depth <- 0
  if (type == "Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  }
  else if (type == "Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  }
  else if (type == "SS") {
    tree$inf <- SS(data[,Y])
  }
  else {
    stop("Nieprawidlowy typ!")
  }
  return (tree)
}

AssignInfo <- function(tree,Y,X,data,type,depth, minobs, overfit, cf )
{
  tree$Y <- data[,Y]
  tree$X <- data[,X]
  tree$data <- data
  tree$type <- type
  tree$Depth <- depth
  tree$minobs <- minobs
  tree$overfit <- overfit
  tree$cf <- cf
}


# Zadanie 1:
# a) Stw?rz funkcj? "FindBestSplit" przyjmuj?c? nast?uj?ce parametry: "Y", "X", "data", "parentVal", "type", "minobs".
# b) Funkcja powinna zwraca? tabel? z wynikami najlepszego mo?liwego podzia?u, zawierj?c?:
#    - "infGain" - zysk informacyjny dla podzia?u, 
#    - "lVal" - miar? niejednorodno?ci dla lewego w?z?a, 
#    - "rVal" - miar? niejednorodno?ci dla prawego w?z?a,
#    - "point" - punkt (lub zbi?r punkt?w dla zmiennych kategorycznych) podza?u,
#    - "Ln" - liczb? obserwacji w lewym w??le, 
#    - "Rn" - liczb? obserwacji w prawym w??le.
# c) Funkcja powinna akceptowa? zmienne ciag?e, porz?dkowe oraz nominalne. Dwa ostatnie typy reprezentpwane s? jako factor.
# 

SpliNum <- function( Y, x, parentVal, splits, minobs, type){
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ){
    
    if(is.numeric(x)){
      partition <- x <= splits[i] 
    }
    else{
      partition <- x %in% splits[1:i]
    }
    
    ln <- sum( partition )
    rn <- n - ln
    
    if( any((c(ln,rn) < minobs| any(is.na(c(ln,rn)))))){ 
      
      res[i,] <- 0
      
    }else{
      
      lVal <- if (type=="Gini") {
        Gini(Prob(Y[partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[partition]))
      } else {
        SS(Y[partition])
      }   
      
      rVal <- if (type=="Gini") {
        Gini(Prob(Y[!partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[!partition]))
      } else {
        SS(Y[!partition])
      } 
      
      InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      if (is.numeric(x)){
        res[i,"point"] <- splits[i]
      }
      else{
        res[i,"point"] <- as.character(paste(splits[i]))
      }
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
      
    }
    
  }
  
  return( res )
  
}


SplitVar <- function( Y, x, parentVal, minobs, type ){
  
  s <- unique( x )
  if( length(x) == 1 ){
    
    splits <- s
    
  }else{
    
    splits <- head( sort( s ), -1 )
    
  }
  
  res <- SpliNum( Y, x, parentVal, splits, minobs, type)
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[ incl, , drop = F ]
  
  best <- which.max( res$InfGain )
  
  res <- res[ best, , drop = F ]
  
  return( res )
  
}



FindBestSplit <- function( Y, X, data, parentVal, type, minobs ){
  res <- sapply( X, function(i){
    
    if (is.numeric(data[,i])) { 
      d <- data[,i]
    }
    else if(is.ordered(data[,i])){
      d <- as.numeric(paste(data[,i]))
    }
    else{
      if(is.factor(data[,Y])){
        temp <- data[data[,Y] == 1,]
        z <- prop.table(table(temp[,i]))
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
      else {
        z <- tapply(data[,Y], data[,i], mean)
        z <- sort(z)
        d <- (paste(factor(data[,i], levels = names(z), ordered = TRUE)))
      }
    }
    SplitVar( data[,Y], d, parentVal, minobs, type )
  }, simplify = F )
  
  res <- do.call( "rbind", res )
  
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  
  return( res )
  
}


# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".
# 

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



Tree<- function(Y, X, data, type, depth, minobs, overfit ,cf){
  
  if(StopIfNot(Y, X, data, type, depth, minobs, overfit,cf) == FALSE)
  {return(FALSE)}
  
  
  for (i in 1:length(X)) { 
    if (is.factor(data[,i])) {
      data[,i] <- as.character(data[,i])
    }
    else {
      next
    }
  }
  
  tree<- Node$new("Root")
  tree$Count <- nrow(data)
  
  if (type == "Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  }
  else if (type == "Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  }
  else {
    tree$inf <- SS(data[,Y])
  }
  
  tree <- AssignInitialMeasures(tree, Y, data, type, depth)
  BuildTree( tree, Y, X, data, depth, type, minobs)  
  PruneTree <- function(){}
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf )
  
  return( tree )
}

# Zadanie 3:
# a) Stw?rz funkcj? "PredictTree" przyjmuj?c? nast?uj?ce parametry: "tree", "data".
# b) Funkcja w pierwszym kroku powinna sprawdza? czy przewidywanie zmiennej celu dla nowego zbioru danych jest mo?liwe do wykonania,
#    tj. czy wszystkie zmienne, kt?re buduj? struktur? drzewa istniej? w nowym zbiorze danych 
#        oraz czy wszystkie kategorie dla zmiennych porz?dkowych i nominalnych istniej? w nowym zbiorze danych.
# c) Funkcja powinna rekurencyjnie przechodzi? po strukturze drzewa i wykonywa? testy w ka?dym w??le dla danego atrybutu i punktu podzia?u.
#    Przechodz?c do finalnego li?cia funkcja powinna odczytywa? warto?? prognozowan?.
# d) Funkcja powinna zwraca?:
#    - dla regresji: wektor z warto?ciami przewidywanymi.
#    - dla klasyfikacji: nazwan? ramk? danych o rozmiarze "n x k+1", np. dla wersji binarnej o etykietach "P", "N",
#      tabela wygl?da nast?puj?co: data.frame( P = c(0.3,0.6), N = c(0.7,0.4), Klasa = c("N","P") ), 
#      tj. pierwsze dwie kolumny zawieraj? prawdopodobie?stwo przynale?no?ci danej obserwacji do danej klasy (nazwy kolumn s? wa?ne),


library(data.tree)

ObsPred <- function(tree, obs) {
  
  if (tree$isLeaf) {
    print(tree$Prob)
    return(data.frame("Prob" = max(tree$Prob), "Class" = (tree$Class), stringsAsFactors = F))}
  
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]}
  else {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]}
  return (ObsPred(child,obs))
}

PredictTree <- function(tree, data) {
  
  if (is.factor(attributes(tree)$Y)) {
    
    res <- data.frame(matrix(0, nrow = nrow(data), ncol = 2))
    for (i in 1:nrow(data)) {
      res[i,] <- (ObsPred(tree, data[i, ,drop = F]))}
    colnames(res) <- c("Prob", "Class")
  }
  else{
    res <- c()
    for (i in 1:nrow(data)) {
      res[i] <- (ObsPred(tree, data[i, ,drop = F])$Class)}
  }
  return (res)
}


