# Plik proszę nazwać numerem swojego indeksu.
# 
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

install.packages( data.tree )
library( data.tree )


StopIfNot <- function(Y,X,data,type,depth,minobs,overfit,cf) {
  if (!is.data.frame(data)) {
    print("Dane nie sa ramka danych!")
    return (FALSE)
  }
  else if (any(!X %in% colnames(data) | !Y %in% colnames(data))) {
    print("Braki danych, sprawdz dane wejsciowe!")
    return (FALSE)
  }
  else if (anyNA(data[,Y]) | anyNA(data[,X])) {
    print(" Wartosci zmiennych Y lub X nie istnieja w zbiorze danych!")
    return (FALSE)
  }
  else if (depth<0 | minobs<0) {
    print("Niedopuszczalne wartosci depth lub minobs, mniejsze od zera!")
    return (FALSE)
  }
  else if (type != "Gini" & type != "Entropy" & type != "SS") {
    print("Typ niemozliwy do wykonania")
    return (FALSE)
  }
  else if (overfit != "prune" & overfit != "none") {
    print("Overfit przyjmuje niedopuszczalne wartosci")
    return (FALSE)
  }
  else if (cf<=0 | cf>0.5) {
    print("Niedopuszczalny przedzial cf ")
    return (FALSE)
  }
  else if (is.factor(data[,Y]) & type == "SS") {
    print("Niedopuszczalna kombinacja parametrow! Type SS, gdy Y jest faktorem.")
    return (FALSE)
  }
  else if (!is.factor(data[,Y]) & (type == "Entropy" | type == "Gini")) {
    print("Niedopuszczalna kombinacja parametrow! Type Gini lub Entropy, gdy dane nie sa faktorem")
    return (FALSE)
  }
  else {
#RETURN("Operacja mozliwa do wykonania!")
#print("Operacja mozliwa do wykonania!"
    return (TRUE)
  }
} #StopIfNot

Prob <- function(y_tar) {
  result <- unname(table(y_tar))
  result <- result/sum(result)
  return(result)
}

Entropy <- function(prob){
  if (any(prob > 1 | prob < 0) == TRUE) {
    stop("Miara prawdopodobienstwa MUSI zawierac sie w przedziale [0,1]! Nie zawiera sie, wprowadz jeszcze raz")
  }
  else {
    result <- -prob * log2(prob)
    result[prob == 0] <- 0
    result <- sum(result)
    return(result)
  }
}

Gini <- function(prob) {
  if (any(prob > 1 | prob < 0) == TRUE) {
    stop("Miara prawdopodobienstwa MUSI zawierac sie w przedziale [0,1]! Nie zawiera sie, wprowadz jeszcze raz")
  }
  else {
    result <- 1 - sum(prob^2)
    return (result)
  }
}

SS <- function(y) {
  if (is.factor(y)) {
    stop("Niemozliwe do wykonania. Dziala jedynie dla regresji. Wprowadzono factor, SS jest tylgo do regresji")
  }
  else {
    y_1 <- sum(y)/length(y)
    y_2 <- (y-y_1)^2
    result <- sum(y_2)
    return (result)
  }
}

AssignInitialMeasures <- function(tree, Y, data, type, depth) {
  tree$Depth <- 0
  if (type == "Entropy") {
    Entropy <- Entropy(Prob(data[,Y]))
    tree$inf <- Entropy
  }
  else if (type == "Gini") {
    Gini <- Gini(Prob(data[,Y]))
    tree$inf <- Gini
  }
  else if (type == "SS") {
    SS <- SS(data[,Y])
    tree$inf <- SS
  }
  else {
    stop("wrong type!")
  }
  return (tree)
}#AssignInitialMeasures

AssignInfo <- function(tree,Y,X,data,type,depth, minobs, overfit, cf ) {
  attr(tree, "Y") <- data[,Y]
  attr(tree, "X") <- data[,X]
  attr(tree, "data") <- data
  attr(tree, "type") <- type
  attr(tree, "depth") <- depth
  attr(tree, "minobs") <- minobs
  attr(tree, "overfit") <- overfit
  attr(tree, "cf")<- cf
  return (tree)
}


SplitNum <- function(Y, x, parentVal, splits, minobs, type) {
  
  n <- length( x )
  res <- data.frame( matrix( 0, length(splits), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  
  for( i in 1:length(splits) ) {
    
    if (is.numeric(x)) {
      partition <- x <= splits[i] # %in%: x %in% c("wyzsze","podstawowe")
    }
    
    else {
      partition <- x %in% splits[1:i]
    }
    
    ln <- sum(partition)
    rn <- n - ln
    
    if( any((c(ln,rn) < minobs | any(is.na(c(ln,rn)))))) {
      res[i,] <- 0
    }
    
    else {
      lVal <- if (type=="Gini") { Gini(Prob(Y[partition])) } 
      else if (type=="Entropy") { Entropy(Prob(Y[partition])) }
      else { SS(Y[partition]) }   
      
      rVal <- if (type=="Gini") { Gini(Prob(Y[!partition])) }
      else if (type=="Entropy") { Entropy(Prob(Y[!partition])) }
      else { SS(Y[!partition]) } 
      
      InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
      
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      
      if (is.numeric(x)) {
        res[i,"point"] <- splits[i]
      }
      else {
        res[i,"point"] <- as.character(paste(splits[i]))
      }
      
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    } #else
  } #for
  return(res)
} #SplitNum


SplitVar <- function(Y, x, parentVal, minobs, type) {
  s <- unique(x)
  
  if(length(x) == 1) {
    splits <- s
  }
  else {
    splits <- head(sort( s ), -1)
  }
  
  res <- SplitNum(Y, x, parentVal, splits, minobs, type)
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[ incl, , drop = F ]
  
  best <- which.max(res$InfGain)
  
  res <- res[best, , drop = F]
  
  return(res)
}# SpliVar

FindBestSplit <- function(Y, X, data, parentVal, type, minobs) {
  res <- sapply(X, function(i) {
    
    # if (is.numeric(data[,i]) | is.ordered(data[,i])) {
    #   d <- as.numeric(paste(data[,i]))
    # }
    if (is.numeric(data[,i])) {
      d <- data[,i]
    }
    else if (is.ordered(data[,i])) {
      d <- as.numeric(paste(data[,i]))
    }
    else {
      if (is.factor(data[,Y])) {
        temp <- data[data[,Y] == 1,]
        a <- prop.table(table(temp[,i]))
        a <- sort(a)
        d <- (paste(factor(data[,i], levels = names(a), ordered = TRUE)))
      }
      else {
        a <- tapply(data[,Y], data[,i], mean)
        a <- sort(a)
        d <- (paste(factor(data[,i], levels = names(a), ordered = TRUE)))
      }
    }
    SplitVar(data[,Y], d, parentVal, minobs, type)
  }, simplify = F ) #function res
  
  res <- do.call("rbind", res)
  
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return (res)
} #FindBestSplit

PruneTree <- function() {
  
}


# Zadanie 2:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".


BuildTree <- function(node, Y, X, data, depth, type , minobs) {
  node$Count <- nrow( data )

  if (is.factor(data[,Y])) {
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else {
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs) #POLACZENIE FindBestSplit
  
  ifStop <- nrow(bestSplit) == 0
  
  if ((is.factor(data[,Y] ) & (node$Depth == depth | ifStop | all(node$Prob %in% c(0,1)))) | (!is.factor(data[,Y]) & (node$Depth == depth | ifStop))) {
    node$Leaf <- "*"
    return( node )
  }
  else {
    
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split(data, split_indx)
    
    name_l <- sprintf("%s <= %s", rownames(bestSplit), bestSplit$point ) 
    child_l <- node$AddChild(name_l)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    child_l$feature <- rownames(bestSplit)
    child_l$BestSplit <- bestSplit$point
    
    BuildTree(child_l, Y, X, child_frame[["TRUE"]], depth, type, minobs)
    
    name_r <- sprintf( "%s >  %s", rownames(bestSplit), bestSplit$point )
    child_r <- node$AddChild( name_r )
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    child_r$feature <- rownames(bestSplit)
    child_r$BestSplit <- bestSplit$point
    
    BuildTree(child_r, Y, X, child_frame[["FALSE"]], depth, type, minobs) 
  }
} #BuildTree


Tree <- function(Y, X, data, type, depth, minobs, overfit = "none", cf = "0.2") {
  
  if (StopIfNot(Y, X, data, type, depth, minobs, overfit,cf) == FALSE) {
    return(FALSE)
  }
  
  #STRING W FACTOR
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
  
  BuildTree(tree, Y, X, data, depth, type, minobs)  #BuildTree
  PruneTree <- function() {}
  
  AssignInfo(tree,Y,X,data,type,depth, minobs, overfit, cf ) #AssignTree
  
  return(tree)
} #Tree



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

obsPred <- function(tree, obs) {
  
  if (tree$isLeaf) {
    print(tree$Prob)
    return(data.frame("Prob" = max(tree$Prob), "Class" = (tree$Class), stringsAsFactors = F))
    }
  if (is.numeric(tree$children[[1]]$BestSplit) | is.ordered(tree$children[[1]]$BestSplit)) {
    
    child <- tree$children[[ifelse(obs[,tree$children[[1]]$feature] > (tree$children[[1]]$BestSplit), 2, 1)]]
    }
  else {
    split <- tree$children[[1]]$feature
    child <- tree$children[[ifelse((obs[,tree$children[[1]]$feature] %in% split), 1, 2)]]
  }
  
  return (obsPred(child,obs))
} #obsPred


PredictTree <- function(tree, data) {
  
  if (is.factor(attributes(tree)$Y)) {
    out <- data.frame(matrix(0, nrow = nrow(data), ncol = 2))
    
    for (i in 1:nrow(data)) {
      out[i,] <- (obsPred(tree, data[i, ,drop = F]))
    }
    colnames(out) <- c("Prob", "Class")
  }
  
  else {
    out <- c()
    
    for (i in 1:nrow(data)) {
      out[i] <- (obsPred(tree, data[i, ,drop = F])$Class)
    }
  }
  return (out)
} #PredictTree





