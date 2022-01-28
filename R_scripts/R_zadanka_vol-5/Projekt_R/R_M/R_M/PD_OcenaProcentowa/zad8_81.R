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

install.packages( data.tree )
library( data.tree )

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf) {
  if (!is.data.frame(data)) {
    #return ("Dane nie sa ramka danych")
    print ("Dane nie sa ramka danych")
    return(FALSE)
  }
  else if (anyNA(data[,Y]) | anyNA(data[,X])) {
    #return ("Zmienne Y lub X nie istnieja w zbiorze danych!")
    print("Zmienne Y lub X nie istnieja w zbiorze danych!")
    return(FALSE)
  }
  else if (depth<0 | minobs<0) {
   # return ("Niedopuszczalne wartości, mniejsze od zera!")
    print("Niedopuszczalne wartości, mniejsze od zera!")
    return(FALSE)
  }
  else if (type != "Gini" & type != "Entropy" & type != "SS") {
    #return ("Typ niemozliwy do wykonania")
    print("Typ niemozliwy do wykonania")
    return(FALSE)
  }
  else if (overfit != "prune" & overfit != "none") {
    #return ("Overfit przyjmuje niedopuszczalne wartosci")
    print("Overfit przyjmuje niedopuszczalne wartosci")
    return(FALSE)
  }
  else if (cf<=0 | cf>0.5) {
    #return ("Niedopuszczalny przedzial!")
    print("Niedopuszczalny przedzial!")
    return(FALSE)
  }
  else if (is.factor(data$y) & type == "SS") {
    #return ("Niedopuszczalna kombinacja parametrow! Type GG, gdy Y jest faktorem")
    print("Niedopuszczalna kombinacja parametrow! Type GG, gdy Y jest faktorem")
    return(FALSE)
  }
  else if (!is.factor(data$y) & (type == "Entropy" | type == "Gini")) {
    #return("Niedopuszczalna kombinacja parametrow! Type Gini lub Entropy, gdy dane nie sa faktorem")
    print("Niedopuszczalna kombinacja parametrow! Type Gini lub Entropy, gdy dane nie sa faktorem")
    return(FALSE)
  }
  else {
    #RETURN("Operacja mozliwa do wykonania!")
    #print("Operacja mozliwa do wykonania!")
    return (TRUE)
  }
}



# Zadanie 2:
# a) Stwórz funkcję "AssignInitialMeasures" przyjmującą nastęujące parametry: "tree", "Y", "data", "type", "depth".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (czyli korzenia) wartości początkowe:
#    - "depth" = 0.
#    - w zależności od "type" wartość miary Gini, Entropy, SS dla calej populacji (bo to korzeń).
library(rpart)


Prob <- function( y_tar ){
  result <- unname( table( y_tar ) )
  result <- result / sum( result )
  return( result )
} #z zajec


Gini <- function(prob) {
  if (all(prob >= 0 &  prob <= 1 ) == TRUE) {
    # result <- 1 - sum(prob^2)
    # return (result)
    result <- prob^2
    result <- 1-sum( result )
    return( result )
  }
  else {
    return( "Miara prawdopodobienstwa. Musi zawierac sie w przedziale [0,1]" )
  }
}

# Entropy <- function( prob ){
#   result <- prob * log2( prob )
#   result[ prob == 0 ] <- 0
#   result <- -sum( result )
#   return( result )
# }

Entropy <- function(prob){
  if (all(prob >= 0 &  prob <= 1 ) == TRUE) {
      result <- prob * log2( prob )
      result[prob == 0] <- 0
      result <- -sum(result)
      return( result )
  }
  else {
    return( "Miara prawdopodobienstwa. Musi zawierac sie w przedziale [0,1]")
  }
}



SS <- function(y) {
  if (is.factor(y)) {
    return( "Niemozliwe do wykonania. Dziala jedynie dla regresji. Wprowadzono factor")
  }
  else {
    y_1 <- sum(y)/length(y)
    y_2 <- (y - y_1)^2
    result <- sum(y_2)
    return (result)
  }
}


AssignInitialMeasures <- function(tree, Y, data, type, depth){
  #    przypisanie "depth" = 0 z zajec
  tree$depth <- 0
  if (type == "Gini" | type == "gini" | type == "GINI") {
    Gini <- Gini(Prob(data[,Y]))
    tree$Gini <- Gini
  }
  
  else if (type == "Entropy" | type == "entropy" | type == "ENTROPY") {
    Entropy <- Entropy(Prob(data[,Y]))
    tree$Entropy <- Entropy
  }

  else if (type == "SS" | type == "ss") {
    SS <- SS(data[,Y])
    tree$SS <- SS
  }
  else {
    return("Niemozliwe do wykonania, nie jest to Gini, Ent lub SS")
  }
  return (tree)
}


# Zadanie 3:
# a) Stwórz funkcję "AssignInfo" przyjmującą nastęujące parametry: "tree", "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Funkcja powinna na podstawie parametrów wejściowych przypisywać do obiektu "tree" (jako attrybuty obiektu) wartości owych parametrów.


AssignInfo <- function( tree, Y, X, data, type, depth, minobs, overfit, cf){
  attr(tree,"Y") <- c(Y)
  attr(tree,"X") <- c(X)
  attr(tree,"data") <- data
  attr(tree,"tree") <- c(type)
  attr(tree,"depth") <- c(depth)
  attr(tree,"minobs") <- c(minobs)
  attr(tree,"overfit") <- c(overfit)
  attr(tree,"cf") <- c(cf)
  return (tree)
}


# Zadanie 4:
# a) Stwórz funkcję "Tree" przyjmującą nastęujące parametry: "Y", "X", "data", "type", "depth", "minobs", "overfit", "cf".
# b) Jest to rozwinięcie funkcji ze slajdu nr 19. Funckja powinna po kolei wywoływać pozostałe funkcje:
#    - "StopIfNot", jeżeli zwracana wartość to "FALSE" to kończymy działanie całej funkcji (zwracamy obiekt niewidzialny),
#    - tworzenie obiektu "tree",
#    - "AssignInitialMeasures",
#    - "BuildTree",
#    - "PruneTree", na tę chwilę ta funkcja jest pusta PruneTree<-function(){},
#    - "AssignInfo".
# c) Funkcja powwina zwracać obiekt "tree".



#split num, split Var, zajecia 9 data: MIKOŁAJKI 5.12.2021


SpliNum <- function(Y, x, parentVal, splits, minobs, type) {
        n <- length(x)
        res <- data.frame(matrix( 0, length(splits), 6))
        colnames(res) <- c("InfGain","lVal","rVal","point","ln","rn")
        
        for(i in 1:length(splits)){
          partition <- x <= splits[i] # %in%: x %in% c("wyzsze","podstawowe")
          ln <- sum(partition)
          rn <- n - ln
          check <- any(c(ln,rn) < minobs)
          
          if(check == TRUE) {
            
            res[i,] <- 0
            
          }
          
          
          else {
            if (type == "Entropy") {
              lVal <- Entropy(Prob(Y[partition]))
              rVal <- Entropy(Prob(Y[!partition]))
              InfGain <- parentVal - ( lVal * ln/n  + rVal * rn/n )
            }
            
            else if (type == "Gini") {
              lVal <- Gini(Prob(Y[partition]))
              rVal <- Gini(Prob(Y[!partition]))
              InfGain <- parentVal - (lVal * ln/n  + rVal * rn/n)
            }
            
            
            else {
              lVal <- SS(Y[partition])
              rVal <- SS(Y[!partition])
              InfGain <- parentVal - (lVal * ln/n  + rVal * rn/n)
            }
            
            res[i,"InfGain"] <- InfGain
            res[i,"lVal"] <- lVal
            res[i,"rVal"] <- rVal
            res[i,"point"] <- splits[i]
            res[i,"ln"] <- ln
            res[i,"rn"] <- rn
          }
        }
        
        return(res)
}# SpliNum


SplitVar <- function(Y, x, parentVal, minobs, type) {
          s <- unique(x)
          if (length(x) == 1) {
            splits <- s
          }
          else {
            splits <- head(sort( s ), -1)
          }
          res <- SpliNum(Y, x, parentVal, splits, minobs, type)
          
          incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
          res <- res[incl, , drop = F]
          
          best <- which.max(res$InfGain)
          # ten który daje zbilansowany podzial
          res <- res[best, , drop = F]
          
          return(res)
}#SpliVar


#NAJLEPSZA KOMBINACJA Z ZAJEC9
FindBestSplit <- function(Y, data, parentVal, minobs, type) {
          X <- !colnames(data) %in% Y
          
          res <- sapply(colnames( data )[X], function(i) {
            SplitVar( data[,Y], data[,i], parentVal, minobs, type)
          }, simplify = F)
          
          res <- do.call("rbind", res)
          
          best <- which.max(res$InfGain)
          res <- res[best, , drop = F]
          
          return(res)
}# FindBestSplit



BuildTree <- function(node, Y, X, data, depth, minobs, type){
  node$Count <- nrow(data)
  
  if (is.factor(data[,Y])) {
    node$Prob <- Prob(data[,Y])
    node$Class <- levels(data[,Y])[which.max(node$Prob)]
  }
  else {
    node$Prob <- SS(data[,Y])
    node$Class <- mean(data[,Y])
  }
  
  bestSplit <- FindBestSplit(Y, data, node$inf, minobs, type)
  
  ifStop <- nrow(bestSplit) == 0
  
  if ((is.factor(data[,Y]) & (node$Depth == depth | ifStop | all(node$Prob %in% c(0,1)))) | (!is.factor(data[,Y]) & (node$Depth == depth | ifStop))) {
    node$Leaf <- "*"
    return (node)
  }
  else {
    split_indx <- data[,rownames(bestSplit)] <= bestSplit$point
    child_frame <- split( data, split_indx )
    
    name <- sprintf("%s <= %s", rownames(bestSplit), bestSplit$point)
    child_l <- node$AddChild(name)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree(child_l, Y, X, child_frame[["TRUE"]], depth, minobs, type)
    
    name <- sprintf("%s >  %s", rownames(bestSplit), bestSplit$point)
    child_r <- node$AddChild(name)
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree(child_r, Y, X, child_frame[["FALSE"]], depth, minobs, type)
  }
}# BulitTree

PruneTree <- function() {

}


Tree <- function(Y, X, data, type, depth = 3, minobs = 5, overfit = "none", cf = 0.2) {
  if (StopIfNot(Y, X, data, type, depth, minobs, overfit, cf) == FALSE){
    #print("Podano zle wartosci! ")
    stop("Zle wartosci")
  }
  
  else {
    tree <- Node$new("Root")
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
    
    tree <- AssignInitialMeasures(tree,Y,data,type,depth)
    BuildTree(tree, Y, X, data, depth, minobs, type)
    PruneTree()
    tree <- AssignInfo(tree,Y,X,data,type,depth,minobs,overfit,cf)
    
    return (tree)
  }
} #Tree


# Zadanie 5:
# a) Dokonaj integracji opracowanej funkcji "FindBestSplit" z funkcjami "Tree" oraz "BuildTree".
