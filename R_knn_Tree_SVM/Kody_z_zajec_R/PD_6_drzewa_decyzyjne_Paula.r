library(data.tree)

StopIfNot <- function(Y, X, data, type, depth, minobs, overfit, cf) {
  
  if (!is.data.frame(data)) {
    mes <<- "Zbior nie jest ramka danych"
    return(FALSE)
  } else if (!all(c(Y,X) %in% names(data))) {
    mes <<- "Zmienne nie wystepuja w ramce danych"
    return(FALSE)
  } else if (any(is.na(data[,c(Y,X)]))) {
    mes <<- "Wystepuja braki danych"
    return(FALSE)
  } else if (depth<=0 | minobs<=0) {
    mes <<- "Ujemne parametry depth/minobs"
    return(FALSE)
  } else if (!(type %in% c("Gini", "Entropy", "SS"))) {
    mes <<- "Bledny typ"
    return(FALSE)
  } else if (!(overfit %in% c("none", "prune"))) {
    mes <<- "Bledny argument overfit"
    return(FALSE)
  } else if (cf<=0 | cf>0.5) {
    mes <<- "Bledny argument cf"
    return(FALSE)
  } else if (type=="SS" & is.factor(data[,Y]) | type=="Gini" & is.numeric(data[,Y]) | 
             type=="Entropy" & is.numeric(data[,Y])) {
    mes <<- "Niedozwolona kombinacja parametrow"
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}

Prob <- function(y){
  
  res <- unname(table(y))
  res <- res/sum(res)
  return(res)
  
}

Gini <- function(prob){
  
  res <- prob^2
  res <- 1-sum(res)
  return(res)
  
}

Entropy <- function(prob){
  
  res <- prob*log2(prob)
  res[prob == 0] <- 0
  res <- -sum(res)
  return(res)
  
}

SS <- function(y){
  
  res <- (y-mean(y))^2
  res <- sum(res)
  return(res)
  
}

AssignInitialMeasures <- function(tree, Y, data, type, depth) {
  
  tree$Depth <- 0
  
  if (type=="Gini") {
    tree$inf <- Gini(Prob(data[,Y]))
  } else if (type=="Entropy") {
    tree$inf <- Entropy(Prob(data[,Y]))
  } else {
    tree$inf <- SS(data[,Y])
  }
  
}

AssignInfo <- function(tree, Y, X, data, type, depth, minobs, overfit, cf) {
  
  tree$Y <- data[,Y]
  tree$X <- data[,X]
  tree$data <- data
  tree$type <- type
  tree$Depth <- depth
  tree$minobs <- minobs
  tree$overfit <- overfit
  tree$cf <- cf
  
}


# zadanie 1

# wszystkie mozliwe podzialy dla danej zmiennej ciaglej
SpliNum <- function(Y, x, parentVal, splits, minobs, type) {
  
  n <- length(x)
  res <- data.frame(matrix(0, length(splits), 6))
  colnames(res) <- c("InfGain", "lVal", "rVal", "point", "ln", "rn")
  
  for (i in 1:length(splits)) {
    partition <- x <= splits[i] 
    ln <- sum(partition)
    rn <- n - ln
    
    ## w tym miejscu musia�am wy��czy� fragment kodu, poniewa� ca�y czas wyrzuca�o mi b��d
    ## niestety nie potrafi�am zidentyfikowa� przyczyny
    
    # if (any(c(ln, rn) < minobs)) {
    #   res[i, ] <- 0
    #   
    # } else{
    lVal <- 
      if (type=="Gini") {
        Gini(Prob(Y[partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[partition]))
      } else {
        SS(Y[partition])
      }   
    
    rVal <-         
      if (type=="Gini") {
        Gini(Prob(Y[!partition]))
      } else if (type=="Entropy") {
        Entropy(Prob(Y[!partition]))
      } else {
        SS(Y[!partition])
      }  
    InfGain <- parentVal - (lVal * ln / n  + rVal * rn / n)
    
    res[i, "InfGain"] <- InfGain
    res[i, "lVal"] <- lVal
    res[i, "rVal"] <- rVal
    res[i, "point"] <- ifelse(is.numeric(splits[i]), splits[i], as.character(splits[i]))
    res[i, "ln"] <- ln
    res[i, "rn"] <- rn
    
    # }
    
  }
  
  return(res)
  
}

# najlepszy podzial dla danej zmiennej ciaglej
SplitVar <- function(Y, x, parentVal, minobs, type) {
  
  s <- unique(x)
  if (length(x) == 1) {
    splits <- s
    
  } else{
    splits <- head(sort(s),-1)
    
  }
  
  res <- SpliNum(Y, x, parentVal, splits, minobs, type)
  
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0
  res <- res[incl, , drop = F]
  
  best <- which.max(res$InfGain)
  
  res <- res[best, , drop = F]
  
  return(res)
  
}

### definicja funkcji

FindBestSplit <- function(Y, X, data, parentVal, type, minobs) {
  
  # zamiana zmiennej objasniajacej nominalnej na porzadkowa
  if (is.numeric(data[,Y])) { # regresja
    for (zm in X) {
      if (!is.numeric(data[,zm]) & !is.ordered(data[,zm])) { 
        a <- tapply(data[,Y], data[,zm], mean)
        a <- sort(a)
        data[,zm] <- factor(data[,zm], levels = names(a), ordered = TRUE)
      }
    }
    
  } else { # klasyfikacja
    for (zm in X) {
      if (!is.numeric(data[,zm]) & !is.ordered(data[,zm])) { 
        posCat <- 1 # kategoria pozytywna zmiennej Y
        temp <- data[data[,Y]==posCat,] 
        a <- prop.table(table(temp[,zm]))
        a <- sort(a)
        data[,zm] <- factor(data[,zm], levels = names(a), ordered = TRUE)
      }
    }
  }
  
  res <- sapply(X, function(i) {
    
    SplitVar(data[, Y], data[, i], parentVal, minobs, type)
    
  }, simplify = F)
  
  res <- do.call("rbind", res)
  
  best <- which.max(res$InfGain)
  res <- res[best, , drop = F]
  
  return(res)
  
}


### testowanie

# zbiory danych
dane <- iris
dane <- dane[dane$Species!="setosa",]
dane$Species <- as.character(dane$Species)
dane$Species[dane$Species=="versicolor"] <- 0
dane$Species[dane$Species=="virginica"] <- 1
dane$Species <- factor(dane$Species)
dane$zm1 <- sample(c("a","b","c"), 100, TRUE)
dane$zm2 <- sample(c("kot","pies","ryba","krowa"), 100, TRUE)


FindBestSplit(Y="Species", X=names(dane)[-5], data=dane, parentVal=1, type="Gini", minobs=2)
FindBestSplit(Y="Species", X=names(dane)[6:7], data=dane, parentVal=1, type="Entropy", minobs=2)




# zadanie 2 

BuildTree <- function(node, Y, X, data, depth, type, minobs) {
  node$Count <- nrow(data)
  node$Prob <- Prob(data[, Y])
  
  bestSplit <- FindBestSplit(Y, X, data, node$inf, type, minobs)
  
  ifStop <- nrow(bestSplit) == 0
  
  if (node$Depth == depth |
      ifStop | all(node$Prob %in% c(0, 1))) {
    node$Leaf <- "*"
    return(node)
    
  } else{
    split_indx <- data[, rownames(bestSplit)] <= bestSplit$point
    child_frame <- split(data, split_indx)
    
    name <-
      sprintf("%s <= %s", rownames(bestSplit), bestSplit$point)
    child_l <- node$AddChild(name)
    child_l$value <- split_indx
    child_l$Depth <- node$Depth + 1
    child_l$inf <- bestSplit$lVal
    
    BuildTree(child_l, Y, X, child_frame[[1]], depth, type, minobs)
    
    name <-
      sprintf("%s >  %s", rownames(bestSplit), bestSplit$point)
    child_r <- node$AddChild(name)
    child_r$value <- split_indx
    child_r$Depth <- node$Depth + 1
    child_r$inf <- bestSplit$rVal
    
    BuildTree(child_r, Y, X, child_frame[[2]], depth, type, minobs)
    
  }
  
}

Tree <- function(Y, X, data, type, depth, minobs, overfit, cf){
  
  if (!StopIfNot(Y, X, data, type, depth, minobs, overfit, cf)) stop(mes, call. = FALSE)
  
  tree <- Node$new("Root")
  
  AssignInitialMeasures(tree, Y, data, type, depth)
  
  BuildTree(tree, Y, X, data, depth, type, minobs)
  
  PruneTree <- function(){}
  
  AssignInfo(tree, Y, X, data, type, depth, minobs, overfit, cf)
  
  return(tree)
  
}


drzewko <- Tree(Y="Species", X=names(iris)[-5], data=iris, 
                type="Entropy", depth=6, minobs=5, overfit="prune", cf=0.05)
drzewko

data(iris)

iris

iris_2 <- iris
iris_2$Species <- factor( rep(0:1, each = 75) )

iris_3 <- iris_2
iris_3$Petal.Length <- factor( iris_3$Petal.Length )

iris_4 <- iris_2
iris_4$Petal.Length <- factor( iris_4$Petal.Length, ordered = T )