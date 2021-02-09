library(caret)
library(rpart)
library(nnet)
library(neuralnet)
library(kernlab)
library(e1071)
#DANE KLASYFIKACJA BINARNA
Binarna <- as.data.frame(x=transfusion)
colnames(Binarna) <- c("Recency", "Frequency","Monetary","Time","Y_out")
View(Binarna)

#Zmienne są następujące:
#R. Recency - miesiące od ostatniej darowizny
#F. Częstotliwość - całkowita liczba darowizn
#M. Monetary - całkowita oddana krew w cm3 (ml)
#T. Czas - miesiące od pierwszej darowizny
#y. zmienna binarna określająca, czy oddał krew w marcu 2007 r. (1 = tak; 0 = nie)

# Drzewo decyzyjne - jest to kod użyty podczas zajec nr 4 
set.seed(666)
rpart(formula = Y_out~Recency+Frequency+Monetary+Time, data = Binarna, minsplit = 1, maxdepth = 3, cp = 0)

Prob <- function( y ){
  res <- unname( table( y ) )
  res <- res / sum( res )
  return( res )
}

#prob<-Prob(Binarna$Y_out)

Entropy <- function( prob ){
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
}

#Entropy(prob)

SplitNum <- function(Y, x, parentVal, splits, minobs){
  n <- length(x)
  res <- data.frame(matrix(0,length(splits),6))
  colnames(res) <- c("InfGain","lVal","rVal","point","ln","rn")
  for(i in 1:length(splits)){
    partition <- x <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    print(any(c(ln,rn) < minobs))
    if(any(c(ln,rn) < minobs, na.rm = TRUE)){
      res[i,] <- 0
    }else{
      lVal <- Entropy(Prob(Y[partition]))
      rVal <- Entropy(Prob(Y[!partition]))
      InfGain <- parentVal - (ln/n * lVal + rn/n * rVal)
      res[i,"InfGain"] <- InfGain
      res[i,"lVal"] <- lVal
      res[i,"rVal"] <- rVal
      res[i,"point"] <- splits[i]
      res[i,"ln"] <- ln
      res[i,"rn"] <- rn
    }
  }
  return(res)
}
#SplitNum( Y = Binarna$Y_out , x = c(Binarna$Recency,Binarna$Frequency,Binarna$Monetary,Binarna$Time), parentVal = 1, splits = head(sort(unique(c(Binarna$Recency,Binarna$Frequency,Binarna$Monetary,Binarna$Time))),-1), minobs = 2 )

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

#SplitVar( Y = Binarna$Y_out, x = c(Binarna$Recency,Binarna$Frequency,Binarna$Monetary,Binarna$Time), parentVal = 1, minobs = 2 )

FindBestSplit <- function( Y, Xnames, data, parentVal, minobs ){
  res <- sapply( Xnames, function( i ){
    SplitVar( Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs )
  }, simplify = F )
  res <- do.call( rbind, res )
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  return( res )
}

#FindBestSplit( Y = "Y_out", Xnames = c("Recency", "Frequency","Monetary","Time"), data = Binarna, parentVal = 1, minobs = 2 )

library(data.tree)

Tree <- function( Y, Xnames, data, depth, minobs ){
  tree <- Node$new( "Root" )
  tree$Depth <- 0
  tree$Count <- nrow( data )
  tree$Val <- Entropy( Prob( data[,Y] ) )
  BuildTree( tree, Y, Xnames, data, depth, minobs )
  return( tree )
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

Drzewko <- Tree( Y = "Y_out", Xnames = c("Recency", "Frequency","Monetary","Time"), data = Binarna, depth = 3, minobs = 1)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

dane <- iris
dane <- dane[dane$Species!="setosa",]
dane$Species <- as.character(dane$Species)
dane$Species[dane$Species=="versicolor"] <- 0
dane$Species[dane$Species=="virginica"] <- 1

drzeweko <-  Tree( Y = "Species", Xnames = names(dane)[-5], data = dane, depth = 3, minobs = 1)

drzemko <- rpart( formula = Species~., data = dane, minsplit = 1, maxdepth = 3, cp = 0 )
prp(drzemko, type = 0)


PE <- function( p, n, z ){
  return( ( p + (z^2)/(2*n) + z*sqrt( p/n - (p^2)/(n) + (z^2)/(4*n^2) ) ) / ( 1 + z^2/n ) )
}
PE(1 - 0.98, 48, qnorm(1-0.25))
PE(1 - 1, 50, qnorm(1-0.25))

library( data.tree )

PEP <- function( tree, cf = 0.25 ){
  z <- qnorm( 1 - cf )
  # Czy korzeń, Root
  if( length( tree$Get("pathString") ) == 1 ) return( NULL ) 
  liscie_byly <- c()
  repeat{
    sciezka_lisci <- tree$Get( "pathString", filterFun = isLeaf )
    if( all( sciezka_lisci %in% liscie_byly ) | sciezka_lisci[1] == "Root" ) break
    temp <- strsplit( sciezka_lisci[ !sciezka_lisci %in% liscie_byly ][1], "/" )[[1]]
    leaf <- eval( parse( text = paste( "tree", paste0( paste0( "'", temp[-1] ), "'", collapse = "$" ), sep = "$") ) )
    parent <- leaf$parent
    sibling <- leaf$siblings[[1]]
    leaf_class <- leaf$Class
    sibling_class <- sibling$Class
    parent_class <- parent$Class
    leaf_prob <- leaf$Prob
    sibling_prob <- sibling$Prob
    parent_prob <- parent$Prob
    leaf_count <- leaf$Count
    sibling_count <- sibling$Count
    parent_count <- parent$Count
    leaf_isLeaf <- leaf$isLeaf
    sibling_isLeaf <- sibling$isLeaf
    
    leaf_PE <- PE( 1 - max(leaf_prob), leaf_count, z )
    sibling_PE <- PE( 1 - max(sibling_prob), sibling_count, z )
    parent_PE <- PE( 1 - max(parent_prob), parent_count, z )
    
    if_prune <- parent_PE <= leaf_count / parent_count * leaf_PE + sibling_count / parent_count * sibling_PE
    if( if_prune & leaf_isLeaf == sibling_isLeaf ){
      parent$RemoveChild( sibling$name )
      parent$RemoveChild( leaf$name )
      parent$Leaf <- "*"
    }else{
      liscie_byly <- c( liscie_byly, leaf$pathString )
    }
  }
}

print( Drzewko, "Count", "Class", "Prob", "Leaf" )
PEP( Drzewko, cf = 0.25 )
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

#K-najblizszych sasiadow
KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew){
  if(all(!is.na(X)) & all(!is.na(y_tar)) & k>0 & (is.matrix(X) | is.data.frame(X))){
    if(is.matrix(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
    }
    else if(is.data.frame(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
      X_norm <- as.data.frame(X_norm)
    }
    minOrg <- c()
    maxOrg <- c()
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        for (j in 1:nrow(X)) {
          X_norm[j,i] <- ( (X[j,i] - min(X[,i])) / (max(X[,i]) - min(X[,i])) ) * (XmaxNew - XminNew) + XminNew
          minOrg[i] <- min(X[,i])
          maxOrg[i] <- max(X[,i])
        }
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i]) | is.factor(X[,i])){
        X_norm[,i] <- X[,i]
        minOrg[i] <- NA
        maxOrg[i] <- NA
      }
      else{
        c("Niepoprawne dane")
      }
    }
    attr(X_norm,"minOrg") <- minOrg
    attr(X_norm,"maxOrg") <- maxOrg
    attr(X_norm,"minmaxNew") <- c("min"=XminNew,"max"=XmaxNew)
    
    knn <- list()
    knn[["X"]] <- X_norm
    knn[["y"]] <- y_tar
    knn[["k"]] <- k
    return( knn )
  }
  else{
    c("Niepoprawne dane")
  }  
}

# Zadanie 2

KNNpred <- function(KNNmodel, X){
  if(all(!is.na(X)) & ncol(KNNmodel$X)==ncol(X)){
    if(is.matrix(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
    }
    if(is.data.frame(X)){
      X_norm <- matrix(0,nrow(X),ncol(X))
      X_norm <- as.data.frame(X_norm)
    }
    num <- 0
    fac_o <- 0
    fac <- 0
    for (i in 1:ncol(X)) {
      if(is.numeric(X[,i])){
        num <- num + 1
        for (j in 1:nrow(X)) {
          X_norm[j,i] <- ((X[j,i] - attr(KNNmodel$X,"minOrg")[i]) / 
                            (attr(KNNmodel$X,"maxOrg")[i] - attr(KNNmodel$X,"minOrg")[i]))  * 
            (attr(KNNmodel$X,"minmaxNew")["max"] - attr(KNNmodel$X,"minmaxNew")["min"]) + 
            attr(KNNmodel$X,"minmaxNew")["min"]
        }
      }
      else if(is.factor(X[,i]) & is.ordered(X[,i])){
        fac_o <- fac_o + 1
        X_norm[,i] <- X[,i]
      }
      else if(is.factor(X[,i])){
        fac <- fac + 1
        X_norm[,i] <- X[,i]
      }
    }
    odl <- matrix(0, nrow(KNNmodel$X), nrow(X_norm))
    if(num == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[ i, j ] <- sqrt(sum( (KNNmodel$X[i,] - X_norm[j,])^2 ))
        }
      }
    }
    else if(fac_o == ncol(X_norm)){ 
      for(i in 1:nrow((KNNmodel$X))){
        for(j in 1:nrow(X_norm)){
          for (k in 1:ncol(X_norm)) {
            uniq <- length(unique(X_norm[,k]))
            odl[i, j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_norm[j,]))  / (uniq - 1)) )
          }
        }
      }
    }
    else if(fac == ncol(X_norm)){ 
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[i, j] <- ( (sum(KNNmodel$X[i,] != X_norm[j,])) / ncol(X_norm) )
          #odl[i, j] <- ( (sum(KNNmodel$X[i,] == X_norm[j,])) / ncol(X_norm) )
        }
      }
    }
    else{
      c("odległość Gowera")
    } 
    if(is.numeric(KNNmodel$y)){
      pred <- double(nrow(X_norm))
      for( i in 1:nrow(X_norm) ){
        kNaj <- order( odl[,i] )
        kNaj <- kNaj[1:KNNmodel$k]
        y_hat <- mean( KNNmodel$y[ kNaj ] )
        pred[ i ] <- y_hat
      }
      
    }
    else if(is.factor(KNNmodel$y)){
      pred <- c("klasyfikacja")
    }
    return(pred)  
  }
  else{
    c("Niepoprawne dane")
  }
}

KNNmodel <- KNNtrain( X1, y_tar, k = 5, 0,1 )
X <- iris[1:20,1:3]
X1 <- iris[1:20,2:4]
y_tar <- rnorm( 20 )

KNNmodel <- KNNtrain( zbior$R, y_tar, k = 5, 0,1 )
X <- iris[1:20,1:3]
X1 <- iris[1:20,2:4]
y_tar <- rnorm( 20 )

KNNpred(KNNmodel, X)
KNNpred(KNNmodel, zbior$Recency)

#K-najblizszych sasiadow WBUDOWANA

##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(Binarna), 0.9 * nrow(Binarna)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
Binarna_norm <- as.data.frame(lapply(Binarna[,c(1,2,3,4)], nor))

summary(Binarna_norm)
##extract training set
Binarna_train <- Binarna_norm[ran,] 
##extract testing set
Binarna_test <- Binarna_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
Binarna_target_category <- Binarna[ran,5]
##extract 5th column if test dataset to measure the accuracy
Binarna_test_category <- Binarna[-ran,5]
##load the package class
library(class)
##run knn function
pr <- knn(Binarna_train,Binarna_test,cl=Binarna_target_category,k=5)

##create confusion matrix
tab <- table(pr,Binarna_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# DRZEWO DECYZYJNE FUNKCJA WBUDOWANA
library(tree)
Binarna.ltr <- tree(Y_out ~ Recency+Frequency+Monetary+Time, Binarna)
Binarna.ltr

# MASZYNA WEKTORÓW NOSNYCH FUNKCJA WBUDOWANA
library(e1071)
