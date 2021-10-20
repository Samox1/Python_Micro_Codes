################################################
plik_wieloklasowa<-file("https://archive.ics.uci.edu/ml/machine-learning-databases/00528/dataset.csv",open="r")
wieloklasowa <- read.table(plik_wieloklasowa, dec=" ", header=T)

Y_data_kl_w<-as.numeric(data_kl_w[,9])
SequenceName<-as.numeric(data_kl_w[,1])

library(caret)
library(rpart)
library(nnet)
library(neuralnet)
library(kernlab)
library(e1071)
#DANE KLASYFIKACJA WIELOKLASOWA
Wieloklasowa <- as.data.frame(dermatology)
Wieloklasowa$V34 <- as.numeric(Wieloklasowa$V34)
isna<-is.na(Wieloklasowa$V34)
Wieloklasowa$V34 <- ifelse(is.na(Wieloklasowa$V34),0,Wieloklasowa$V34)
srednia<-mean(Wieloklasowa$V34)
Wieloklasowa$V34 <- ifelse(Wieloklasowa$V34==0,35,Wieloklasowa$V34)
View(Wieloklasowa)

# DRZEWO DECYZYJNE FUNKCJA WBUDOWANA
library(tree)
Wieloklasowa.ltr <- tree(V35~., Wieloklasowa)
Wieloklasowa.ltr

# K-NAJBLIZSZYCH SASIADOW FUNKCJA WBUDOWANA

##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(Wieloklasowa), 0.9 * nrow(Wieloklasowa)) 

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
##Run nomalization on first 4 coulumns of dataset because they are the predictors
Wieloklasowa_norm <- as.data.frame(lapply(Wieloklasowa[,c(1,2,3,4,5,6,7,8,9,10,
                                                          11,12,13,14,15,16,17,18,19,
                                                          20,21,22,23,24,25,26,27,28,29,
                                                          30,31,32,33,34)], nor))

summary(Wieloklasowa_norm)
##extract training set
Wieloklasowa_train <- Wieloklasowa_norm[ran,] 
##extract testing set
Wieloklasowa_test <- Wieloklasowa_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
Wieloklasowa_target_category <- Wieloklasowa[ran,35]
##extract 5th column if test dataset to measure the accuracy
Wieloklasowa_test_category <- Wieloklasowa[-ran,35]
##load the package class
library(class)
##run knn function
pr <- knn(Wieloklasowa_train,Wieloklasowa_test,cl=Wieloklasowa_target_category,k=5)

##create confusion matrix
tab <- table(pr,Wieloklasowa_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

# DRZEWO DECYZYJNE - KOD WLASNY
rpart(formula = V35~., data = Wieloklasowa, minsplit = 1, maxdepth = 3, cp = 0)

Prob <- function( y ){
  res <- unname( table( y ) )
  res <- res / sum( res )
  return( res )
}

prob<-Prob(Wieloklasowa$V35)

Entropy <- function( prob ){
  res <- prob * log2( prob )
  res[ prob == 0 ] <- 0
  res <- -sum( res )
  return( res )
}

Entropy(prob)

SplitNum <- function(Y, x, parentVal, splits, minobs){
  n <- length(x)
  res <- data.frame(matrix(0,length(splits),6))
  colnames(res) <- c("InfGain","lVal","rVal","point","ln","rn")
  for(i in 1:length(splits)){
    partition <- x <= splits[i]
    ln <- sum(partition)
    rn <- n - ln
    if(any(c(ln,rn) < minobs)){
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
SplitNum( Y = Wieloklasowa$V35 , x = Wieloklasowa[,-35], parentVal = 1, splits = head(sort(Wieloklasowa[,-35]),-1), minobs = 2 )

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
SplitVar( Y = Wieloklasowa$V35 , x = Wieloklasowa[,-35], parentVal = 1, minobs = 2 )
SplitVar( Y = Wieloklasowa$V35 , x = c(Wieloklasowa$V1,Wieloklasowa$V2,Wieloklasowa$V3,Wieloklasowa$V4,
                                       Wieloklasowa$V5,Wieloklasowa$V6,Wieloklasowa$V7,Wieloklasowa$V8,
                                       Wieloklasowa$V9,Wieloklasowa$V10,Wieloklasowa$V11,Wieloklasowa$V12,
                                       Wieloklasowa$V13,Wieloklasowa$V14,Wieloklasowa$V15,Wieloklasowa$V16,
                                       Wieloklasowa$V17,Wieloklasowa$V18,Wieloklasowa$V19,Wieloklasowa$V20,
                                       Wieloklasowa$V21,Wieloklasowa$V22,Wieloklasowa$V23,Wieloklasowa$V24,
                                       Wieloklasowa$V25,Wieloklasowa$V26,Wieloklasowa$V27,Wieloklasowa$V28,
                                       Wieloklasowa$V29,Wieloklasowa$V30,Wieloklasowa$V31,Wieloklasowa$V32,
                                       Wieloklasowa$V33,Wieloklasowa$V34), parentVal = 1, minobs = 2 )

FindBestSplit <- function( Y, Xnames, data, parentVal, minobs ){
  res <- sapply( Xnames, function( i ){
    SplitVar( Y = data[,Y] , x = data[,i], parentVal = parentVal, minobs = minobs )
  }, simplify = F )
  res <- do.call( rbind, res )
  best <- which.max( res$InfGain )
  res <- res[ best, , drop = F ]
  return( res )
}

FindBestSplit( Y = "V35", Xnames = c("V1", "V2","V3","V4","V5", "V6","V7","V8",
                                     "V9", "V10","V11","V12","V13", "V14","V15","V16",
                                     "V17", "V18","V19","V20","V21", "V22","V23","V24",
                                     "V25", "V26","V27","V28","V29", "V30","V31","V32","V33","V34"), data = Wieloklasowa, parentVal = 1, minobs = 2 )

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

Drzewko <- Tree( Y = "V35",Xnames = c("V1", "V2","V3","V4","V5", "V6","V7","V8",
                                    "V9", "V10","V11","V12","V13", "V14","V15","V16",
                                    "V17", "V18","V19","V20","V21", "V22","V23","V24",
                                    "V25", "V26","V27","V28","V29", "V30","V31","V32","V33","V34"), data = Wieloklasowa, depth = 3, minobs = 1)
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

library(rpart) #CART
###################################################################
Drzewko <- Tree( Y = "V35",Xnames = c("V1", "V2","V3","V4","V5", "V6","V7","V8",
                                      "V9", "V10","V11","V12","V13", "V14","V15","V16",
                                      "V17", "V18","V19","V20","V21", "V22","V23","V24",
                                      "V25", "V26","V27","V28","V29", "V30","V31","V32","V33","V34"), data = Wieloklasowa, depth = 6, minobs = 5 )
print( Drzewko, "Count", "Class", "Prob", "Leaf" )

library( rpart  ) #CART
rpart( formula = Species~., data = iris, minsplit = 5, maxdepth = 6, cp = 0 )

PE <- function( p, n, z ){
  return( ( p + (z^2)/(2*n) + z*sqrt( p/n - (p^2)/(n) + (z^2)/(4*n^2) ) ) / ( 1 + z^2/n ) )
}
PE( 1 - 0.98, 48, qnorm( 1-0.25 ) )
PE( 1 - 1, 50, qnorm( 1-0.25 ) )

library(data.tree)

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
###################################################################

# K-NAJBLISZYCH SASIADOW - KOD WLASNY
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
