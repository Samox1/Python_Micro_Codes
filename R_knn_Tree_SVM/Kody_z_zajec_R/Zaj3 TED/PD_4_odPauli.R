#zadanie 1

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
        c("Niepoprawne dane!")
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
    c("Niepoprawne dane!")
  }  
}

#zadanie 2

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
    if(num == ncol(X_norm)){ #skala ilorazowa - odl. euklidesa
      for(i in 1:nrow(KNNmodel$X)){
        for(j in 1:nrow(X_norm)){
          odl[ i, j ] <- sqrt(sum( (KNNmodel$X[i,] - X_norm[j,])^2 ))
        }
      }
    }
    else if(fac_o == ncol(X_norm)){ #skala porządkowa
      for(i in 1:nrow((KNNmodel$X))){
        for(j in 1:nrow(X_norm)){
          for (k in 1:ncol(X_norm)) {
            uniq <- length(unique(X_norm[,k]))
            odl[i, j] <- (sum( abs(as.numeric(KNNmodel$X[i,]) - as.numeric(X_norm[j,]))  / (uniq - 1)) )
          }
        }
      }
    }
    else if(fac == ncol(X_norm)){ #skala nominalna - odl.Hamminga
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
    ########################################   
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
    
    ########################################    
  }
  
  else{
    c("Niepoprawne dane!")
  }
}

KNNmodel <- KNNtrain( X1, y_tar, k = 5, 0,1 )
X <- iris[1:20,1:3]
X1 <- iris[1:20,2:4]
y_tar <- rnorm( 20 )

KNNpred(KNNmodel, X)



#odległość euklidesowa na skali ilorazowej
set.seed(123)
X<-matrix(runif(10,5,10),5,5)
X1 <-matrix(runif(10,4,11),10,5)


#skala porządkowa
X<-data.frame(fact_1_10 = factor(c(1:10),ordered = TRUE), 
              abc = factor(c("a","b","c","a","b","c","a","b","c","a"), levels = c("a", "b", "c"), ordered = TRUE), 
              xyz = factor(c("x","y","x","z","y","x","x","z","y","x"),levels = c("y", "x","z"), ordered = TRUE))
X1<-data.frame(fact_1_10 = factor(sample(1:10,10,T),ordered = TRUE), 
               abc = factor(c("a","a","c","b","b","c","a","b","c","a"), levels = c("a", "b", "c"), ordered = TRUE), 
               xyz = factor(c("x","x","x","x","y","x","x","z","y","x"),levels = c("y", "x","z"), ordered = TRUE))

y_tar <- factor(c("x","x","y","y","z","x","x","y","y","z"))

#odległość Hamminga dla skali nominalnej
X1<-data.frame(l=factor(c(1:5)),xyz=factor(c("x","x","y","y","z")),l2=factor(c(5:9)))
X<-data.frame(l=factor(c(1,6,3,6)),xyz=factor(c("x","x","y","y")),l2=factor(c(9:6)))

#odległość Gowera dla skali mieszanej
odl <- matrix(0, nrow(X_norm), nrow(KNNmodel$X))
for(i in 1:nrow(KNNmodel$X)){
  for(j in 1:nrow(X_norm)){
    for (k in 1:ncol(X_norm)) {
      if(is.numeric(X_norm[,k])){ #ilorazowa
        odl[i, j] <- (sum(abs(X_norm[i,] - KNNmodel$X[j,])/(max(X_norm[i,]) - min(X_norm[i,])) )) / ncol(X_norm) 
      }
      else if(is.factor(X_norm[,k])){ #nominalna
        odl[i, j] <- (sum(X_norm[i,] != KNNmodel$X[j,])) / ncol(X_norm) 
      }
      else if(is.ordered(X_norm[,k])){ #porządkowa
        odl[i, j] <- (sum(abs(as.numeric(X_norm[i,]-1)/(max(length(unique(X_norm[,k])))-1) - 
                                as.numeric(KNNmodel$X[j,]-1)/(max(length(unique(X_norm[,k])))-1))/
                            (max((as.numeric(X_norm[i,])-1)/(max(length(unique(X_norm[,k])))-1)) -
                               min((as.numeric(X_norm[i,])-1)/(max(length(unique(X_norm[,k])))-1)))) ) / 
          ncol(X_norm)
      }
    }
  }
}
odl
X<-data.frame(n1 = c(1:10), 
              abc = factor(c("a","b","c","a","b","c","a","b","c","a")), 
              xyz = factor(c("x","y","x","z","y","x","x","z","y","x"),levels = c("y", "x","z"), ordered = TRUE))
X1<-data.frame(n1 = sample(1:10,10,T), 
               abc = factor(c("a","a","c","b","b","c","a","b","c","a")), 
               xyz = factor(c("x","x","x","x","y","x","x","z","y","x"),levels = c("y", "x","z"), ordered = TRUE))






