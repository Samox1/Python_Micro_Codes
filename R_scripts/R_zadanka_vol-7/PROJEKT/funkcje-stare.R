

normalizacja <- function(x){
  return((x- min(x)) /(max(x)-min(x)))
}

MAE <- function( y_tar, y_hat ){
  return( mean(abs(y_tar - y_hat)) )
}

MSE <- function( y_tar, y_hat ){
  return( mean((y_tar - y_hat)*(y_tar - y_hat)) )
}


MAPE <- function(y_tar, y_hat){
  return( mean(abs((y_tar - y_hat)/y_tar)) )
}

Trafnosc <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

Czulosc <- function(x){(x[1]/(x[1]+x[2]) )* 100}


Specyficznosc <- function(x){(x[4]/(x[4]+x[3]) )* 100}

#KNN PAKIET KLASYFIKACJA BINARNA


find_best_knn_kl_b_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,4)
  
  for( k in 1:max_k ){
    
    pred<-knn(x_train,x_pred,cl=y_tar,k=k)
    m[k,1]<-k
    m[k,2]<- Trafnosc(table(pred,y_pred))
    m[k,3]<- Czulosc(table(pred,y_pred))
    m[k,4]<- Specyficznosc(table(pred,y_pred))
  }
  
  colnames(m)<-c("K","Trafnosc","Czulosc","Specyficznosc")

  return( m)
}

#KNN KOD WLASNY KLASYFIKACJA BINARNA

d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}


KNNtrain <- function( X, y_tar, k = 5 ){
  # k-d tree
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k
  return( knn )
}


KKNpred_kl <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[ i, j ] <- d_euklides( model$X[i,], Xnew[j,] )
    }
  }
  pred <- double( nPred )
  for( i in 1:nPred ){
    kNaj <- order( odl[,i] )
    kNaj <- kNaj[1:model$k]
    y_hat <- Mode( model$y[kNaj] )
    pred[i] <- y_hat
  }
  return( pred )
}


find_best_knn_kl_b_kod <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,4)
  
  for( k in 1:max_k ){
    
    train<-KNNtrain( x_train,  y_tar, k )
    pred<-KKNpred_kl( train, x_pred )
    m[k,1]<-k
    m[k,2]<- Trafnosc(table(pred,y_pred))
    m[k,3]<- Czulosc(table(pred,y_pred))
    m[k,4]<- Specyficznosc(table(pred,y_pred))
  }
  
  colnames(m)<-c("K","Trafnosc","Czulosc","Specyficznosc")

  return( m)
}


#DRZEWA DECYZYJNE PAKIET KLASYFIKACJA BINARNA

find_best_tree_kl_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_deph,cp){
  m <- matrix(0,max_deph,4)
  
  for( k in 1:max_deph ){
    tree_train<-rpart( formula = y_tar~., data = x_train, minsplit = 2, maxdepth = k, cp = cp, method="class" )
    pred<-table(predict(tree_train, x_pred, type = "class"),y_pred)
    
    m[k,1]<-k
    m[k,2]<- sum(diag(pred)/(sum(rowSums(pred)))) * 100
    m[k,3]<- Czulosc(pred)
    m[k,4]<- Specyficznosc(pred)
    
  }
  
  colnames(m)<-c("MaxDeph","Trafnosc","Czulosc","Specyficznosc")
  
  return( m)
}

#DRZEWA DECYZYJNE KOD WLASNY KLASYFIKACJA BINARNA I WIELOKLASOWA


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

SplitNum <- function( Y, x, parentVal, splits, minobs ){
  n <- length( x )
  res <- data.frame( matrix( 0, length( splits ), 6 ) )
  colnames( res ) <- c("InfGain","lVal","rVal","point","ln","rn")
  for( i in 1:length( splits ) ){
    partition <- x <= splits[ i ]
    ln <- sum( partition )
    rn <- n - ln
    if( any( c(ln,rn) < minobs ) ){
      res[i,] <- 0
    }else{
      lVal <- Entropy( Prob( Y[partition] ) )
      rVal <- Entropy( Prob( Y[!partition] ) )
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

SplitVar <- function( Y, x, parentVal, minobs ){
  s <- unique( x )
  if( length(x) == 1 ){
    splits <- s
  }else{
    splits <- head( sort( s ),-1 )
  }
  res <- SplitNum( Y, x, parentVal, splits, minobs )
  incl <- res$ln >= minobs & res$rn >= minobs & res$InfGain > 0 
  res <- res[ incl, , drop = F ]
  best <- which.max( res$InfGain )
  # ten, ktory  daje zbilansowany podzial
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

#MASZYNA WEKTOROW NOSNYCH PAKIET KLASYFIKACJA BINARNA




find_best_knn_svm_pakiet <- function( x_train, y_tar, x_pred, y_pred, C ){
  m <- matrix(0,C,4)
  
  for( k in 1:C ){
    
    pred<-predict( svm( y = factor(y_tar), x = data_kl_b_Train, kernel = "linear" ), data_kl_b_Pred  )
    m[k,1]<-k
    m[k,2]<- Trafnosc(table(pred,y_pred))
    m[k,3]<- Czulosc(table(pred,y_pred))
    m[k,4]<- Specyficznosc(table(pred,y_pred))
  }
  
  colnames(m)<-c("C","Trafnosc","Czulosc","Specyficznosc")
  
  return( m)
}


#MASZYNA WEKTOROW NOSNYCH KOD WLASNY KLASYFIKACJA BINARNA


Decyzja <- function( X, theta, theta0 ){
  X %*% t( theta ) + theta0
}

Margines <- function( X, y, theta, theta0 ){
  y * Decyzja( X, theta, theta0 )
}

Koszt <- function( margines, theta, C ){
  ( 1/ 2 ) * theta %*% t(theta) + C * sum( pmax( 0, 1 - margines ) ) # apply( tabela, 1, max )
}

trainSVM <- function( X, y, C = 1, lr = 0.001, maxiter = 500 ){
  n <- nrow( X )
  p <- ncol( X )
  theta <- matrix( runif( p ), nrow = 1 ) 
  theta0 <- 0
  koszt <- double( maxiter )
  for( i in 1:maxiter ){
    margines <- Margines( X, y, theta, theta0 )
    koszt[i] <- Koszt( margines, theta, C )
    indxMiss <- which( margines < 1 )
    d_theta <- theta - C * y[ indxMiss ] %*% X[ indxMiss, ]
    d_theta0 <- - C * sum( y[ indxMiss ] )
    theta <- theta - lr * d_theta
    theta0 <- theta0 - lr * d_theta0
  }
  SuppVector_Niepoprawnie <- which( Margines( X, y, theta, theta0 ) <= 1 )
  return( list( Theta = theta, Theta0 = theta0, SupVecNiepop = SuppVector_Niepoprawnie ) )
}

predSVM <- function( X, theta, theta0 ){
  sign( Decyzja( X, theta, theta0 ) )
}




find_best_knn_svm_kod_wlasny <- function( x_train, y_tar, x_pred, y_pred, C ){
  m <- matrix(0,C,4)
  
  for( k in 1:C ){
  
  mwn_kl_b<-trainSVM( as.matrix( x_train ), y_tar, C = k, lr = 0.001, maxiter = 5000 )
  predSVM( as.matrix( data_kl_b_Pred ), mwn_kl_b$Theta, mwn_kl_b$Theta0 )
  
  m[k,1]<-k
  m[k,2]<- Trafnosc(table(predSVM( as.matrix( data_kl_b_Pred ), mwn_kl_b$Theta, mwn_kl_b$Theta0 ),y_pred))
  m[k,3]<- Czulosc(table(predSVM( as.matrix( data_kl_b_Pred ), mwn_kl_b$Theta, mwn_kl_b$Theta0 ),y_pred))
  m[k,4]<- Specyficznosc(table(predSVM( as.matrix( data_kl_b_Pred ), mwn_kl_b$Theta, mwn_kl_b$Theta0 ),y_pred))
  
  }
  colnames(m)<-c("C","Trafnosc","Czulosc","Specyficznosc")
  
  return( m)
}





#KNN PAKIET KLASYFIKACJA WIELOKLASOWA



find_best_knn_kl_w_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,2)
  
  for( k in 1:max_k ){
    
    pred<-knn(x_train,x_pred,cl=y_tar,k=k)
    m[k,1]<-k
    m[k,2]<- Trafnosc(table(pred,y_pred))
  }
  
  colnames(m)<-c("K","Trafnosc")
  
  return(m)
}

knn_kl_w_pakiet_tab <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,2)
  
  for( k in 1:max_k ){
    
    pred<-knn(x_train,x_pred,cl=y_tar,k=k)
    m[k,1]<-k
    m[k,2]<- Trafnosc(table(pred,y_pred))
  }
  
  return(table(pred,y_pred))
}



#KNN KOD KLASYFIKACJA WIELOKLASOWA


KNNtrain <- function( X, y_tar, k = 5 ){
  # k-d tree
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k
  return( knn )
}


KKNpred_kl <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[ i, j ] <- d_euklides( model$X[i,], Xnew[j,] )
    }
  }
  pred <- double( nPred )
  for( i in 1:nPred ){
    kNaj <- order( odl[,i] )
    kNaj <- kNaj[1:model$k]
    y_hat <- Mode( model$y[kNaj] )
    pred[i] <- y_hat
  }
  return( pred )
}



#DRZEWO DECYZYJNE PAKIET KLASYFIKACJA WIELOKLASOWA





find_best_tree_kl_W_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_deph,cp){
  m <- matrix(0,max_deph,2)
  
  for( k in 1:max_deph ){
    tree_train<-rpart( formula = y_tar~., data = x_train, minsplit = 2, maxdepth = k, cp = cp, method="class" )
    pred<-table(predict(tree_train, x_pred, type = "class"),y_pred)
    
    m[k,1]<-k
    m[k,2]<-sum(diag(pred)/(sum(rowSums(pred)))) * 100

    
  }
  
  colnames(m)<-c("MaxDeph","Trafnosc")
  
  return( m)
}





# knn dla regresji - pakiet
find_best_knn_reg_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,4)
  
  for( k in 1:max_k ){
    knnn_train<-knnreg( x_train, y_tar, k )
    pred<-predict(knnn_train,x_pred)
    m[k,1]<-k
    m[k,2]<- MAE(y_tar=y_pred,y_hat=pred)
    m[k,3]<- MSE(y_tar=y_pred,y_hat=pred)
    m[k,4]<- MAPE(y_tar=y_pred,y_hat=pred)
  }
  
  colnames(m)<-c("K","MAE","MSE","MAPE")
  
  return( m)
}

# knn dla regresji - kod wlasny

d_euklides <- function( x_i, x_n ){
  return( sqrt( sum( ( x_i - x_n )^2 ) ) )
}


KNNtrain <- function( X, y_tar, k = 5 ){
  # k-d tree
  knn <- list()
  knn$X <- X
  knn$y <- y_tar
  knn$k <- k
  return( knn )
}


KKNpred <- function( model, Xnew ){
  nTrain <- nrow( model$X )
  nPred <- nrow( Xnew )
  odl <- matrix( 0, nTrain, nPred )
  for( i in 1:nTrain ){
    for( j in 1:nPred ){
      odl[ i, j ] <- d_euklides( model$X[i,], Xnew[j,] )
    }
  }
  pred <- double( nPred )
  for( i in 1:nPred ){
    kNaj <- order( odl[,i] )
    kNaj <- kNaj[1:model$k]
    y_hat <- mean( model$y[kNaj] )
    pred[i] <- y_hat
  }
  return( pred )
}

find_best_knn_reg_kod <- function( x_train, y_tar, x_pred, y_pred, max_k ){
  m <- matrix(0,max_k,4)
  
  for( k in 1:max_k ){
    knnn_train<-KNNtrain( x_train, y_tar, k )
    pred<-KKNpred(knnn_train,x_pred)
    m[k,1]<-k
    m[k,2]<- MAE(y_tar=y_pred,y_hat=pred)
    m[k,3]<- MSE(y_tar=y_pred,y_hat=pred)
    m[k,4]<- MAPE(y_tar=y_pred,y_hat=pred)
  }
  
  colnames(m)<-c("K","MAE","MSE","MAPE")

  return( m)
}


PE <- function( p, n, z ){
  return( ( p + (z^2)/(2*n) + z*sqrt( p/n - (p^2)/(n) + (z^2)/(4*n^2) ) ) / ( 1 + z^2/n ) )
}


PEP <- function( tree, cf = 0.25 ){
  z <- qnorm( 1 - cf )
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

#DRZEWA DECYZYJNE PAKIET REGRESJA

find_best_tree_reg_pakiet <- function( x_train, y_tar, x_pred, y_pred, max_deph ){
  m <- matrix(0,max_deph,4)
  
  for( k in 1:max_deph ){
    tree_train<-rpart( formula = y_tar~., data = x_train, minsplit = 2, maxdepth = k, method="anova")
    pred<-table(y_pred, predict(tree_train, x_pred))
    m[k,1]<-k
    m[k,2]<- MAE(y_tar=y_pred,y_hat=predict(tree_train, x_pred))
    m[k,3]<- MSE(y_tar=y_pred,y_hat=predict(tree_train, x_pred))
    m[k,4]<- MAPE(y_tar=y_pred,y_hat=predict(tree_train, x_pred))
  }
  
  colnames(m)<-c("Deph","MAE","MSE","MAPE")
  
  return(m)
}

