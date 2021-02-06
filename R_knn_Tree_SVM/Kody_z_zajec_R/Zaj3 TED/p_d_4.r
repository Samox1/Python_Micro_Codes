#Zad 1

KNNtrain <- function(X, y_tar, k, XminNew, XmaxNew) {
  if (any(is.na(X) == TRUE || is.na(y_tar) == TRUE)) {
    return("Niekompletne dane")
  }
  else{
    if (k > 0 && (is.matrix(X) == TRUE || is.data.frame(X) == TRUE)) {
      nazwa <- vector()
      column_names <- colnames(X)
      minOrg <- vector()
      maxOrg <- vector()
      minmaxNew <- c(XminNew, XmaxNew)
      for (name in column_names) {
        if (class(X[name][1, ]) == 'numeric') {
          nazwa <- append(nazwa, name)
          minOrg <- append(minOrg, min(X[name]))
          maxOrg <- append(maxOrg, max(X[name]))
          X[name] <-
            ((X[name] - min(X[name])) / (max(X[name]) - min(X[name]))) * (XmaxNew - XminNew) + XminNew
        }
      }
      names(maxOrg) <- nazwa
      names(minOrg) <- nazwa
      attr(X, 'minOrg') <- minOrg
      attr(X, 'maxOrg') <- maxOrg
      attr(X, 'minmaxNew') <- minmaxNew
      
      knn <- list()
      knn[["X"]] <- X
      knn[["y"]] <- y_tar
      knn[["k"]] <- k
      
    }
    return(knn)
  }
}

#Zad 2

d_euklides <- function(x_i, x_n) {
  return(sqrt(sum((x_i - x_n) ^ 2)))
}

d_hamming <- function(x_i, x_n, p) {
  suma <- 0
  for (j in 1:p) {
    if (x_i[j] == x_n[j]) {
      a[j] = 1
    }
    else{
      a[j] = 0
    }
    suma <- suma + a[j]
  }
  return(suma / p)
}

d_gower <- function(x_i, x_n, p, skala) {
  G <- 0
  suma_G <- 0
  if (skala == 'ilorazowa') {
    for (j in 1:p) {
      G[j] <- (abs(x_i[j] - x_n[j])) / (max(x_i) - min(x_i))
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
  else if (skala == 'porz¹dkowa') {
    z <- 0
    for (j in 1:p) {
      z[j] <- (x_i[j] - 1) / (max(x_i - 1))
      G[j] <- (abs(z[j] - p)) / (max(z[j]) - min(z[j]))
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
  else if (skala == 'nominalna') {
    for (j in 1:p) {
      if (x_i[j] == x_n[j]) {
        G[j] = 1
      }
      else{
        G[j] = 0
      }
      suma_G <- suma_G + G[j]
    }
    return(suma_G / p)
  }
}


KNNpred <- function(KNNmodel, X) {
  if (any(is.na(KNNmodel) == TRUE || is.na(X) == TRUE)) {
    return("Niekompletne dane")
  }
  else{
    dFrame <- as.data.frame(KNNmodel$X)
    if (nrow(dFrame) == nrow(X)) {
      #regresja
      nTrain <- nrow(KNNmodel$X)
      nPred <- nrow(X)
      odl_eukl <- matrix(0, nTrain, nPred)
      odl_hamming <- matrix(0, nTrain, nPred)
      for (i in 1:nTrain) {
        for (j in 1:nPred) {
          odl_eukl[i, j] <- d_euklides(KNNmodel$X[i, ], X[j, ])
          odl_hamming[i, j] <- d_hamming(KNNmodel$X[i, ], X[j, ])
          if (ncol(Filter(is.numeric, X)) != 0 &
              ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'ilorazowa'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'nominalna'
          }
          else if (ncol(Filter(is.numeric, X)) == 0 &
                   ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
            skala = 'porz¹dkowa'
          }
          odl_gower[i, j] <- d_gower(KNNmodel$X[i, ], X[j, ], skala)
        }
      }
      pred <- double(nPred)
      for (i in 1:nPred) {
        kNaj_eukl <- order(odl_eukl[, i])
        kNaj_hamming <- order(odl_hamming[, i])
        kNaj_gower <- order(odl_gower[, i])
        kNaj_eukl <- kNaj_eukl[1:KNNmodel$k]
        kNaj_hamming <- kNaj_hamming[1:KNNmodel$k]
        kNaj_gower <- kNaj_gower[1:KNNmodel$k]
        y_hat_eukl <- mean(KNNmodel$y[kNaj_eukl])
        y_hat_hamming <- mean(KNNmodel$y[kNaj_hamming])
        y_hat_gower <- mean(KNNmodel$y[kNaj_gower])
        pred_eukl[i] <- y_hat_eukl
        pred_hamming[i] <- y_hat_hamming
        pred_gower[i] <- y_hat_gower
      }
      
      if (ncol(Filter(is.numeric, X)) != 0 &
          ncol(Filter(is.factor, X)) == 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_eukl)
      }
      else if (ncol(Filter(is.numeric, X)) == 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) == 0) {
        return(pred_hamming)
      }
      else if (ncol(Filter(is.numeric, X)) != 0 &
               ncol(Filter(is.factor, X)) != 0 & ncol(Filter(is.ordered, X)) != 0) {
        return(pred_gower)
      }
    }
  }
}
