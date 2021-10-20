library(parallel)
library(biglm)
library(bigmemory)
library(pryr)
library(data.table)
library("doParallel")
library("doSNOW")



### Zad. 0

set.seed = (555)
macierz <- matrix(nrow = 1000, ncol = 1001)
colnames(macierz) <- c("Y", paste("x_", seq(1:(ncol(macierz)-1)), sep = ""))
macierz[,'Y'] <- runif(nrow(macierz), min = 1, max = 100)

set.seed = (555)
for (i in 1:nrow(macierz)) {
  for (j in 2:ncol(macierz)) {
    macierz[i,j] <- macierz[i,1] + rnorm(1)
  }
}
rm(i)
rm(j)


### Zad. 1

ModelParallel <- function(dane, Ynazwa, XnazwyList, Nrdzeni, metoda) {
  lista_wynik <- list()
  dane = data.frame(dane)
  if (metoda == 'for') {
    wynik <- foreach(i = 1:length(XnazwyList), .export = c("lista_wynik")) %dopar% 
                {
                  # Opisywanie formuly dla LM
                  model <- paste("Y ~", XnazwyList[[i]][1])
                  j = 1
                  while (j < length(XnazwyList[[i]])) 
                  {
                    model <- paste(model, "+", XnazwyList[[i]][j + 1])
                    j = j + 1
                  }
                  # Model LM
                  model_wyn <- lm(model, data = dane)
                  
                  lista_wynik[[i]] = cbind(names(coef(model_wyn)), coef(model_wyn))
                }
             
  }
  else if (metoda == 'lapply'){
    b <- list()
    wynik_lap <- function( x ){
      rownanie <- paste(paste(Ynazwa, '~', sep=' '), XnazwyList[[x]][1])
      j=1
      while (j<length(XnazwyList[[x]])) {
        rownanie <- paste(rownanie,"+",XnazwyList[[x]][j+1])
        j=j+1
      }
      model <- lm(rownanie, data= data.frame(dane))
      b[[x]] = cbind(summary(model)$coefficients[,"Estimate"])
    }
    srodowisko <- new.env()
    srodowisko$b <- b
    srodowisko$XnazwyList <- XnazwyList
    srodowisko$dane <- dane
    clusterExport(klaster, c("b", "XnazwyList", "dane"), envir=srodowisko)
    parLapply(klaster, 1:length(XnazwyList),wynik_lap)
  }
}


### Zad. 3

# Zad. 1) 

nCores <- detectCores()
klaster <- makeCluster(nCores)
registerDoParallel(klaster)
Xnazwy <- paste("x_", seq(1:(ncol(macierz)-1)), sep = "")

lista_LM <- ModelParallel(macierz, 'Y', list('x_1', c('x_1', 'x_5', 'x_7'), 'x_2', 'x_3'), nCores, 'for')
lista_LM <- ModelParallel(macierz, 'Y', Xnazwy, nCores, 'for')

lista_LM <- ModelParallel(macierz, 'Y', list('x_1', c('x_1', 'x_5', 'x_7'), 'x_2', 'x_3'), nCores, 'lapply')







