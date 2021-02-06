#Zad 1

install.packages("parallel")
library(parallel)

nCores <- detectCores()
klaster <- makeCluster(nCores)
registerDoParallel(klaster)

ModelParallel <-
  function(dane, Ynazwa, XnazwyList, Nrdzeni, metoda) {
    lista_wynik <- list()
    dane = data.frame(dane)
    if (metoda == 'for') {
      wynik <-
        foreach(i = 1:length(XnazwyList),
                .export = c("lista_wynik")) %dopar% {
                  model <- paste("Y ~", XnazwyList[[i]][1])
                  j = 1
                  while (j < length(XnazwyList[[i]])) {
                    model <- paste(model, "+", XnazwyList[[i]][j + 1])
                    j = j + 1
                  }
                  model_wyn <- lm(model, data = dane)
                  lista_wynik[[i]] = cbind(names(coef(model_wyn)), coef(model_wyn))
                }
      wynik
    }
    
    if (metoda == 'lapply') {
      parLapply(klaster, 1:Nrdzeni, function() {
        model <- lm(dane[, Ynazwa] ~ dane[, XnazwyList[klaster]])
        tabela <- cbind(names(coef(model)), coef(model))
        lista_wynik[klaster] <- tabela
      })
      lista_wynik
    }
  }
ModelParallel(macierz, 'Y', list(x_1, c(x_1, x_5, x_7), x_2, x_3), 4, 'for')


#Zad 2

install.packages('biglm')
library(biglm)
install.packages("bigmemory")
library(bigmemory)
install.packages('pryr')
library(pryr)
install.packages('data.table')
library(data.table)
install.packages("doParallel")
library("doParallel")
install.packages("doSnow")
library("doSnow")

rozmiar <- 50 * 1048576

Filtr <- function(dane, filtr) {
  dane = data.table(dane)
  if (object_size(dane) > rozmiar) {
    small <- as.big.matrix(dane)
    if (object_size(small) > rozmiar) {
      c("Za du¿y rozmiar")
    }
  }
  else{
    p <- eval(parse(text = filtr))
    macierz[p]
  }
}

Filtr(macierz, 'macierz$x_230 > 100')
