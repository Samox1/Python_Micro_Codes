# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadanie 1-2.
# 
# Zadanie 0:
# a) Stwórz macierz rozmiaru "1000x1001". 
#    Przypisz nazwę "Y" do pierwszej kolumny. Przypisz nazwy od "x_1" do "x_1000" do następnych kolumn.
# b) Wstaw losowe wartości z wektora od 1 do 100 w kolumnę "Y". set.seed = (555).
# c) Wstaw do kolumn od "x_1" do "x_1000" wartości zgodne z nastepujacym schematem 
#    "x_i = Y + wartość losowa z rozkładu normalnego". set.seed = (555).
# 
# Zadanie 1:
# a) Stwórz funkcję przyjmującą nastęujące parametry: "dane", "Ynazwa", "XnazwyList", "Nrdzeni", "metoda".
# b) Funkcja operując na zbiorze "dane" powinna tworzyć model regresji liniowej dla Ynazwa w odniesieniu do zmiennych XnazwyList.
#    W najprostszej postaci dla danych z "Zadanie 0" są to modele: ("Y~x1", "Y~x2", "Y~x3" etc.).
#    To jakiej postaci model powinien być zbudowany, definiowane jest przez parametr "XnazwyList", przyjmujący obiekt typu lista.
#    Lista ma tyle elementów, ile modeli będzie zbudowanych. Każdy element listy jest wektorem nazw zmiennych "x".
#    Przykład: "list(x1,c(x1,x5,x7))" buduje dwa modele 1) "Y~x1" oraz 2) "Y~x1+x5+x7".
# c) Funkcja powinna budować każdą kombinację modeli równolegle.
# d) W zależności od przekazanego argumentu do "metoda", funkcja wykorzystywać powinna albo równoleglą wersję "lapply",
#    albo równoleglą wersję pętli "for".
# e) Każda równoległa pętla powinna zwracać informacje o nazwach zmiennej/zmiennych (pierwsza kolumna tekstowa) 
#    i oszacowaniach parametrów (druga kolumna numeryczna).
# f) Funkja powinna zwracać wyniki w formie listy.
# g) Nazwa funkcji to "ModelParallel".
# 
# Zadanie 2:s
# a) Zasymulujmy środowisko pracy: Zakładamy, że komputer ma tylko 50MB RAM.
# b) Stwórz funkcję przyjmującą nastęujące parametry: "dane", "filtr".
# c) Funkcja powinna w pierwszym kroku sprawdzać rozmiar obiektu "dane". 
#    Jeżeli "dane" mają rozmiar większy niż dostępna pamięć RAM, obiekt powinien zostać w odpowiedni sposób przetworzony.
# d) Funkcja powinna następnie filtrować wiersze w obiekcie "dane" poprzez parametr "filtr", 
#    przyjmujący warunek w formie tekstowej o postaci: "Nazwa_zmiennej operator wartość (%,|) ...".
#    Przykład: "x5 >= 0.5" lub "x8 %in% c(0.3,0.6)" lub "x2 >= 0.5 & x3 < 0.1".
#    Podpowiedź: z racji tego, że "filtr" jest tekstem, należy przetowrzyć go na wyrażenie.
# e) Funkcja powinna zwracać wyfiltrowany obiekt "dane" oraz działać SZYBKO.
# f) Nazwa funkcji to "Filtr".
# 
# Zadanie 3:
# a) Przetestuj działanie opracowanych funkcji na tabeli z "Zadanie 0".


### OCENA - 25%

# install.packages("parallel")
# install.packages("biglm")
# install.packages("bigmemory")
# install.packages("pryr")
# install.packages("data.table")
# install.packages("doParallel")
# install.packages("doSNOW")

library(parallel)
library(biglm)
library(bigmemory)
library(pryr)
library(data.table)
library("doParallel")
library("doSNOW")
library(stringr)




### Zad. 0 ###

set.seed = (555)
macierz <- matrix(0, nrow = 1000, ncol = 1001)
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



### Zad. 1 ###

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




### Zad. 2 ###



Filtr <- function(dane, filtr) {
  
  rozmiar_RAM_MB <- 50                   # 50 MB RAM-u
  rozmiar_B <- 1048576
  RAM_B <- rozmiar_RAM_MB * rozmiar_B
  
  if (object_size(dane) > RAM_B) 
  {
    print("Za duza tablica! Uzycie 'big.matrix'...")
    small <- as.big.matrix(dane)
    
    
    kolumny <- str_extract_all(filtr, "[x][_][0-9]+")[[1]]
    fraza_filtru <- str_split(filtr, " ")[[1]]
    
    for(i in 1:length(kolumny))
    {
      #print(kolumny[i])
      dodatek <- paste0("small[,","\'", kolumny[i], "\']")
      #print(dodatek)
      fraza_filtru[which(fraza_filtru == kolumny[i])] <- dodatek
      #print(fraza_filtru)
    }
    #print(paste(fraza_filtru, collapse = " "))
    nowy_filtr <- paste(fraza_filtru, collapse = " ")
    #print(nowy_filtr)
    
    macierz_wyfiltrowana <- data.table(small[eval(parse(text = nowy_filtr)),])
    print(paste0("Macierz po filtrze zajmuje = ",object_size(macierz_wyfiltrowana), " B = ", object_size(macierz_wyfiltrowana)/1000, " KB = ", object_size(macierz_wyfiltrowana)/10^6, " MB" ))
    
    return(macierz_wyfiltrowana)
  }
  else
  {
    dane = data.table(dane)
    return(dane[eval(parse(text = filtr))])
  }
}




### Zad. 3 ###

# Zad. 1) 

nCores <- detectCores()
klaster <- makeCluster(nCores)
registerDoParallel(klaster)
Xnazwy <- paste("x_", seq(1:(ncol(macierz)-1)), sep = "")

lista_LM_for_small <- ModelParallel(macierz, 'Y', list('x_1', c('x_1', 'x_5', 'x_7'), 'x_2', 'x_3'), nCores, 'for')         # 'for' - dziala
lista_LM_for_all <- ModelParallel(macierz, 'Y', Xnazwy, nCores, 'for')

lista_LM_lapply_small <- ModelParallel(macierz, 'Y', list('x_1', c('x_1', 'x_5', 'x_7'), 'x_2', 'x_3'), nCores, 'lapply')      # 'lapply' - jeszcze nie dziala
lista_LM_lapply_all <- ModelParallel(macierz, 'Y', Xnazwy, nCores, 'lapply')



# Zad. 2) 

filtrowanie_1 <- Filtr(macierz, 'x_25 > 15.3 | x_2 < 9.2')
filtrowanie_2 <- Filtr(macierz, 'x_15 < 25 & x_250 < 10')

