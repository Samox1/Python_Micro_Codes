# Plik proszę nazwać numerem swojego indeksu.
# Plik powinien zawierać tylko definicję funkcji z Zadanie 1-3.
# 
# Zadanie 0:
# a) Stwórz macierz rozmiaru "1000x1001". 
#    Przypisz nazwę "Y" do pierwszej kolumny. Przypisz nazwy od "x_1" do "x_1000" do następnych kolumn.
# b) Wstaw losowe wartości z wektora od 1 do 100 w kolumnę "Y". set.seed = (555).
# c) Wstaw do kolumn od "x_1" do "x_1000" wartości zgodne z nastepujacym schematem 
#    "x_i = Y + wartość losowa z rozkładu normalnego". set.seed = (555).

set.seed(555)
m <- as.data.frame(matrix(0, 1000, 1001))
colnames(m)[1] <- "Y"
colnames(m)[2:ncol(m)] <- c(paste0("x_",1:(ncol(m)-1)))

set.seed(555)
m$Y <- sample(1:100, 1000, replace = TRUE) 

set.seed(555)
m[,2:ncol(m)] <- apply(m[,2:ncol(m)], 2, function(x) m$Y + rnorm(1000))




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


library(parallel)
library(foreach)

ModelParallel <- function(dane, Ynazwa, XnazwyList, Nrdzeni, metoda="lapply"){
  
  p <- lapply(XnazwyList, function(var) paste(var, collapse= "+"))
  
  if(metoda == "lapply")
  {
    z <- mclapply(p, function(x) lm(as.formula(paste0(Ynazwa,"~" ,x)), data = dane))
  } 
  else if(metoda == "for")
  {
    cl<-makeCluster(Nrdzeni) 
    registerDoSNOW(cl)
    z <- foreach(i=1:length(XnazwyList)) %dopar% 
    {
      lm(as.formula(paste0(Ynazwa,"~" ,p[[i]])), data = dane)
    } 
    
  }
  else
  {
    stop("Podaj wlasciwa nazwe metody (lapply lub for)")
  }
  
  df<- data.frame(var = unlist(p))
  df$coef <- lapply(z, function(x) x$coef)
  
  return(df)
}


#Test ModelParallel
ModelParallel(m, "Y", list("x_1", c("x_1", "x_2")),"lapply")
ModelParallel(m, "Y", list("x_1", c("x_1", "x_2")),"for")




# Zadanie 2:
# a) Stwórz funkcję "ModelOcena" przyjmującą nastęujące parametry: "y_tar" (rzeczywista), "y_hat" (prognoza).
# b) Funkcja w pierwszym kroku powinna rozpoznawać czy mamy do czynienia z problemem regresji czy klasyfikacji. 
#    y_tar: regresja -> numeric, klasyfikacja -> factor.
# c) W zależności od problemu funkcja szacować powinna różnego rodzaju błędy:
#    Regresja: MAE, MSE, MAPE.
#    Klasyfikacja: AUC (szacowanie metodą trapezów), macierz klasyfikacji (punkt odcięcia wyznaczany jest poprzez index Youdena), 
#                  Czułość, Specyficzność, Jakość.
# d) Dla czytelności kodu, wszystkie miary powinny być liczone w oparciu o zewnętrzne funkcje (dowolne nazwy), 
#    których definicje znajdować się powinny przed definicją funkcji "ModelOcena". 
# e) Funkja powinna zwracać wyniki w formie: 
#    Regresja: nazwany wektor (MAE, MSE, MAPE) o trzech elementach.
#    Klasyfikacja: nazwana lista (Mat, J, Miary) o trzech elementach:
#                  Mat = macierz klasyfikacji, w wierszach znajdują się wartości "y_tar" a w kolumnach wartości "y_hat",
#                        nazwy wierszy i kolumn muszą być zgodne z dostępnymi etykietami klas.
#                  J = wartość indexu Youdena,
#                  Miary = nazwany wektor o elementach AUC, Czułość, Specyficzność, Jakość.
# f) Funkcja będzie testowana tylko dla klasyfikacji binarnej i regresji.


library(pROC)

MSE <- function(y_tar, y_hat){
  return(mean(y_tar-y_hat)^2)
}

MAE <- function(y_tar, y_hat){
  return(mean(abs(y_tar - y_hat)))
}

MAPE <- function(y_tar, y_hat){
  return(mean(abs((y_tar-y_hat)/y_tar)) * 100)
}


AUC_wart<-function(y_tar, y_hat){
  y_hat2 <- as.numeric(y_hat)
  roc_obj<- roc(y_tar, y_hat2)
  a <-as.numeric(roc_obj$auc)
  return(a)
}

Conf_matrix <- function(y_tar, y_hat, odciecie = 1){
  
  if(odciecie != 1)
  {
    y_hat <- ifelse(y_hat>odciecie, 1,0)
  }
  y_tar <- factor(y_tar, levels=c(0,1))
  y_hat <- factor(y_hat, levels=c(0,1))
  return(t(table(y_hat, y_tar)))
}

Sensitivity <- function(table){
  return(table[1,1]/(table[1,1]+table[1,2]))
}

Specificity <- function(table){
  return(table[2,2]/(table[2,2]+table[2,1]))
}

Accuracy <- function(table){
  return((table[1,1]+table[1,2])/(table[1,1]+table[2,2]+table[1,2]+table[2,1]))
}

Youden_index  <- function(sensy,specy){
  return(sensy+specy-1)
}


ModelOcena <- function(y_tar, y_hat){
  
  if(is.numeric(y_tar)==TRUE)
  {
    MAE_model <- MAE(y_tar, y_hat)
    MSE_model <-  MSE(y_tar, y_hat)
    MAPE_model <- MAPE(y_tar, y_hat)
    wynik <- c(MAE_model, MSE_model, MAPE_model)
    names(wynik) <- c("MAE", "MSE", "MAPE")
    
    return(wynik)
  }
  else if(is.factor(y_tar)==TRUE)
  {

    auc_model <- AUC_wart(y_tar, y_hat)
    conf_matrix_model <- Conf_matrix(y_tar, y_hat)
    sensitivity_model <- Sensitivity(conf_matrix_model)
    specificity_model <- Specificity(conf_matrix_model)
    accuracy_model <- Accuracy(conf_matrix_model)
    youden_index_model <- Youden_index(sensitivity_model, specificity_model)
    
    Mat <- conf_matrix_model
    J <- youden_index_model
    Miary <- c(auc_model, sensitivity_model, specificity_model, accuracy_model)
    names(Miary) <- c("AUC", "CZULOSC", "SPECYFICZNOSC", "JAKOSC")
    
    wynik <- list(Mat, J, Miary)
    names(wynik)<- c("MAT", "J", "MIARY")
    
    return(wynik)
  }
  else
  {
    stop("Podaj wektor y_tar dla regresji lub klasyfikacji")
  }
}


#Test regresji
set.seed(346)
y_tar_reg <- rnorm(20)
y_hat_reg <- y_tar_reg + sample(x=c(0.5, 0.1))

ModelOcena(y_tar_reg,y_hat_reg)


#Test klasyfikacji
y_tar_klas <- as.factor(c(0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,0))
y_hat_klas <- as.factor(c(1,1,0,0,0,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1))

ModelOcena(y_tar_klas, y_hat_klas)




# Zadanie 3:
# a) Stwórz funkcję "CrossValidTune" przyjmującą nastęujące parametry: "dane", "kFold", "parTune", "seed". 
#    W skrócie: funkcja powinna krosswalidacyjnie tunować parametry danego algorytu. 
# b) Funkcja powinna w pierwszym kroku stworzyć listę przechowującą informację, które obserwacje posłużą jako zbiór treningowy,
#    a które wejdą do zbioru walidacyjnego. Ilość elementów listy zdefiniowana jest przez parametr "kFold" (liczba podzbiorów walidacyjnych).
#    Każdy element listy jest wektorem o tej samej długości, równej nrow("dane").
#    Każdy z wektorów zawiera albo liczbę 1 (obserwacja jest w zbiorze treningowym) albo 2 (obserwacja jest w zbiorze walidacyjnym). 
#    Przykład: list( c(1,2,1,1), c(2,1,1,2) ) - oznacza, że mamy doczynienia z 2-krotną walidacją na zbiorze z 4 obserwacjami, 
#    gdzie dla pierwszej iteracji tylko jeden element jest w podzbiorze walidacyjnym.
#    Losowanie rozpoczyna się od ustawienia ziarna na wartość "seed".
# c) W kolejnym kroku funkcja powinna stworzyć ramkę danych, w której przechowywane będą wyniki oraz kombinacje parametrów.
#    Liczba wierszy i kolumn zależy od zagadnienia (klasyfikacja, regresja) oraz od liczby tunowanych parametrów "parTune" i "kFold":
#    Przykład: "parTune" = data.frame( a = c(1,2), b = c(1,1) ) - oznacza, że algorytm ma 2 parametry do tunowania,
#              Dla "kFold" = 2 oraz "parTune", kombinacja parametrów to data.frame( "kFold" = c(1,2,1,2), a = c(1,2), b = c(1,1) ).
#              Kolejne kolumny tabeli wynikowej powinny stanowić miary uzyskane dzięki funkcji "ModelOcena".
#              Regresja: MAEt, MSEt, MAPEt, MAEw, MSEw, MAPEw - ozanczają miary dla zbioru treningowego i walidacyjnego.
#                        Finalnie tabele jest rozmiaru 4x9.
#              Klasyfikacja: AUCT, CzułośćT, SpecyficznośćT, JakośćT, AUCW, SpecyficznośćW, MAPEW, JakośćW - j.w.
#                            Finalnie tabele jest rozmiaru 4x11.
# d) W ostatnim kroku funkcja powinna budować w pętli model predykcyjny dla danej kombincaji parametrów i uzupełniać tabelę wynikową.      
#    Z racji tego, że nie stworzyliśmy na razie żadnego algorytmu ta część powinna działać następująco:
#    Każda pętla tworzy dwa podzbiory zdefiniowane przez wektor znajdujący się w liście z pkt b) dla danej kombinacji.
#    Do kolumn z miarami jakości wstawiane są wartości równe 0.
# e) Funkcja zwraca tabelę wynikową.



CrossValidTune <- function(dane, kFold, parTune, seed){
  set.seed(seed)
  k = nrow(dane)
  wektor = vector(mode = "numeric", length = k)
  lista = list()
  
  if(kFold == 1)
  {
    indxT <- sample( x = 1:k, size = 1, replace = F )
    wektor[indxT] <- 2
    wektor[-indxT] <- 1
    lista[[1]] <- wektor
  }
  else if(kFold > 1)
  {
    indxT <- sample( x = 1:k, size = 1, replace = F )
    wektor[indxT] <- 2
    wektor[-indxT] <- 1
    lista[[1]] <- wektor
    
    for (j in 2:kFold)
    {
      indxT <- sample( x = 1:k, size = round((1-1/kFold) * k), replace = F )
      s_indxT <- sort(indxT)
      
      wektor[s_indxT] <- 1
      wektor[-s_indxT] <- 2
      
      lista[[j]] <- wektor
    }
  }
  
  print("Lista podzialu zbiorow")
  print(lista)
  print("----------------------")
  
  
  ramka_par <- merge(data.frame(k_Fold=1:kFold), parTune, by=NULL)
  
  print("Kombinacja parametrow")
  print(ramka_par)  
  print("----------------------")
  
  
  y_tar <- dane[,1]

  if(is.numeric(y_tar)){
    
    f_reg <- ramka_par
    
    for (i in 1:nrow(ramka_par)) {
      # MODEL
      print(paste0("Ocena modelu (reg), parametry: ", i))
      print(ramka_par[i,])
      
      print("Podzial danych na trening (1) i walidacje (2)")
      print(lista[[ramka_par[i,"k_Fold"]]])
      
      Dane_trening <- dane[lista[[ramka_par[i,"k_Fold"]]] == 1,]
      print(Dane_trening)
      
      Dane_walidacja <- dane[lista[[ramka_par[i,"k_Fold"]]] == 2,]
      print(Dane_walidacja)
      
      # Ocena_reg_trening <- ModelOcena(y_tar, y_hat_trening)
      # Ocena_reg_walidacja <- ModelOcena(y_tar, y_hat_walidacja)
      
      f_reg[i,'MAE_t'] = 0
      f_reg[i,'MSE_t'] = 0
      f_reg[i,'MAPE_t'] = 0
      f_reg[i,'MAE_w'] = 0
      f_reg[i,'MSE_w'] = 0
      f_reg[i,'MAPE_w'] = 0
      
    }
    return(f_reg)
  
  }
  else if(is.factor(y_tar))
  {
    
    f_klas <- ramka_par
    
    for (i in 1:nrow(ramka_par)) {
      # MODEL
      print(paste0("Ocena modelu (klas), parametry: ", i))
      print(ramka_par[i,])
      
      print("Podzial danych na trening (1) i walidacje (2)")
      print(lista[[ramka_par[i,"k_Fold"]]])
      
      Dane_trening <- dane[lista[[ramka_par[i,"k_Fold"]]] == 1,]
      print(Dane_trening)
      
      Dane_walidacja <- dane[lista[[ramka_par[i,"k_Fold"]]] == 2,]
      print(Dane_walidacja)
      
      
      # Ocena_klas_trening <- ModelOcena(y_tar, y_hat_trening)
      # Ocena_klas_walidacja <- ModelOcena(y_tar, y_hat_walidacja)
      
      f_klas[i,'AUC_T'] = 0
      f_klas[i,'Czulosc_T'] = 0
      f_klas[i,'Specyficznosc_T'] = 0
      f_klas[i,'Jakosc_T'] = 0
      f_klas[i,'AUC_W'] = 0
      f_klas[i,'Czulosc_W'] = 0
      f_klas[i,'Specyficznosc_W'] = 0
      f_klas[i,'Jakosc_W'] = 0
    }
    return(f_klas)
    
  }
  
}


#Test CrossValidTune

paramTune <- expand.grid(a=c(1:2), b=1)     # w przykladzie => data.frame(a = c(1,2), b = c(1,1))

dane_reg <- data.frame(y_tar = c(1:10), X = c(3:12))
CrossValidTune(dane = dane_reg, kFold = 2, parTune = paramTune, 555)

dane_klas <- data.frame(y_tar = as.factor(c(0,0,0,0,0,1,1,1,1,1)), X = c(1:10))
CrossValidTune(dane = dane_klas, kFold = 2, parTune = paramTune, 555)

