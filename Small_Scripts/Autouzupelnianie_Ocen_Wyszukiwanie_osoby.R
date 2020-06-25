### Skrypt do autouzupelniania oceny z "zejsciowki"
### 

rm(list=ls())

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)


# Dane <- fread("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/Wszystkie_Ocenki.xlsx", sep = ',', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)

Wszystkie_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/Wszystkie_Oceny_Poprawka.csv", sep = ';', header = TRUE, encoding = "UTF-8")
colnames(Wszystkie_Oceny) <- c("nr", "Nazwisko","Imię","Dzień tygodnia","1","30","20","Planck","Ciepło","3","11","10","Hall")

Poniedzialek_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ poniedziałek 11_ oceny.csv", sep = ',', header = TRUE, encoding = "UTF-8")
Sroda_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ środa 14_ oceny.csv", sep = ',', header = TRUE, encoding = "UTF-8")
Piatek_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ piątek 14_ oceny.csv", sep = ',', header = TRUE, encoding = "UTF-8")

Poniedzialek_Sprawozdania_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ_poniedziałek_11_Sprawozdania.csv", sep = ',', header = TRUE, encoding = "UTF-8")
Sroda_Sprawozdania_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ_środa_14_Sprawozdania.csv", sep = ',', header = TRUE, encoding = "UTF-8")
# Piatek_Sprawozdania_Oceny <- read.table("D:/Programming/Python_Micro_Codes/Small_Scripts/Ocenki/WIBHIŚ piątek 14_ oceny.csv", sep = ',', header = TRUE, encoding = "UTF-8")




Wszystkie_Oceny$Kombajn <- paste(Wszystkie_Oceny$Nazwisko, Wszystkie_Oceny$Imię, sep=" ")
Wszystkie_Oceny$Zejsciowka_11 <- 0
Wszystkie_Oceny$Sprawozdania_11 <- 0

Poniedzialek_Oceny$Kombajn <- paste(Poniedzialek_Oceny$Nazwisko, Poniedzialek_Oceny$X.U.FEFF.Imię, sep=" ")
Sroda_Oceny$Kombajn <- paste(Sroda_Oceny$Nazwisko, Sroda_Oceny$X.U.FEFF.Imię, sep=" ")
Piatek_Oceny$Kombajn <- paste(Piatek_Oceny$Nazwisko, Piatek_Oceny$X.U.FEFF.Imię, sep=" ")

Poniedzialek_Sprawozdania_Oceny$Kombajn <- paste(Poniedzialek_Sprawozdania_Oceny$Nazwisko, Poniedzialek_Sprawozdania_Oceny$X.U.FEFF.Imię, sep=" ")
Sroda_Sprawozdania_Oceny$Kombajn <- paste(Sroda_Sprawozdania_Oceny$Nazwisko, Sroda_Sprawozdania_Oceny$X.U.FEFF.Imię, sep=" ")
# Piatek_Sprawozdania_Oceny$Kombajn <- paste(Piatek_Sprawozdania_Oceny$Nazwisko, Piatek_Sprawozdania_Oceny$X.U.FEFF.Imię, sep=" ")


### --- Petla dla PONIEDZIALKU --- 

for (x in c(1:length(Poniedzialek_Oceny[,1]))) {
  # print(x)

  print( which(Poniedzialek_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
  Wszystkie_Oceny$Zejsciowka_11[which(Poniedzialek_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Poniedzialek_Oceny$Laboratorium.nr.11....Gamma....zejściówka[x]
  
}



### --- Petla dla SRODY --- 

for (x in c(1:length(Sroda_Oceny[,1]))) {
  # print(x)
  
  print( which(Sroda_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
  Wszystkie_Oceny$Zejsciowka_11[which(Sroda_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Sroda_Oceny$Laboratorium.nr.11....Gamma....zejściówka[x]
  
}



### --- Petla dla PIATKU --- 

for (x in c(1:length(Piatek_Oceny[,1]))) {
  # print(x)
  
  print( which(Piatek_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
  Wszystkie_Oceny$Zejsciowka_11[which(Piatek_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Piatek_Oceny$Laboratorium.nr.11....Gamma....zejściówka[x]
  
}


### --- Sprawdzenie ilosci ocen w poszczegolne dni == Wszystkie Oceny --- ###

print(length(which(Poniedzialek_Oceny$Laboratorium.nr.11....Gamma....zejściówka > 0)))
print(length(which(Sroda_Oceny$Laboratorium.nr.11....Gamma....zejściówka > 0)))
print(length(which(Piatek_Oceny$Laboratorium.nr.11....Gamma....zejściówka > 0)))

print(length(which(Wszystkie_Oceny$Zejsciowka_11 > 0)))


### - Przepisanie punktow za Sprawozdanie z komentarza - PONIEDZIALEK - ###

for (x in c(1:length(Poniedzialek_Sprawozdania_Oceny[,1]))) {
  # print(x)
  
  kappa <- str_split(Poniedzialek_Sprawozdania_Oceny$Opinia[x], " - ")
  Poniedzialek_Sprawozdania_Oceny$Punkty[x] <- as.double(kappa[[1]][1])
  
  print( which(Poniedzialek_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
  Wszystkie_Oceny$Sprawozdania_11[which(Poniedzialek_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Poniedzialek_Sprawozdania_Oceny$Punkty[x]
  
}


### - Przepisanie punktow za Sprawozdanie z komentarza - SRODA - ###

for (x in c(1:length(Sroda_Sprawozdania_Oceny[,1]))) {
  # print(x)
  
  kappa <- str_split(Sroda_Sprawozdania_Oceny$Opinia[x], " - ")
  Sroda_Sprawozdania_Oceny$Punkty[x] <- as.double(kappa[[1]][1])
  
  print( which(Sroda_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
  Wszystkie_Oceny$Sprawozdania_11[which(Sroda_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Sroda_Sprawozdania_Oceny$Punkty[x]
  
}



### - Przepisanie punktow za Sprawozdanie z komentarza - PIATEK - ###

# for (x in c(1:length(Piatek_Sprawozdania_Oceny[,1]))) {
#   # print(x)
#   
#   kappa <- str_split(Piatek_Sprawozdania_Oceny$Opinia[x], " - ")
#   Piatek_Sprawozdania_Oceny$Punkty[x] <- as.double(kappa[[1]][1])
#   
#   print( which(Piatek_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn) )
#   Wszystkie_Oceny$Sprawozdania_11[which(Piatek_Sprawozdania_Oceny$Kombajn[x]==Wszystkie_Oceny$Kombajn)] <- Piatek_Sprawozdania_Oceny$Punkty[x]
#   
# }



### - Male podsumowanie wynikow - ###

print(c("Ocena = 0.0 == ", length(which(Wszystkie_Oceny$Sprawozdania_11==0))))
print(c("Ocena = 2.0 == ", length(which(Wszystkie_Oceny$Sprawozdania_11==2))))
print(c("Ocena = 3.0 == ", length(which(Wszystkie_Oceny$Sprawozdania_11==3))))
print(c("Ocena = 3.5 == ", length(which(Wszystkie_Oceny$Sprawozdania_11==3.5))))
print(c("Ocena = 4.0 == ", length(which(Wszystkie_Oceny$Sprawozdania_11==4))))





### - Sprawdzenie czy sa takie same osoby - ###
# for (x in c(1:length(Wszystkie_Oceny[,1]))) {
#   
#   gdzie <- which(Wszystkie_Oceny$Kombajn[x] == Wszystkie_Oceny$Kombajn)
#   
#   if (length(gdzie)>1) {
#     print(c(x, Wszystkie_Oceny[x,]))
#   }
#   
# }





