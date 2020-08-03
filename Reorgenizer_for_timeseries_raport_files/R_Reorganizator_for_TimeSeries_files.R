rm(list=ls())

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(easycsv)
library(lubridate)

### Plik Testowy = "D:/Programming/Python_Micro_Codes/Reorgenizer_for_timeseries_raport_files/Jaw_2019-01.txt"

# Dane <- fread("D:/Programming/Python_Micro_Codes/Reorgenizer_for_timeseries_raport_files/Jaw_2019-01.txt", sep = ';', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)

fread_folder(directory = "D:/Programming/Python_Micro_Codes/Reorgenizer_for_timeseries_raport_files", extension = "TXT", sep = ';', header = TRUE, blank.lines.skip = TRUE, fill = TRUE, showProgress = interactive())
Data_Framki <- ls()

Dane <- NULL

for (framki in Data_Framki) {
  Dane <- rbind(Dane, get(framki))
}

### Sprawdzenie czy w danych pojawia sie puste miejsce lub NA
NA_pozycje <- which(is.na(Dane) == TRUE, arr.ind = TRUE)
if(length(NA_pozycje) == 0){
  print("NA Check = 0")
}else{
  print(c("Find NA values = ", length(NA_pozycje[,1])))
  print(c("NA values: /n", NA_pozycje))
}

### Sortowanie Danych przy pomocy daty
Dane$GODZ <- ymd_hms(Dane$GODZ)
Zmiana_Godziny <- which(Dane$FLAGA_EXTR==1)
if(length(Zmiana_Godziny) != 0){
  Dane$GODZ[Zmiana_Godziny] <- Dane$GODZ[Zmiana_Godziny] + minutes(60)
}

Dane <- arrange(Dane, GODZ)

# Znalezc i dosztukowac dane 
# for (godzinki in Dane$GODZ) {
#   for (stacyjki in Stacje) {
#     for (markery in Energia) {
#       
#     }
#   }
# }


### Wydobycie unikalnych nazw Stacji i markerow: A+ | Rc- | Ri+
Stacje <- unique(Dane$PPE_NO)
Energia <- unique(Dane$ENERGIA)
Daty <- unique(Dane$GODZ)
Sekwencja_Czasu <- seq(Daty[1],Daty[length(Daty)], by = '15 mins')

if(length(Daty) != length(Sekwencja_Czasu)){
  print("Different Sequentions of Dates!!!")
  Sekwencja_Czasu[!(Sekwencja_Czasu %in% Daty)]
}else{
  print("Time Series is Okey - Countinue")
}

# Print tylko jednej stacji
print(Dane[Dane$PPE_NO==Stacje[2],])


### Tablica wynika ma wygladac nastepujaco:
#
#  Daty  | Stacja_1_A+ | Stacja_1_Rc- | Stacja_1_Ri+ | Stacja_2_A+ | Stacja_2_Rc- | Stacja_2_Ri+ | ...
# Data_1 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
# Data_2 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
# Data_3 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
#


### Tworzenie Header'a, czyli nazwy stacji + markery

Header <- "Daty"

for (stacje_nazwy in Stacje) {
  for (mininazwy in Energia) {
   Header <- c(Header, paste(stacje_nazwy,mininazwy, sep = "_"))
   
  }
}

print(Header[-1])


### Zrobienie tablicy wynikowej

# Wynik <- data.frame(Daty)

# Dane[which(Dane$ENERGIA[which(Dane$PPE_NO==Stacje[1])] == Energia[1]),]

# Wynik <- data.frame()

# Dane[which(Dane$PPE_NO==Stacje[1])]
# Test = Dane[which(Dane[which(Dane$PPE_NO==Stacje[1])] == Energia[1])]
# print(Test)

Wynik <- data.frame(matrix(NA,length(Daty),length(Header)))
colnames(Wynik) <- Header
Wynik[,1] <- Daty

for (stacje_nazwy in Stacje) {
  for (mininazwy in Energia) {
    
    po_stacjach <- Dane[which(Dane$PPE_NO==stacje_nazwy)]
    po_markerach <- po_stacjach[which(po_stacjach$ENERGIA==mininazwy)]
    Kolumna <- po_markerach$WARTOSC
    
    # Kolumna <- Dane$WARTOSC[which(Dane$ENERGIA[which(Dane$PPE_NO==stacje_nazwy)] == mininazwy)]
    
    print(paste0(stacje_nazwy, " -> ", mininazwy, " = ", length(Kolumna)))
    # Wynik <- cbind(Wynik, Kolumna)
    
  }
}

# for (godzinki in Dane$GODZ) {
#   for (stacyjki in c(1:length(Stacje))) {
#     for (markery in c(1:length(Energia))) {
#       
#       
#       
#       # print(Dane$WARTOSC[which(Dane$PPE_NO==Stacje[stacyjki] & Dane$GODZ==godzinki & Dane$ENERGIA==Energia[markery])])
#       # WARTOSC <- Dane$WARTOSC[which(Dane$PPE_NO==Stacje[stacyjki] & Dane$GODZ==godzinki & Dane$ENERGIA==Energia[markery])]
#       Wynik[godzinki, (stacyjki*markery+1)] <- WARTOSC
#         
#     }
#   }
# }

licznik <- length(Daty)
licznik <- as.integer(licznik / 10)
indeks <- 10

### Problem z 28713 - Zmiana godziny, podwojne fragmenty czasowe 3:15:00 z flaga i bez flagi

for (godzinki in c(28700:length(Daty))) {
  for (stacyjki in c(1:length(Stacje))) {
    for (markery in c(1:length(Energia))) {
      
      Po_Godzinie <- Dane[Dane$GODZ==Daty[godzinki],]
      # print(Po_Godzinie)
      Po_Stacjach <- Po_Godzinie[which(Po_Godzinie$PPE_NO==Stacje[stacyjki]),]
      # print(Po_Stacjach)
      Po_Markerach <- Po_Stacjach[which(Po_Stacjach$ENERGIA==Energia[markery]),]
      # print(Po_Markerach)
      WARTOSC <- Po_Markerach$WARTOSC
      print(Po_Markerach)
      print(godzinki)
      
      # print(Dane$WARTOSC[which(Dane$PPE_NO==Stacje[stacyjki] & Dane$GODZ==godzinki & Dane$ENERGIA==Energia[markery])])
      # WARTOSC <- Dane$WARTOSC[which(Dane$PPE_NO==Stacje[stacyjki] & Dane$GODZ==as_date(godzinki) & Dane$ENERGIA==Energia[markery])]
      
      if(length(WARTOSC) == 0){
        WARTOSC = NA
      }
      Wynik[godzinki, paste0(Stacje[stacyjki], "_", Energia[markery])] <- WARTOSC
      
    }
  }
  
  if(godzinki%%licznik == 0){
    print(paste0("Progress = ", indeks))
    indeks = indeks + 10
  }
    
}

# colnames(Wynik) <- Header
View(Wynik)

print(which(is.na(Wynik), arr.ind = TRUE))




# View(Dane[which(Dane$PPE_NO==Stacje[1])])
# print(Dane[which(Dane$PPE_NO==Stacje[2])])
# print(Dane[which(Dane$PPE_NO==Stacje[3])])
# length(Dane$ENERGIA)

