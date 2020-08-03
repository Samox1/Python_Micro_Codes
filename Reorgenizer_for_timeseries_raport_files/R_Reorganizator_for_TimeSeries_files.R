rm(list=ls())


library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
#library(lubridate)

### Plik Testowy = "D:/Programming/Python_Micro_Codes/Reorgenizer_for_timeseries_raport_files/Jaw_2019-01.txt"

Dane <- fread("D:/Programming/Python_Micro_Codes/Reorgenizer_for_timeseries_raport_files/Jaw_2019-01.txt", sep = ';', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)

### Sprawdzenie czy w danych pojawia sie puste miejsce lub NA
NA_pozycje <- which(is.na(Dane) == TRUE)

### Wydobycie unikalnych nazw Stacji i markerow: A+ | Rc- | Ri+
Stacje <- unique(Dane$PPE_NO)
Energia <- unique(Dane$ENERGIA)

# Print tylko jednej stacji
Dane[Dane$PPE_NO==Stacje[1],]


### Tablica wynika ma wygladac nastepujaco:
#
#        | Stacja_1_A+ | Stacja_1_Rc- | Stacja_1_Ri+ | Stacja_2_A+ | Stacja_2_Rc- | Stacja_2_Ri+ | ...
# Data_1 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
# Data_2 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
# Data_3 |   Wartosc   |   Wartosc    |   Wartosc    |   Wartosc   |   Wartosc    |   Wartosc    | ...
#



