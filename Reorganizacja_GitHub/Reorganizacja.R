### Reorganizacja GITHUB-a Doktoranckiego
### Szymon Baczyñski
### Start - 10.07.2020

### ********************************************************************************************
### BRO 
### --> Robisz kopie z kazdego dnia w folderze "D:/Programming/DOKTORAT/KOPIA_DANE_Z_LABU" --> 
### --> Wyrzucasz Historie z glownego folderu "A_Dane_z_LABORATORIUM" --> 
### --> Wlaczasz skrypt i masz HISTORIE
### BRO
### ********************************************************************************************


rm(list=ls())

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)



files <- list.files(path = "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM", recursive = T)
nazwy <- basename(list.files(path = "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM", recursive = T))

# print(as.array(files))
# print(as.array(nazwy))

daty <- NULL

for (x in files) {
  kappa <- str_split(x, " - ")[[1]][1]
  
  daty <- c(daty, kappa)
  
}

# print(as.array(daty))

Nazwy_z_datami <- paste(daty, nazwy, sep = " - ")
# print(as.array(Nazwy_z_datami))



### --- Test Test --- ###

# rm(T1)
# T1 <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T3")])


### --- Grupowanie plikow --- na odpowiednie probki --- ###

v5F0PS <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "v5-K-0"),str_which(Nazwy_z_datami, "v5K0"), str_which(Nazwy_z_datami, "v5F0"))])
v5F0PS_id <- c(str_which(Nazwy_z_datami, "v5-K-0"),str_which(Nazwy_z_datami, "v5K0"), str_which(Nazwy_z_datami, "v5F0"))

v5F1PP <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "v5-1"), str_which(Nazwy_z_datami, "v5F1"))])
v5F1PP_id <- c(str_which(Nazwy_z_datami, "v5-1"), str_which(Nazwy_z_datami, "v5F1"))
v5F2PP <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "v5-2"),str_which(Nazwy_z_datami, "v5F2"))])
v5F2PP_id <- c(str_which(Nazwy_z_datami, "v5-2"),str_which(Nazwy_z_datami, "v5F2"))
v5F3PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "v5F3")])
v5F3PS_id <- str_which(Nazwy_z_datami, "v5F3")
v5F4PP <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "v5F4")])
v5F4PP_id <- str_which(Nazwy_z_datami, "v5F4")
v5F5PP <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "v5F5")]) 
v5F5PP_id <- str_which(Nazwy_z_datami, "v5F5")
v5F6PP <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "v5F6")]) 
v5F6PP_id <- str_which(Nazwy_z_datami, "v5F6") 

v8D1PP <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "v8"), str_which(Nazwy_z_datami, "D1"))])
v8D1PP_id <- c(str_which(Nazwy_z_datami, "v8"), str_which(Nazwy_z_datami, "D1"))

v5K_T1_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T1")])
v5K_T1_PS_id <- str_which(Nazwy_z_datami, "T1")
v5K_T2_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T2")])
v5K_T2_PS_id <- str_which(Nazwy_z_datami, "T2")
v5F_T3_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T3")])
v5F_T3_PS_id <- str_which(Nazwy_z_datami, "T3")
v5F_T4_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T4")])
v5F_T4_PS_id <- str_which(Nazwy_z_datami, "T4")

v5K_T7x_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "T7x")])
v5K_T7x_PS_id <- str_which(Nazwy_z_datami, "T7x")

KLC_v1_F1_PP <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "KLCv1F1"), str_which(Nazwy_z_datami, "KLCv1F-"))])
KLC_v1_F1_PP_id <- c(str_which(Nazwy_z_datami, "KLCv1F1"), str_which(Nazwy_z_datami, "KLCv1F-"))
KLC_v1_F2_PP <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "KLCv1F2")])
KLC_v1_F2_PP_id <- str_which(Nazwy_z_datami, "KLCv1F2")
KLC_v1_F3_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "KLCv1F3")])
KLC_v1_F3_PS_id <- str_which(Nazwy_z_datami, "KLCv1F3")
KLC_v1_F4_PP <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "KLC_v1_F4")])
KLC_v1_F4_PP_id <- str_which(Nazwy_z_datami, "KLC_v1_F4")
KLC_v1_F5_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "KLC_v1_F5")])
KLC_v1_F5_PS_id <- str_which(Nazwy_z_datami, "KLC_v1_F5")
KLC_v1_F6_PS <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "KLC_v1_F6")])
KLC_v1_F6_PS_id <- str_which(Nazwy_z_datami, "KLC_v1_F6")

FW1 <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "FW1"), str_which(Nazwy_z_datami, "v5WFv1"))])
FW1_id <- c(str_which(Nazwy_z_datami, "FW1"), str_which(Nazwy_z_datami, "v5WFv1"))


LC_PDMS_zPlazma <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "LC_PDMS_zPlazma")])
LC_PDMS_zPlazma_id <- str_which(Nazwy_z_datami, "LC_PDMS_zPlazma")
LC_PDMS_bezPlazmy <- data.frame(Nazwy_z_datami[c(str_which(Nazwy_z_datami, "LC_PDMS_bezPlazmy"), str_which(Nazwy_z_datami, "LC_PDMS_bezPlazma"))])
LC_PDMS_bezPlazmy_id <- c(str_which(Nazwy_z_datami, "LC_PDMS_bezPlazmy"), str_which(Nazwy_z_datami, "LC_PDMS_bezPlazma"))

LC_Cell_1 <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "LC-Cell-1")])
LC_Cell_1_id <- str_which(Nazwy_z_datami, "LC-Cell-1")


LC_GLASS_Test_1 <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "LC_GLASS")])
LC_GLASS_Test_1_id <- str_which(Nazwy_z_datami, "LC_GLASS")

LC_PDMS_Test_1 <- data.frame(Nazwy_z_datami[str_which(Nazwy_z_datami, "LC_PDMS")])
LC_PDMS_Test_1_id <- str_which(Nazwy_z_datami, "LC_PDMS")


Suma_plikow <- length(v5F0PS[,1]) + length(v5F1PP[,1]) + length(v5F2PP[,1]) + length(v5F3PS[,1]) + length(v5F4PP[,1]) + length(v5F5PP[,1]) + length(v5F6PP[,1]) + length(v8D1PP[,1]) + 
  length(v5K_T1_PS[,1]) + length(v5K_T2_PS[,1]) + length(v5F_T3_PS[,1]) + length(v5F_T4_PS[,1]) + length(v5K_T7x_PS[,1]) + length(KLC_v1_F1_PP[,1]) + length(KLC_v1_F2_PP[,1]) +
  length(KLC_v1_F3_PS[,1]) + length(KLC_v1_F4_PP[,1]) + length(KLC_v1_F5_PS[,1]) + length(KLC_v1_F6_PS[,1]) + length(FW1[,1]) + length(LC_Cell_1[,1]) + length(LC_PDMS_zPlazma[,1]) + length(LC_PDMS_bezPlazmy[,1])



### --- Kopiowanie plikow v5F0PS do odpowiedniego folderu --- ###
### Folder dla v5F0PS = "D:\Programming\DOKTORAT\A_Dane_z_LABORATORIUM\0_A_Historia-Probek\v5K0PS-Pierwsza_probka_z_wrzesnia_2019"

# v5F0PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5K0PS-Pierwsza_probka_z_wrzesnia_2019"
# 
# pelna_sciezka <- list.files(path = "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM", full.names = TRUE , recursive = T)
# file.copy(pelna_sciezka[c(str_which(Nazwy_z_datami, "v5-K-0"),str_which(Nazwy_z_datami, "v5K0"), str_which(Nazwy_z_datami, "v5F0"))], to = paste0(v5F0PS_Folder), overwrite = FALSE, copy.date = TRUE)
# 
# oldNames <- basename(list.files(v5F0PS_Folder, full.names = TRUE))
# newNames <- Nazwy_z_datami[c(str_which(Nazwy_z_datami, "v5-K-0"),str_which(Nazwy_z_datami, "v5K0"), str_which(Nazwy_z_datami, "v5F0"))]
# 
# 
# file.rename(from = file.path(v5F0PS_Folder, oldNames), to = file.path(v5F0PS_Folder, newNames))

### --- RENAME wszystkich plikow - w folderze = "KOPIA_DANE_Z_LABU"
kopia_folder <- "D:/Programming/DOKTORAT/KOPIA_DANE_Z_LABU"
all_files <- list.files(path = "D:/Programming/DOKTORAT/KOPIA_DANE_Z_LABU", recursive = T, full.names = TRUE)
file.rename(from = file.path(kopia_folder, basename(all_files)), to = file.path(kopia_folder, Nazwy_z_datami))

for (x in c(1:length(all_files))) {
  folder <- str_remove(all_files[x], basename(all_files[x]))  
  file.rename(from = file.path(folder, basename(all_files[x])), to = file.path(folder, Nazwy_z_datami[x]))
}

pelna_sciezka <- list.files(path = kopia_folder, full.names = TRUE , recursive = T)

### --- Kopiowanie plikow v5F0PS do odpowiedniego folderu --- ###
### Folder dla v5F0PS = "D:\Programming\DOKTORAT\A_Dane_z_LABORATORIUM\0_A_Historia-Probek\v5K0PS-Pierwsza_probka_z_wrzesnia_2019"

dir.create("D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek")

dir.create(v5F0PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5K0PS-Pierwsza_probka_z_wrzesnia_2019")
file.copy(pelna_sciezka[v5F0PS_id], to = v5F0PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F1PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F1PP")
file.copy(pelna_sciezka[v5F1PP_id], to = v5F1PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F2PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F2PP")
file.copy(pelna_sciezka[v5F2PP_id], to = v5F2PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F3PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F3PS")
file.copy(pelna_sciezka[v5F3PS_id], to = v5F3PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F4PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F4PP")
file.copy(pelna_sciezka[v5F4PP_id], to = v5F4PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F5PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F5PP")
file.copy(pelna_sciezka[v5F5PP_id], to = v5F5PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F6PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F6PP")
file.copy(pelna_sciezka[v5F6PP_id], to = v5F6PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v8D1PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v8D1PP")
file.copy(pelna_sciezka[v8D1PP_id], to = v8D1PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5K_T1_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5K_T1_PS")
file.copy(pelna_sciezka[v5K_T1_PS_id], to = v5K_T1_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5K_T2_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5K_T2_PS")
file.copy(pelna_sciezka[v5K_T2_PS_id], to = v5K_T2_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F_T3_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F_T3_PS")
file.copy(pelna_sciezka[v5F_T3_PS_id], to = v5F_T3_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5F_T4_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5F_T4_PS")
file.copy(pelna_sciezka[v5F_T4_PS_id], to = v5F_T4_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(v5K_T7x_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/v5K_T7x_PS")
file.copy(pelna_sciezka[v5K_T7x_PS_id], to = v5K_T7x_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F1_PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F1_PP")
file.copy(pelna_sciezka[KLC_v1_F1_PP_id], to = KLC_v1_F1_PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F2_PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F2_PP")
file.copy(pelna_sciezka[KLC_v1_F2_PP_id], to = KLC_v1_F2_PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F3_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F3_PS")
file.copy(pelna_sciezka[KLC_v1_F3_PS_id], to = KLC_v1_F3_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F4_PP_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F4_PP")
file.copy(pelna_sciezka[KLC_v1_F4_PP_id], to = KLC_v1_F4_PP_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F5_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F5_PS")
file.copy(pelna_sciezka[KLC_v1_F5_PS_id], to = KLC_v1_F5_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(KLC_v1_F6_PS_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/KLC_v1_F6_PS")
file.copy(pelna_sciezka[KLC_v1_F6_PS_id], to = KLC_v1_F6_PS_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(FW1_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/FW1")
file.copy(pelna_sciezka[FW1_id], to = FW1_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(LC_PDMS_zPlazma_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/LC_PDMS_zPlazma")
file.copy(pelna_sciezka[LC_PDMS_zPlazma_id], to = LC_PDMS_zPlazma_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(LC_PDMS_bezPlazmy_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/LC_PDMS_bezPlazmy")
file.copy(pelna_sciezka[LC_PDMS_bezPlazmy_id], to = LC_PDMS_bezPlazmy_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(LC_Cell_1_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/LC_Cell_1")
file.copy(pelna_sciezka[LC_Cell_1_id], to = LC_Cell_1_Folder, overwrite = FALSE, copy.date = TRUE)


dir.create(LC_GLASS_Test_1_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/LC_aGLASS_Test_1")
file.copy(pelna_sciezka[LC_GLASS_Test_1_id], to = LC_GLASS_Test_1_Folder, overwrite = FALSE, copy.date = TRUE)

dir.create(LC_PDMS_Test_1_Folder <- "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek/LC_aPDMS_Test_1")
file.copy(pelna_sciezka[LC_PDMS_Test_1_id], to = LC_PDMS_Test_1_Folder, overwrite = FALSE, copy.date = TRUE)




Wszystkie_pliki_wedlug_probki <- list.files(path = "D:/Programming/DOKTORAT/A_Dane_z_LABORATORIUM/0_A_Historia-Probek", full.names = TRUE , recursive = T)


### --- Test - jakie pliki nie zostaly uwzglednione --- ###

pominiete_pliki <- pelna_sciezka

for (x in basename(Wszystkie_pliki_wedlug_probki)) {
  pominiete_pliki[str_which(basename(pelna_sciezka), x)] <- " "
}

print(basename(pominiete_pliki))







