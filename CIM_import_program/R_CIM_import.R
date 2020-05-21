rm(list=ls())

library(tidyverse)
library(dplyr)
library(methods)
library(XML)
library(xml2)

print("Hello")


Input <- read_lines("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h.xml", skip_empty_rows = FALSE)

### Dane wejsciowe sa zjebane - w niektorych obiektach sa powtorzone elementy z roznymi ID - trzeba zmienic nazwe elementu obiektu

PsrList_ALL <- which(grepl("PowerSystemResource.PsrLists", Input))
Equipment_EquipmentContainer_ALL <- which(grepl("Equipment.EquipmentContainer", Input))
DiscreteValue_Discrete_ALL <- which(grepl("DiscreteValue.Discrete", Input))


if(length(DiscreteValue_Discrete_ALL) == 1){
  Input[DiscreteValue_Discrete_ALL] <- str_replace_all(Input[DiscreteValue_Discrete_ALL],"DiscreteValue.Discrete", "DiscreteValue.Discrete2")
}
if(length(PsrList_ALL) == 1){
  Input[PsrList_ALL] <- str_replace_all(Input[PsrList_ALL],"PowerSystemResource.PsrLists", "PowerSystemResource.PsrLists2")
}
if(length(Equipment_EquipmentContainer_ALL) == 1){
  Input[Equipment_EquipmentContainer_ALL] <- str_replace_all(Input[Equipment_EquipmentContainer_ALL],"Equipment.EquipmentContainer", "Equipment.EquipmentContainer2")
}


skok <- 0
if(length(DiscreteValue_Discrete_ALL) > 1){
for(x in c(2:(length(DiscreteValue_Discrete_ALL)-1))){
  if((DiscreteValue_Discrete_ALL[x-1]+1) == DiscreteValue_Discrete_ALL[x]){
    if(((DiscreteValue_Discrete_ALL[x-1]+1) == DiscreteValue_Discrete_ALL[x]) & DiscreteValue_Discrete_ALL[x] == (DiscreteValue_Discrete_ALL[x+1]-1)){
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c("3 linia: ",Input[PsrList_ALL[x+2]]))
      Input[DiscreteValue_Discrete_ALL[x]] <- str_replace_all(Input[DiscreteValue_Discrete_ALL[x]],"DiscreteValue.Discrete", "DiscreteValue.Discrete2")
      Input[DiscreteValue_Discrete_ALL[x+1]] <- str_replace_all(Input[DiscreteValue_Discrete_ALL[x+1]],"DiscreteValue.Discrete", "DiscreteValue.Discrete3")
      # print(c(PsrList_ALL[x], "1 linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], "2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x+2], "3 linia: ",Input[PsrList_ALL[x+2]]))
      skok <- 1
    }else{
      if(skok == 1){
        skok <- 0
        next
      }
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      Input[DiscreteValue_Discrete_ALL[x]] <- str_replace_all(Input[DiscreteValue_Discrete_ALL[x]],"DiscreteValue.Discrete", "DiscreteValue.Discrete2")
      # print(c("2 mod linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x], " linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], " linia: ",Input[PsrList_ALL[x+1]]))
    }
  }
}
  if((DiscreteValue_Discrete_ALL[x]+1) == DiscreteValue_Discrete_ALL[x+1]){
    Input[DiscreteValue_Discrete_ALL[x+1]] <- str_replace_all(Input[DiscreteValue_Discrete_ALL[x+1]],"DiscreteValue.Discrete", "DiscreteValue.Discrete2")
  }
}
print("--- Zmiana DiscreteValue.Discrete ---")

skok <- 0
if(length(PsrList_ALL) > 1){
for(x in c(2:(length(PsrList_ALL)-1))){
  if((PsrList_ALL[x-1]+1) == PsrList_ALL[x]){
    if(((PsrList_ALL[x-1]+1) == PsrList_ALL[x]) & PsrList_ALL[x] == (PsrList_ALL[x+1]-1)){
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c("3 linia: ",Input[PsrList_ALL[x+2]]))
      Input[PsrList_ALL[x]] <- str_replace_all(Input[PsrList_ALL[x]],"PowerSystemResource.PsrLists", "PowerSystemResource.PsrLists2")
      Input[PsrList_ALL[x+1]] <- str_replace_all(Input[PsrList_ALL[x+1]],"PowerSystemResource.PsrLists", "PowerSystemResource.PsrLists3")
      # print(c(PsrList_ALL[x], "1 linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], "2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x+2], "3 linia: ",Input[PsrList_ALL[x+2]]))
      skok <- 1
    }else{
      if(skok == 1){
        skok <- 0
        next
      }
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      Input[PsrList_ALL[x]] <- str_replace_all(Input[PsrList_ALL[x]],"PowerSystemResource.PsrLists", "PowerSystemResource.PsrLists2")
      # print(c("2 mod linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x], " linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], " linia: ",Input[PsrList_ALL[x+1]]))
    }
  }
}
  if((PsrList_ALL[x]+1) == PsrList_ALL[x+1]){
    Input[PsrList_ALL[x+1]] <- str_replace_all(Input[PsrList_ALL[x+1]],"PowerSystemResource.PsrLists", "PowerSystemResource.PsrLists2")
  }
}

print("--- Zmiana PowerSystemResource.PsrLists ---")

skok <- 0
if(length(Equipment_EquipmentContainer_ALL) > 1){
for(x in c(2:(length(Equipment_EquipmentContainer_ALL)-1))){
  if((Equipment_EquipmentContainer_ALL[x-1]+1) == Equipment_EquipmentContainer_ALL[x]){
    if(((Equipment_EquipmentContainer_ALL[x-1]+1) == Equipment_EquipmentContainer_ALL[x]) & Equipment_EquipmentContainer_ALL[x] == (Equipment_EquipmentContainer_ALL[x+1]-1)){
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c("3 linia: ",Input[PsrList_ALL[x+2]]))
      Input[Equipment_EquipmentContainer_ALL[x]] <- str_replace_all(Input[Equipment_EquipmentContainer_ALL[x]],"Equipment.EquipmentContainer", "Equipment.EquipmentContainer2")
      Input[Equipment_EquipmentContainer_ALL[x+1]] <- str_replace_all(Input[Equipment_EquipmentContainer_ALL[x+1]],"Equipment.EquipmentContainer", "Equipment.EquipmentContainer3")
      # print(c(PsrList_ALL[x], "1 linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], "2 linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x+2], "3 linia: ",Input[PsrList_ALL[x+2]]))
      skok <- 1
    }else{
      if(skok == 1){
        skok <- 0
        next
      }
      # print(c("1 linia: ",Input[PsrList_ALL[x]]))
      # print(c("2 linia: ",Input[PsrList_ALL[x+1]]))
      Input[Equipment_EquipmentContainer_ALL[x]] <- str_replace_all(Input[Equipment_EquipmentContainer_ALL[x]],"Equipment.EquipmentContainer", "Equipment.EquipmentContainer2")
      # print(c("2 mod linia: ",Input[PsrList_ALL[x+1]]))
      # print(c(PsrList_ALL[x], " linia: ",Input[PsrList_ALL[x]]))
      # print(c(PsrList_ALL[x+1], " linia: ",Input[PsrList_ALL[x+1]]))
    }
  }
}
  if((Equipment_EquipmentContainer_ALL[x]+1) == Equipment_EquipmentContainer_ALL[x+1]){
    Input[Equipment_EquipmentContainer_ALL[x+1]] <- str_replace_all(Input[Equipment_EquipmentContainer_ALL[x+1]],"Equipment.EquipmentContainer", "Equipment.EquipmentContainer2")
  }
}
print("--- Zmiana Equipment.EquipmentContainer ---")


write.table(Input, "D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml", fileEncoding="UTF-8", row.names = FALSE, col.names = FALSE, quote = FALSE)


# write(Input, "D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml")
# rm(Input)
# rm(PsrList_ALL)
# rm(x)
# Input[189384]
# Input[189385]


### 1 - Wczytanie danych =
# 1) xmlToDataFrame = Tabela ze wszystkimi danymi
# 2) xmlToList = Lista z danymi i nazwami obiektow - pojawia sie BUG i jest problem z powtarzajacymi sie ID
# 3) read_xml = Lista z danymi ID 

# data <- xmlParse(Input, useInternalNode=TRUE)
data <- xmlParse("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml", useInternalNode=TRUE)
print("--- xmlParse ---")
# data <- xmlParse("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_HALF.xml", useInternalNode=TRUE)
# XML_Lista <- xmlToList(data)
XML_Tabela <- xmlToDataFrame(data)
print("--- xmlToDataFrame ---")
# XML_Lista_ID <- read_xml("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h.xml",as_html = FALSE)
XML_Lista_ID <- read_xml("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml", encoding="UTF-8",as_html = FALSE)
# XML_Lista_ID <- read_xml(Input, encoding="UTF-8")
print("--- read_xml ---")
XML_Listowa <- as_list(XML_Lista_ID)
print("--- as_list(XML_List_ID) ---")

XML_Tabela_Okrojona <- XML_Tabela[, -(1:6)]
print("--- XML_Tabela okrojona ---")



### 2 - Wyciagniecie nazw obiektow z = xmlToList()


### Znalezienie ID dla kazdego obiektu
# ObjectsID <- xml_attrs(xml_child(XML_Lista_ID, 1))
# 
# for (x in c(2:length(XML_Tabela[,1]))) {
#   ObjectsID <- c(ObjectsID, xml_attrs(xml_child(XML_Lista_ID, x)))
# }
# 
# XML_Tabela <- data.frame(ObjectsID, XML_Tabela)
# 
# print("--- Objects_ID ---")
# 
# # Znalezienie nazwy obiektu
# 
# ObjectsNames <- xml_name(xml_child(XML_Lista_ID, 1))
# 
# for (x in c(2:length(XML_Tabela[,1]))) {
#   ObjectsNames <- c(ObjectsNames, xml_name(xml_child(XML_Lista_ID, x)))
# }
# 
# XML_Tabela <- data.frame(ObjectsNames, XML_Tabela)
# 
# print("--- Objects_Names ---")


### Wpisanie w miejsca NA np. "-"
# XML_Tabela[is.na(XML_Tabela)] = "-"
# XML_Tabela[XML_Tabela == ""] <- "="


# kappa <- which(grepl("", XML_Tabela$DiscreteValue.Discrete))
# for (x in kappa) {
#   XML_Tabela$DiscreteValue.Discrete[x] <- xml_attrs(xml_child(xml_child(XML_Lista_ID, x), 2))
# }
  
# which(grepl("", XML_Tabela$Equipment.EquipmentContainer))

# kappa1 <- which(grepl("", XML_Tabela$Equipment.EquipmentContainer))
# for (x in kappa1) {
#   XML_Tabela$Equipment.EquipmentContainer[x] <- xml_attrs(xml_child(xml_child(XML_Lista_ID, x), 4))
# }

### Working BITCH --------------------------------------------------------------------------------------
# kappa1 <- which(grepl("", XML_Tabela[,8]))
# for (x in kappa1) {
#   numeros <- which(grepl(colnames(XML_Tabela[8]), names(XML_Listowa$RDF[[x]])))
#   XML_Tabela[x,8] <- xml_attrs(xml_child(xml_child(XML_Lista_ID, x), numeros))
# }
### ----------------------------------------------------------------------------------------------------
for (y in c(2:length(XML_Tabela_Okrojona))) {
  kappa1 <- which(grepl("^$", XML_Tabela_Okrojona[,y]))
  print(length(kappa1))
}

Kolumienki <- 0

for (y in c(2:length(XML_Tabela_Okrojona))) {
  kappa1 <- which(grepl("^$", XML_Tabela_Okrojona[,y]))
  Kolumienki <- c(Kolumienki, length(kappa1))
}

# Od 4 kolumny do konca - sprawdzic czy sa puste4 pola i poszukac atrybutow
print("Szukanie pustych miejsc - START")

for (y in c(2:length(XML_Tabela_Okrojona))) {
  kappa1 <- which(grepl("^$", XML_Tabela_Okrojona[,y]))
  if (length(kappa1) > 0){
    print(str_c("Kolumna: ", y, " / ", length(XML_Tabela_Okrojona), " = Pustych miejsc: ", length(kappa1)))
    kolumna <- colnames(XML_Tabela_Okrojona[y])
    for (x in kappa1) {
      print(x)
      XML_Tabela_Okrojona[x,y] <- xml_attrs(xml_child(xml_child(XML_Lista_ID, x), which(grepl(kolumna, names(XML_Listowa$RDF[[x]])))))  # <- dosyc wolne
    }
  }
}

print("--- Uzupe³nianie XML_Tablica - puste miejsca ---")



write_excel_csv(XML_Tabela_Okrojona, "D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_ALL_Output.csv")



### -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### TEST TEST TEST

# xml_name(xml_child(xml_child(XML_Lista_ID, 6), 4))

# xml_length(xml_child(XML_Lista_ID, 6))

# which(grepl("Equipment.EquipmentContainer", names(XML_Listowa$RDF[[47]]))) ### Working - pokazuje index danego elementu obiektu

# kook <- c(1:length(XML_Listowa$RDF))
# 
# for (x in c(1:length(XML_Listowa$RDF))) {
#   kol <- which(grepl("IdentifiedObject.name", names(XML_Listowa$RDF[[kook]])))
# }

# numeros <- which(grepl(colnames(XML_Tabela[8]), names(XML_Listowa$RDF[[47]]))) ### Working - pokazuje index danego elementu obiektu w zaleznosci od nazwy kolumny
# xml_attrs(xml_child(xml_child(XML_Lista_ID, 47), numeros))

# ile <- 0
# 
# for (x in c(1:length(XML_Listowa$RDF))) {
#   if(length(XML_Listowa[["RDF"]][[x]][["IdentifiedObject.name"]][[1]])){
#     if(grepl("Pos", XML_Listowa[["RDF"]][[x]][["IdentifiedObject.name"]][[1]])){
#       print(c(x, XML_Listowa[["RDF"]][[x]][["IdentifiedObject.name"]][[1]], length(XML_Listowa[["RDF"]][[x]])))
#       ile <- c(ile, length(XML_Listowa[["RDF"]][[x]]))
#     }
#   }
# }

# ile <- 0
# xxx <- 0
# nazwy <- ""
# 
# for (x in c(1:length(XML_Listowa$RDF))) {
# # for (x in c(1:20)) {
#   if(names(XML_Listowa[["RDF"]][x]) == "DiscreteValue"){
#     ile <- c(ile, length(XML_Listowa[["RDF"]][[x]]))
#     # nazwy <- rbind(nazwy, names(XML_Listowa[["RDF"]][[x]]))
#     xxx <- c(xxx, x)
#   }
# }
# holy <- cbind(xxx, ile)
# 
# names(XML_Listowa[["RDF"]][3])
# which(holy[,2] == 4)
# names(XML_Listowa[["RDF"]][[47892]])
# XML_Listowa[["RDF"]][[47892]]
# 
# 
# # for (x in c(1:length(XML_Listowa$RDF))) {
# for (x in c(1:20)) {
#   print(names(XML_Listowa[["RDF"]][[47691]]))
#   
# }
# 
# powtorzenie <- NULL
# tekst <- NULL
# podzial <- NULL
# lepa <- NULL
# 
# for (x in c(1:length(XML_Listowa$RDF))) {
# # for (x in c(47500:47900)) {
#   # print(names(XML_Listowa[["RDF"]][[x]]))
#   podzial <- str_split(names(XML_Listowa[["RDF"]][[x]]), " ")
#   if(length(podzial)>0){
#     lepa <- duplicated(podzial)
#     # print(c(x, (XML_Listowa[["RDF"]][x])))
#     
#     for (k in c(1:length(lepa))) {
#       if(lepa[k]){
#         # print(c("KURWA", x, k))
#         powtorzenie <- rbind(powtorzenie, c(x,k))
#         tekst <- rbind(tekst, podzial[k])
#       }
#     } 
#   }
# }
# 
# 
# tekst_dataframe <- as.data.frame(tekst)
# kup <- as.factor(tekst)


# print("--- Zapis do Excel_CSV ---")
# peke <- function(x, kolumna, XML_Listowa){
#   return(as.character(xml_attrs(xml_child(xml_child(XML_Lista_ID, x), which(grepl(kolumna, names(XML_Listowa$RDF[[x]])))))))
# }
# peke(x, kolumna, XML_Lista_ID ,XML_Listowa)





