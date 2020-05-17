rm(list=ls())

library(tidyverse)
library(XML)
library(methods)
library(xml2)

print("Hello")

### 1 - Wczytanie danych =
# 1) xmlToDataFrame = Tabela ze wszystkimi danymi
# 2) xmlToList = Lista z danymi i nazwami obiektow - pojawia sie BUG i jest problem z powtarzajacymi sie ID
# 3) read_xml = Lista z danymi ID 

data <- xmlParse("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml", useInternalNode=TRUE)
XML_Lista <- xmlToList(data)
XML_Tabela <- data.frame(xmlToDataFrame(data))
XML_Lista_ID <- read_xml("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml",as_html = FALSE)

XML_Tabela <- XML_Tabela[, -(1:6)]


### 2 - Wyciagniecie nazw obiektow z = xmlToList()


### Znalezienie ID dla kazdego obiektu
ObjectsID <- xml_attrs(xml_child(XML_Lista_ID, 1))

for (x in c(2:length(XML_Tabela[,1]))) {
  ObjectsID <- c(ObjectsID, xml_attrs(xml_child(XML_Lista_ID, x)))
}

XML_Tabela <- data.frame(ObjectsID, XML_Tabela)

# Znalezienie nazwy obiektu

ObjectsNames <- names(XML_Lista[1])

for (x in c(2:length(XML_Tabela[,1]))) {
  ObjectsNames <- c(ObjectsNames, names(XML_Lista[x]))
}

XML_Tabela <- data.frame(ObjectsNames, XML_Tabela)


### Wpisanie w miejsca NA np. "-"
# XML_Tabela[is.na(XML_Tabela)] = "-"
# XML_Tabela[XML_Tabela == ""] <- "="


kappa <- which(grepl("", XML_Tabela$DiscreteValue.Discrete))
for (x in kappa) {
  XML_Tabela$DiscreteValue.Discrete[x] <- xml_attrs(xml_child(xml_child(XML_Lista_ID, x), 2))
}
  
# which(grepl("", XML_Tabela$Equipment.EquipmentContainer))
kappa1 <- which(grepl("", XML_Tabela$Equipment.EquipmentContainer))
for (x in kappa1) {
  XML_Tabela$Equipment.EquipmentContainer[x] <- x
  print(xml_attrs(xml_child(xml_child(XML_Lista_ID, x), 4))[["resource"]])
}




