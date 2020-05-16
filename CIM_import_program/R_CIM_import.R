rm(list=ls())

library(tidyverse)
library(XML)
library(methods)
library(xml2)

print("Hello")

data <- xmlParse("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml", useInternalNode=TRUE)
xml_data <- xmlToList(data)
kappa1 <- xmlToDataFrame(data)

kappa <- read_xml("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml",as_html = FALSE)

ObjName <- as.character(xml_data[[2]])
ObjectNames <- as.character(xml_attrs(xml_child(xml_child(kappa, 2), 2)))
print(ObjectNames)

