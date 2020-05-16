rm(list=ls())

library(tidyverse)
library(XML)
library(methods)
library(xml2)

print("Hello")

data <- xmlParse("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml")
xml_data <- xmlToList(data)

kappa <- read_xml("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h--SH.xml",as_html = FALSE, options = "NOBLANKS")
