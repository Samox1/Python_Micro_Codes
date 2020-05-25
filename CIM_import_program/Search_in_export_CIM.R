rm(list=ls())

library(tidyverse)
library(dplyr)
library(data.table)


Dane <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/export-cim.txt", sep = '\t', header = FALSE, blank.lines.skip = TRUE, fill = TRUE)
# Dane2 <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml", header = FALSE, blank.lines.skip = TRUE, fill = TRUE)

kappa <- sapply(Dane,nchar)
summary(kappa)
znaki <- Dane[which(kappa[,3]<3),3]
