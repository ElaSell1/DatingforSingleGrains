## This script is an example script containing the functions I have made for analysing Single Grain OSL data from JonZac.
##################################
library(EasyOSL)
##################################
## 0. Set WD and file path
setwd("D:/Home/esellwood/OneDrive - Université de Rennes/Post Doc-PR048038/3. CPJB OSL/3. Files for R package test")

file <- "All_CPJB_L11.xlsx"

## 1. Update excel file.
file_names <- c("521_20240806-CPJB-L6-16Pos1-44-De_determination.binx", "522_20241002-CPJB-L10-14-De_determination.binx")
file_dose_rates <- c(0.158, 0.166)
file_dr_err <- c(0.007, 0.007)

dose_rates <- data.frame(file_names, file_dose_rates, file_dr_err)

add_DeGy_columns(file, dose_rates)
