## This script is an example script containing the functions I have made for analysing Single Grain OSL data from JonZac.
####################################################################
library(EasyOSL)
library(BayLumPlus)
library(rjags)
####################################################################
## 0. Set WD and file path
setwd("D:/Home/esellwood/OneDrive - Université de Rennes/Post Doc-PR048038/3. CPJB OSL/2. Analysis/Std_analysis_package/EasyOSL/EasyOSL/data/")

file <- "All_CPJB_L11.xlsx"

## 1. Update excel file. ##########################################
file_names <- c("521_20240806-CPJB-L6-16Pos1-44-De_determination.binx", "522_20241002-CPJB-L10-14-De_determination.binx")
file_dose_rates <- c(0.158, 0.166)
file_dr_err <- c(0.007, 0.007)

reader_dose_rates <- data.frame(file_names, file_dose_rates, file_dr_err)

## Convert De S to De Gy ###########################################
## add_DeGy_columns <- function(file_path, dose_rates)
osl_data_Gy <- add_DeGy_columns(file, reader_dose_rates)

## Filter grains ###################################################
## filter_OSL_grains <- function(data, filter_options = "all", d0filt_limit = NULL, plots = TRUE)
## Filter options: "all","positive" "feldsparfilt" "d0filt" "recuperation" "zerodoserem" "nanrem"
filt_data_gy <- filter_OSL_grains(osl_data_Gy, filter_options = "all")

## Summary statistics ##############################################
## grain_summary_stats <- function(datasets, samplename,  path_for_save = NULL)
stats <- grain_summary_stats(filt_data_gy, file,  path_for_save = "Test_folder")

## Save files for BayLum ###########################################
## save_discpos_files <- function(path, data, discpos_to_save = NULL, discpos_to_use = NULL, env_dose_rates = NULL, reader_dose_rates = NULL)
## dispos_to_use options: options: "all","positive" "feldsparfilt" "d0filt" "recuperation" "zerodoserem" "nanrem"
env_dose_rate = c(1.55, 0.0007)

save_discpos_files("Test_folder", filt_data_gy, discpos_to_use = "d0filt", reader_dose_rates, env_dose_rates)

## NOTE: You will need to save your original Binx files in each resepctive folder which is generated from the save_discpos_files function.

#####################################################################
## Palaeodose calculation
#####################################################################
Path=c("Test_folder/")


