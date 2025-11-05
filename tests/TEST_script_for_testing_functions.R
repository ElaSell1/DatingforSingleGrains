## This script is an example script containing the functions I have made for analysing Single Grain OSL data from JonZac.
####################################################################
library(EasyOSL)
library(BayLumPlus)
library(rjags)
####################################################################
## 0. Set WD and file path

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

###############################################################
## Set up sample names and folders
###############################################################
# # State folder name/beginning of your .BINX file for all samples
sample_data <-  c("CPJB_L7/521_20240806", "CPJB_L7/521_20241003",
                  "CPJB_L8/521_20240806", "CPJB_L8/521_20241003",
                  "CPJB_L9/521_20240806", "CPJB_L9/521_20241003",
                  "CPJB_L10/521_20240806", "CPJB_L10/522_20241002",
                  "CPJB_L11/521_20240806", "CPJB_L11/522_20241002",
                  "CPJB_L12/521_20240806", "CPJB_L12/522_20241002",
                  "CPJB_L13/521_20240806", "CPJB_L13/522_20241002",
                  "CPJB_L14/521_20240806", "CPJB_L14/522_20241002",
                  "CPJB_L15/521_20240806", "CPJB_L15/521_20241016",
                  "CPJB_L16/521_20240806", "CPJB_L16/521_20241016",
                  "CPJB_L17/521_20240725", "CPJB_L17/521_20241016",
                  "CPJB_L18/521_20240725", "CPJB_L18/521_20241016",
                  "CPJB_L19/521_20240725", "CPJB_L19/522_20241018",
                  "CPJB_L20/521_20240725", "CPJB_L20/522_20241018",
                  "CPJB_L21/521_20240725", "CPJB_L21/522_20241018")

# # List sample names
Names=c("CPJB_L7","CPJB_L8","CPJB_L9","CPJB_L10","CPJB_L11","CPJB_L12","CPJB_L13","CPJB_L14","CPJB_L15","CPJB_L16","CPJB_L17","CPJB_L18","CPJB_L19", "CPJB_L20", "CPJB_L21")

Nb_sample=length(Names)

###############################################################
## Load .BINX data
###############################################################
DATA=Generate_DataFile(
  Path=Path,
  FolderNames=sample_data,
  Nb_sample=Nb_sample,
  Nb_binfile = c(2*Nb_sample),   # CHANGE HERE to number of binx files
  BinPerSample = c(rep(2,times = Nb_sample)),   # CHANGE HERE to list of number of .BINX files per sample
  sepDP = c(";"),
  sepDE = c(","),
  sepDS = c(","),
  sepR = c("="),
  verbose = TRUE)

save(DATA, file = "BINX_data_for_BayLum.RData")
# load(file = "BINX_data_for_BayLum.RData")

###############################################################
## Set up prior limits for each sample
###############################################################
# # Make list of (min, max) prioir age estimates for each sample
priorage <- unlist(c(rep(list(c(30, 140)), times = Nb_sample)))

# priorage <- unlist(c(rep(list(c(30, 140)), times = 1),
#                        rep(list(c(30, 110)), times = 1)))
str(DATA)

# # Build a matrix of stratigraphoic constraints
SC = SC_Ordered(Nb_sample=Nb_sample)

###############################################################
## Calculate palaeodoses
###############################################################
de_est = Palaeodose_Computation(DATA, SampleNames=Names,
                                Nb_sample=Nb_sample,
                                BinPerSample = rep(2,Nb_sample),
                                SavePdf = TRUE, SaveEstimates = TRUE,
                                LIN_fit = TRUE,
                                Origin_fit = TRUE,
                                distribution = c("lognormal_A"),
                                Iter = 8000,
                                t = 5,
                                n.chains = 3)

save(de_est, file = "Palaeodose_estimates.RData")

###############################################################
## Age calculation
###############################################################

dose_rate <- rep(1.550, 2)

## Matrix of shared uncertainties #############################
# # Load a CSV file containing a matrix of shared uncertainties.
Theta = paste(Path,"CPJB_L7-21_cov_matrix_GGGammaMeas.csv",sep="")

errorMatrix=read.csv(Theta,sep=",")
Theta=as.matrix(errorMatrix)

Theta <- Theta[1:2,1:2]

# # Select the doses and not the SD from the output of Palaeodose_Computation()
de_dat <- de_est[["Doses"]][1:Nb_sample, ]

###############################################################
## Without stratigraphic constraints
###############################################################
D0Independant <- Compute_AgeS_D(list(D = de_dat[["Dose"]],
                                     sD = de_dat[["sdDose"]],
                                     ddot = dose_rate),
                                Nb_sample = length(Names),
                                SampleNames = de_dat[["SampleNames"]],
                                ThetaMatrix = Theta,
                                prior = "Independance",
                                PriorAge = priorage,
                                Iter = 8000, burnin = 5000, t = 10)

save(D0Independant,  file = "Ages_No_Stratigraphy.RData")

# load("2Ages_No_Stratigraphy.RData")
###############################################################
## With stratigraphic constraints
###############################################################
D0Nicholls = Compute_AgeS_D(list(D = de_dat[["Dose"]],
                                 sD = de_dat[["sdDose"]],
                                 ddot = dose_rate),
                            Nb_sample = length(Names),
                            SampleNames = de_dat[["SampleNames"]],
                            ThetaMatrix = Theta,
                            prior = "StrictNicholls",
                            PriorAge = priorage,
                            Iter = 8000, burnin = 5000, t = 10)

save(D0Nicholls,  file = "Ages_With_Stratigraphy.RData")

###############################################################
## Plotting
###############################################################
Age_Constraints = plot_Ages(D0Nicholls) # plot_mode = "density")
Age_noconstraints = plot_Ages(D0Independant)

###############################################################
## Isotonic Regression modelling, No Stratigraphic  constraints
###############################################################
Isotonic = IsotonicCurve(c(), D0Independant,interactive = FALSE)
knitr::kable(Isotonic$Ages )

AgeWith_IR <-  plot_Ages(object=Isotonic, legend="topright")#  plot_mode="density")
AgesWith_IR_Plot = PlotIsotonicCurve(object=Isotonic, level=.68) # or .95

AgesWith_IR_Plot$DAG
AgesWith_IR_Plot$ribbon

ages_matrix <- as.data.frame(AgesWith_IR_Plot$Iso)

save(Isotonic, file = "IsotonicRegression_results.RDAT")

