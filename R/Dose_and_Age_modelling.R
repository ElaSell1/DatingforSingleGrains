## This script loads in either .BINX files with the Generate_DataFile() function, or RDAT files containing the organised data.
## Palaeodoses are calculated, before Ages are calculated, where stratigraphy can be included.
###############################################################
library(BayLumPlus)
library(rjags)
library(ggplot2)

rm(list=ls())
###############################################################
## Set working directory and path for saving
###############################################################

# # Path to the sample file
Path=c("/")

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
