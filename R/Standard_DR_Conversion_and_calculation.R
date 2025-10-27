################################################################################
## Convert radio nuclide concentrations from TKA file (Raw Gamma Spectrometer data)
## into dose rates.
## Also calculates cosmic contribution
## Saves results in new sheet in same excel file.
##
## Inputs:  Excel file with concentrations in ppm or percentage
##          Grains size (um)
##          Water content (%)
##          Choice of conversion factors.
##                    Choices: "Cresswelletal2018","AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013"
##
## Outputs: Dose rates (Gy/ka) saved in the original excel file, in two new workbook sheet: "DR_Gy_ka", and "Cosmic_DR"
##
## Short cut keys
## Ctrl + shift + c = Remove code you don't want to run, or re-initialise code to run it again.
## Highlight code, then click Ctrl + enter = Highlighted code will run
################################################################################

library(Luminescence)
library(readxl)
library(openxlsx)
rm(list=ls())

dr_meas<- convert_Concentration2DoseRate()
################################################################################
## Things to change:
################################################################################

## Change your working directory and file name:
path <- "D:/Home/esellwood/Documents/4. Dose rate measurements/CPJB Dose rate analysis/"
file_name <- "RenDal Gammaspec analysis CPJB L20.xlsx"
file <- paste0(path,file_name)

raw_data <- read_excel(file, sheet = "Sample details")

## Information for the dose rate calculation
dr_meas$Mineral <- "Q"         # %, CHANGE HERE!! "Q" for quartz. "FS" for feldspar
dr_meas$GrainSize <- 215       # %,  CHANGE HERE!! Grain size in µm
dr_meas$WaterContent <- 15     # %, CHANGE HERE!! Water content in percent
dr_meas$WaterContent_SE <- 5   # %, CHANGE HERE!! Std Error on water content, in percent
ConversionFactors = "Cresswelletal2018"     ## CHANGE HERE!!

## Cosmic DR. Input location information.
cosmic_dose_rate <- calc_CosmicDoseRate(
  latitude = 45.43767,     ## latitude in decimal degrees
  longitude =  -0.419207,  ## longitude in decimal degrees
  altitude = 34,      # in meters
  depth = 1.5,        # assumed sample depth in meters
  density = 2.6,      # typical sediment density in g/cm³
  error = 10         # percent uncertainty on cosmic dose rate
)

print(cosmic_dose_rate$summary)

################################################################################
## PLEASE DON'T CHANGE ANYTHING FROM HERE. JUST RUN THE REST OF THE CODE
## AND LET THE MAGIC HAPPEN
################################################################################

dr_meas$K <- as.numeric(raw_data[1,4])
dr_meas$K_SE <- as.numeric(raw_data[1,7])
dr_meas$U <- as.numeric(raw_data[2,4])
dr_meas$U_SE <- as.numeric(raw_data[2,7])
dr_meas$Th <- as.numeric(raw_data[4,4])
dr_meas$Th_SE <- as.numeric(raw_data[4,7])

## Convert DR
dr_gy <- convert_Concentration2DoseRate(dr_meas, conversion = ConversionFactors)
print(dr_gy$InfDRG)

################################################################################
# # Save results in new sheet in same excel file
wb <- loadWorkbook(file)

# Check if the sheet 'DR_Gy_ka' exists, if not, add it
if (!"DR_Gy_ka" %in% names(wb)) {
  addWorksheet(wb, "DR_Gy_ka")
}

if (!"Cosmic_DR" %in% names(wb)) {
  addWorksheet(wb, "Cosmic_DR")
}

writeData(wb, "DR_Gy_ka", dr_gy@data[["InfDRG"]], rowNames = TRUE)
writeData(wb, "Cosmic_DR", cosmic_dose_rate$summary, rowNames = TRUE)

saveWorkbook(wb, file, overwrite = TRUE)

rm(wb)
