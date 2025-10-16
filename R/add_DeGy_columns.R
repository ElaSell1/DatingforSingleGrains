#' Add or Update Dose Columns in Excel Data and Save, instead of doing it manually.
#'
#' This function reads an Excel file, calculates the equivalent dose and uncertainty in Gy using the provided dose rate and uncertainty, and updates or creates the columns: "De Gy", "De err Gy", "D0 Gy", "D0 err Gy". The updated Excel file is saved.
#'
#' @param file_path Path to the input Excel file.
#' @param dose_rate Environmental dose rate in Gy/ka.
#' @param dose_rate_err Uncertainty in the dose rate in Gy/ka.
#'
#' @return A data frame with updated dose columns.
#' @export
#'
#' @examples
#' \dontrun{
#' updated_data <- add_missing_columns("data.xlsx", dose_rate = 2.5, dose_rate_err = 0.1)
#' }

# add_DeGy_columns <- function(file_path, dose_rate, dose_rate_err) {
#   # Load required packages
#   if (!requireNamespace("readxl", quietly = TRUE)) {
#     stop("Package 'readxl' is required but not installed.")
#   }
#   if (!requireNamespace("writexl", quietly = TRUE)) {
#     stop("Package 'writexl' is required but not installed.")
#   }
#
#   # Read the Excel file
#   data <- readxl::read_excel(file_path)
#
  # data$ED <- as.numeric(data$ED)
  # data$ED_Err <- as.numeric(data$ED_Err)
  # data$Param2 <- as.numeric(data$Param2)
  # data$Error2 <- as.numeric(data$Error2)
#
#   # Extract source columns
#   de_s_col <- data[["ED"]]
#   de_err_s_col <- data[["ED_Err"]]
#   d0_s_col <- data[["Param2"]]
#   d0_err_s_col <- data[["Error2"]]
#
#   # Check for missing source data
#   if (any(is.null(de_s_col), is.null(de_err_s_col), is.null(d0_s_col), is.null(d0_err_s_col))) {
#     stop("One or more required source columns (ED, ED_Err, Param2, Error2) are missing.")
#   }
#
#   # Calculate and update dose columns
#   data[["De Gy"]]      <- de_s_col * dose_rate
#   data[["De err Gy"]]  <- data[["De Gy"]] * sqrt((de_err_s_col / de_s_col)^2 + (dose_rate_err / dose_rate)^2)
#   data[["D0 Gy"]]      <- d0_s_col * dose_rate
#   data[["D0 err Gy"]]  <- data[["D0 Gy"]] * sqrt((d0_err_s_col / d0_s_col)^2 + (dose_rate_err / dose_rate)^2)
#
#   # Save the updated data back to the original Excel file
#   writexl::write_xlsx(data, path = file_path)
#
#   return(data)
# }


add_DeGy_columns <- function(file_path, dose_rates) {
  # Load required packages
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required but not installed.")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required but not installed.")
  }

  # Read the Excel file
  data <- readxl::read_excel(file_path)

  # Check if required columns exist
  required_columns <- c("Filename", "ED", "ED_Err", "Param2", "Error2")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing from the Excel file:", paste(missing_columns, collapse = ", ")))
  }

  # Force relevant columns to numeric
  data$ED <- as.numeric(data$ED)
  data$ED_Err <- as.numeric(data$ED_Err)
  data$Param2 <- as.numeric(data$Param2)
  data$Error2 <- as.numeric(data$Error2)

  # Extract source columns
  de_s_col <- data[["ED"]]
  de_err_s_col <- data[["ED_Err"]]
  d0_s_col <- data[["Param2"]]
  d0_err_s_col <- data[["Error2"]]

  # Initialize new columns
  data[["De Gy"]] <- NA
  data[["De err Gy"]] <- NA
  data[["D0 Gy"]] <- NA
  data[["D0 err Gy"]] <- NA

  # Iterate over each row in the data
  for (i in 1:nrow(data)) {
    filename <- as.character(data$Filename[i])
    # Find the matching dose rate and uncertainty for the filename
    matching_rate <- dose_rates[dose_rates$file_names == filename, ]
    if (nrow(matching_rate) > 0) {
      dose_rate <- as.numeric(matching_rate$file_dose_rates)
      dose_rate_err <- as.numeric(matching_rate$file_dr_err)

      # Calculate and update dose columns
      data$`De Gy`[i] <- de_s_col[i] * dose_rate
      data$`De err Gy`[i] <- data$`De Gy`[i] * sqrt((de_err_s_col[i] / de_s_col[i])^2 + (dose_rate_err / dose_rate)^2)
      data$`D0 Gy`[i] <- d0_s_col[i] * dose_rate
      data$`D0 err Gy`[i] <- data$`D0 Gy`[i] * sqrt((d0_err_s_col[i] / d0_s_col[i])^2 + (dose_rate_err / dose_rate)^2)
    } else {
      warning(paste("No matching dose rate found for filename:", filename))
    }
  }

  # Save the updated data back to the original Excel file
  writexl::write_xlsx(data, path = file_path)

  # Return the updated data
  return(data)
}
