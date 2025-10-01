#' Add or Update Dose Columns in Excel Data and Save, instead of doing it manually.
#'
#' This function reads an Excel file, calculates the equivalent dose and uncertainty in Gy using the provided dose rate and uncertainty, and updates or creates the columns: "De Gy", "De err Gy", "D0 Gy", "D0 err Gy". The updated Excel file is saved.
#'
#' @param file_path Path to the input Excel file, which contains the output on Analyst "Analyse all grains", which has been copied and pasted as is into a clean Excel file.
#' @param dose_rate Environmental dose rate in Gy/ka. Input as integer. I fyou have  multiple dose rates (different measurements on different readers) then add these as a c(x, x, x...) list.
#' @return A data frame with updated dose columns.
#' @export
#'
#' @examples
#' \dontrun{
#' updated_data <- add_missing_columns("data.xlsx", dose_rate = 2.5, dose_rate_err = 0.1)
#' }

add_DeGy_columns <- function(file_path, dose_rate) {
  # Load required packages
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required but not installed.")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required but not installed.")
  }

  # Read the Excel file
  data <- readxl::read_excel(file_path)

  # Check if "Filename" column exists
  if (!"Filename" %in% colnames(data)) {
    stop("The 'Filename' column is missing from the Excel file.")
  }

  # Extract source columns
  de_s_col <- data[["ED"]]
  de_err_s_col <- data[["ED_Err"]]
  d0_s_col <- data[["Param2"]]
  d0_err_s_col <- data[["Error2"]]

  # Check for missing source data
  if (any(is.null(de_s_col), is.null(de_err_s_col), is.null(d0_s_col), is.null(d0_err_s_col))) {
    stop("One or more required source columns (ED, ED_Err, Param2, Error2) are missing.")
  }

  # Initialize new columns
  data[["De Gy"]] <- NA
  data[["De err Gy"]] <- NA
  data[["D0 Gy"]] <- NA
  data[["D0 err Gy"]] <- NA

  # Iterate over each row in the data
  for (i in 1:nrow(data)) {
    filename <- data$Filename[i]
    # Find the matching dose rate and uncertainty for the filename
    matching_rate <- dose_rates[dose_rates[, 1] == filename, ]

    if (nrow(matching_rate) > 0) {
      dose_rate <- matching_rate[1, 2]
      dose_rate_err <- matching_rate[1, 3]

      # Calculate and update dose columns
      data$`De Gy`[i]      <- de_s_col[i] * dose_rate
      data$`De err Gy`[i]  <- data$`De Gy`[i] * sqrt((de_err_s_col[i] / de_s_col[i])^2 + (dose_rate_err / dose_rate)^2)
      data$`D0 Gy`[i]      <- d0_s_col[i] * dose_rate
      data$`D0 err Gy`[i]  <- data$`D0 Gy`[i] * sqrt((d0_err_s_col[i] / d0_s_col[i])^2 + (dose_rate_err / dose_rate)^2)
    }
  }



  # # Calculate and update dose columns
  # data[["De Gy"]]      <- de_s_col * dose_rate
  # data[["De err Gy"]]  <- data[["De Gy"]] * sqrt((de_err_s_col / de_s_col)^2 + (dose_rate_err / dose_rate)^2)
  # data[["D0 Gy"]]      <- d0_s_col * dose_rate
  # data[["D0 err Gy"]]  <- data[["D0 Gy"]] * sqrt((d0_err_s_col / d0_s_col)^2 + (dose_rate_err / dose_rate)^2)

  # Save the updated data back to the original Excel file
  writexl::write_xlsx(data, path = file_path)

  return(data)
}
