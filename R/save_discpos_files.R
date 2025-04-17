

save_discpos_files <- function(path, data, discpos_to_save = NULL, discpos_to_use = NULL, env_dose_rates = NULL, reader_dose_rates = NULL) {
  library(writexl)

  # If discpos_to_save is NULL or empty, use the names of the datasets
  if (is.null(discpos_to_save) || length(discpos_to_save) == 0 || discpos_to_save == "all" ){
    discpos_to_save <- names(data)
  }

  if (!all(discpos_to_save %in% names(data))) {
    stop("Error: One or more values in discpos_to_save are not found in the data variable.")
  }

  for (dat_type in discpos_to_save) {
    dataset <- data[[dat_type]]
    file_names <- unique(dataset$Filename)

    for (i in 1:length(file_names)) {
      dir_name <- file.path(path, substr(file_names[i], start = 1, stop = 12))

      if (!dir.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
      }

      sub_set_by_file <- dataset[dataset$Filename == file_names[i], ]

      dat_fordisc_pos <- data.frame(position = sub_set_by_file$`Disc#`, grain = sub_set_by_file$`Grain#`, stringsAsFactors = FALSE)
      write.table(dat_fordisc_pos, file = file.path(dir_name, paste0(dat_type, "_DiscPos.csv")), row.names = FALSE, sep = ";", col.names = TRUE, quote = FALSE)

      # Save env_dose_rates data in the same subfolder
      if (!is.null(env_dose_rates) && nrow(env_dose_rates) > 0) {
        for (j in 1:nrow(env_dose_rates)) {
          row_data <- env_dose_rates[j, , drop = FALSE]
          file_name <- file.path(dir_name, paste0("DoseEnv.csv"))
          write.table(row_data, file = file_name, row.names = FALSE, col.names = c('obs', 'var'))
        }
      }


      if (!is.null(reader_dose_rates) && ncol(reader_dose_rates) > 0) {

        for (j in 1:ncol(reader_dose_rates)) {
          col_dr_data <- t(reader_dose_rates[,j , drop = FALSE])
          file_name <- file.path(dir_name, paste0("Dosesource", j, ".csv"))
          write.table(col_dr_data, file = file_name, row.names = FALSE, col.names = c('obs', 'var'))
        }
      }
    }
  }

  if (!is.null(discpos_to_use) && length(discpos_to_use) > 0) {
    dataset <- data[[discpos_to_use]]
    file_names <- unique(dataset$Filename)

    for (i in 1:length(file_names)) {
      dir_name <- file.path(path, substr(file_names[i], start = 1, stop = 12))

      if (!dir.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
      }

      sub_set_by_file <- dataset[dataset$Filename == file_names[i], ]

      dat_fordisc_pos <- data.frame(position = sub_set_by_file$`Disc#`, grain = sub_set_by_file$`Grain#`, stringsAsFactors = FALSE)
      write.table(dat_fordisc_pos, file = file.path(dir_name, "DiscPos.csv"), row.names = FALSE, sep = ";", col.names = TRUE, quote = FALSE)


    }
  }
}

