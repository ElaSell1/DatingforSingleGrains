#' Options to save CSV files consisting of 2 columns Disc;Pos
#' Ready for use with BayLum.
#'
#' @param path Directory path for where you whant to load your data from or save your data.
#' @param data List of OSL data.
#' @param discpos_to_save Optional. List of strings of which DiscPos files you want to save. Each data set in the list will be saved if no data set is specified.
#' @param discpos_to_use Optiona. State here which data set in particular you want to start using, and this DiscPos file will be labelled as DiscPos.csv and will be directly usable for BayLum.
#' @param env_dose_rates Optional.
#' @param reader_dose_rates Optional.
#'
#' @return
#' @export
#'
#' @examples
#'
save_discpos_files <- function(path, data, discpos_to_save = NULL, discpos_to_use = NULL, env_dose_rates = NULL, reader_dose_rates = NULL) {

  # If discpos_to_save is NULL or empty, use the names of the datasets
  if (is.null(discpos_to_save) || discpos_to_save == "all" ){
    discpos_to_save <- names(data)
    print("set to all")
  }
  print("set to all")
  if (!all(discpos_to_save %in% names(data))) {
    stop("Error: One or more values in discpos_to_save are not found. Please check the names of your data sets.")
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

