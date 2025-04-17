

#' Title
#' @param datasets A list containing OSL data, which must include columns labelled De Gy, De errGy, D0 Gy and D0 err Gy
#' @param path_for_save Directory for where you want to save the file.
#' @param plots TRUE or FALSE (default). If TRUE, standard plots from the calc_AverageDose function will be plotted.
#'
#' @importFrom utils install.packages write.table
#' @importFrom stats na.omit
#' @return cam_output. COntains table of CAM ages, Over dispersion
#' @export
#'
#' @examples
cam_ages <- function(datasets, path_for_save = NULL, plots = FALSE){

  if (!requireNamespace("Luminescence", quietly = TRUE)) {
    install.packages("Luminescence")
  }

  cam_output <- data.frame(matrix(ncol = 7, nrow = length(datasets)))
  colnames(cam_output) <- c("De", "De err", "OD", "OD_err", "Rel OD", "Rel OD err", "Lmax")
  rownames(cam_output) <- names(datasets)

  for(name in names(datasets)){
    dat_to_analyse <- datasets[[name]]
    print(name)

    all_vals <- data.frame(de = as.numeric(dat_to_analyse$De.Gy), de_err = as.numeric(dat_to_analyse$De.err.Gy))
    all_vals <- all_vals[!apply(all_vals, 1, function(x) any(x <= 0)), ]

    all_cam_log <- Luminescence::calc_AverageDose(all_vals , log = TRUE, na.rm = TRUE, plot = plots)
    cam_output[name,] <- (all_cam_log@data[["summary"]])
  }

  if(!is.null(path_for_save)) {
    write.table(cam_output, file = file.path(path_for_save,"CAM_Des_R.csv"), row.names = TRUE, sep = ";", col.names = TRUE, quote = FALSE)

  }
  return(cam_output)
}

