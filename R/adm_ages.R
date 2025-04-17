adm_ages <- function(datasets, sigma,  path_for_save = NULL, plots = FALSE){
  library(Luminescence)
  adm_output <- data.frame(matrix(ncol = 11, nrow = length(datasets)))
  colnames(adm_output) <- c("AVG De", "AVG De STDerr", "OD", "OD_err", "AVG De level", "AVG De lower quant", "AVG De upper quant", "OD level", "OD lower quant", "OD upper quant", "Max likelihood")
  rownames(adm_output) <- names(datasets)

  for(name in names(datasets)){
    dat_to_analyse <- datasets[[name]]
    print(name)

    all_vals <- data.frame(de = as.numeric(dat_to_analyse$De.Gy), de_err = as.numeric(dat_to_analyse$De.err.Gy))
    all_vals <- all_vals[!apply(all_vals, 1, function(x) any(x <= 0)), ]

    adm <- calc_AverageDose(na.omit(all_vals), sigma_m = sigma, plot = plots)
    adm_output[name,] <- (adm@data[["summary"]])
  }

  if(!is.null(path_for_save)) {
    write.table(adm_output, file = file.path(path_for_save,"ADM_Des_R.csv"), row.names = TRUE, sep = ";", col.names = TRUE, quote = FALSE)

  }
  return(adm_output)
}
