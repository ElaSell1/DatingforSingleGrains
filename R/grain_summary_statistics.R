

grain_summary_statistics <- function(datasets, path_for_save = NULL){
  library(diptest)
  library(e1071)

  summary_stats <- data.frame(matrix(ncol = length(datasets) + 1, nrow = 13))
  colnames(summary_stats) <- names(datasets)
  rownames(summary_stats) <- c("Count",	"Count 0 dose grains (<1 Gy)", "Count NaNs",	"Count Error NANs",	"Mean De","Mean De Std.Err",	"Skew",	"Variance",	"Mean recycling ratio", "RR Std.Err",	"Mean Recuperation", "Recup Std.Err", "Bimodal dip-test")

  for (name in names(datasets)){
    dat <- datasets[[name]]
    summary_stats[1, name] <- signif(nrow(dat), 2)
    summary_stats[2, name] <- signif(length(which(dat$De.Gy < 1)), 2)
    summary_stats[3, name] <- signif(sum(is.nan(as.matrix(dat$De.Gy))), 2)
    summary_stats[4, name] <- signif(sum(is.nan(as.matrix(dat$De.err.Gy))), 2)
    summary_stats[5, name] <- signif(mean(dat$De.Gy, na.rm = TRUE), 2)
    summary_stats[6, name] <- signif(sd(dat$De.Gy, na.rm = TRUE) / sqrt(sum(!is.na(dat$De.Gy))), 2)
    summary_stats[7, name] <- signif(skewness(dat$De.Gy, na.rm = TRUE), 2)
    summary_stats[8, name] <- signif(var(dat$De.Gy, na.rm = TRUE), 2)
    summary_stats[9, name] <- signif(mean(dat$RR1, na.rm = TRUE), 2)
    summary_stats[10, name] <- signif(sd(dat$RR1, na.rm = TRUE) / sqrt(sum(!is.na(dat$RR1))), 2)
    summary_stats[11, name] <- signif(mean(dat$Recup1, na.rm = TRUE), 2)
    summary_stats[12, name] <- signif(sd(dat$Recup1, na.rm = TRUE) / sqrt(sum(!is.na(dat$Recup1))), 2)
    summary_stats[13, name] <- signif(dip.test(na.omit(as.numeric(dat$De.Gy)))$statistic, 2)

  }
  print(summary_stats)


  if (!is.null(path_for_save)) {
    # Ensure the directory exists
    if (!dir.exists(path_for_save)) {
      dir.create(path_for_save, recursive = TRUE)
    }
    write.table(summary_stats, file = file.path(path_for_save, paste0(samplename, "_Summary_stats.csv")),
                row.names = TRUE, sep = ";", col.names = TRUE, quote = FALSE)
  }
  return(summary_stats)
}
