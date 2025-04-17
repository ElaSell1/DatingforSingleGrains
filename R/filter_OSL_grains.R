## filter_osl_grains
# This script takes your loaded excel file, and filters grains based on different criteria
# Criteria include:
#   "positive" WHich only removes negative grains
#   "feldsparfilt" WHich removes negative De avlues, and then only keeps grains with IRSL depletion ratio 0.9<>1
#   "d0filt" after filtering for feldspars, this removes grains with D0 values less than the mean De (Default), or less than the value stated in d0filt
#   "recuperation" after filtering for feldspars,Removes grains with recuperation values < 0.1
#    "zerodoserem" after filtering for feldspars, removes grains with doses < 1 Gy
#    "nanrem" after filtering for feldspars, it removes  grains with De as NAN
#
#Options for plotting De histograms and density plots, as well as De Gy vs. De err Gy, and De Gy vs. D0 Gy. Default = TRUE
##

filter_OSL_grains <- function(data, list_filter_options = "all", d0filt_limit = NULL, plots = TRUE) {

  # list_filter_options = "positive", "feldsparfilt", "d0filt","recuperation", "zerodoserem", "nanrem" Or use "all" to receive all filtered data sets.

  if("all" %in% list_filter_options){
    list_filter_options <- list("positive", "feldsparfilt", "d0filt","recuperation", "zerodoserem", "nanrem")
  }

  filtered_datasets <- list ()
  positive_data <- data[!(data$De.Gy < 0), ]
  feldspar_filt <- positive_data[which(positive_data$RR3 >= 0.9 & positive_data$RR3 <= 1.1), ]

  filtered_datasets$original_dataset <- data


  if ("positive" %in% list_filter_options) {
    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(as.numeric(positive_data$De.Gy), breaks = 100, main = "Positive De's \n Histogram")
      plot(density(na.omit(positive_data$De.Gy)), col="red", main = "Density function")
      plot(positive_data$De.Gy, positive_data$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(positive_data$De.Gy, positive_data$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$positive <- positive_data
  }

  if("feldsparfilt" %in% list_filter_options){
    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(as.numeric(feldspar_filt$De.Gy), breaks = 100, main = "Feldspars removed \n Histogram")
      plot(density(na.omit(feldspar_filt$De.Gy)), col="red", main = "Density function")
      plot(feldspar_filt$De.Gy, feldspar_filt$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(feldspar_filt$De.Gy, feldspar_filt$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$feldsparfilt <- feldspar_filt
  }

  if("d0filt" %in% list_filter_options) {

    limit_de_all <- ifelse(is.null(d0filt_limit), mean(positive_data$De.Gy, na.rm = TRUE), d0filt_limit)
    tnfilt_d0 <- feldspar_filt[which(feldspar_filt$D0.Gy >= limit_de_all), ]

    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(as.numeric(tnfilt_d0$De.Gy), breaks = 100, main = "D0 filtered \n Histogram")
      plot(density(na.omit(tnfilt_d0$De.Gy)), col="red", main = "Density function")
      plot(tnfilt_d0$De.Gy, tnfilt_d0$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(tnfilt_d0$De.Gy, tnfilt_d0$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$d0filt <- tnfilt_d0

  }

  if("recuperation" %in% list_filter_options){
    tnfilt_recup <- feldspar_filt[which(feldspar_filt$Recup1 <= 0.1), ]
    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(as.numeric(tnfilt_recup$De.Gy), breaks = 100, main = "Recuperation filtered \n Histogram")
      plot(density(na.omit(tnfilt_recup$De.Gy)), col="red", main = "Density function")
      plot(tnfilt_recup$De.Gy, tnfilt_recup$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(tnfilt_recup$De.Gy, tnfilt_recup$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$recuperation <- (tnfilt_recup)
  }

  if("zerodoserem" %in% list_filter_options){
    tnfilt_fld_0dose <- feldspar_filt[which(feldspar_filt$De.Gy >= 1), ]
    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(tnfilt_fld_0dose$De.Gy, breaks = 100, main = "Zero dose grains removed \n Histogram")
      plot(density(na.omit(tnfilt_fld_0dose$De.Gy)), col="red", main = "Density function")
      plot(tnfilt_fld_0dose$De.Gy, tnfilt_fld_0dose$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(tnfilt_fld_0dose$De.Gy, tnfilt_fld_0dose$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$zerodoserem <- tnfilt_fld_0dose
  }

  if("nanrem" %in% list_filter_options){
    tnfilt_fld_nan <- feldspar_filt[which(feldspar_filt$De.err.Gy >= 0), ]
    if (plots == TRUE) {
      par(mfrow=c(2,2))
      hist(tnfilt_fld_nan$De.Gy, breaks = 100, main = "NAN grains removed \n Histogram")
      plot(density(na.omit(tnfilt_fld_nan$De.Gy)), col="red", main = "Density function")
      plot(tnfilt_fld_nan$De.Gy, tnfilt_fld_nan$De.err.Gy, main="De.Gy vs De.err.Gy", xlab="De.Gy", ylab="De.err.Gy", col="blue")
      plot(tnfilt_fld_nan$De.Gy, tnfilt_fld_nan$D0.Gy, main="De.Gy vs D0.Gy", xlab="De.Gy", ylab="D0.Gy", col="green")
    }
    filtered_datasets$nanrem <- tnfilt_fld_nan
  }

  return(filtered_datasets)
}
