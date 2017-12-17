#' Combined scale results
#' @name datf
#' @docType data
#' @description Cleaned and combined calibration and main scale datasets
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(datf)
NULL

#' Combined scale results resampled with uncertainty
#' @name bootperturb
#' @docType data
#' @description Results of combined calibration and main scale datasets, 
#' resampled 1000 times with random error introduced within bounds of 
#' uncertainty determined from analysis of calibration dataset
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(bootperturb)
NULL

#' Combined scale results
#' @name dimbreaks
#' @docType data
#' @description Contains multiple look-up tables for plotting and labelling 
#' figures in log10 spaces (aaxis1, aaxis2, taxis1, taxis2, alab1, alab2, tlab1, 
#' tlab2) 
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(datf)
NULL

#' Histograms of scale dimensions
#' @name hdatstat_bt
#' @docType data
#' @description A list containing mean, 2.5, and 97.5 percentile values 
#' resulting from histograms applied to bootperturb for each of the four primary 
#' dimensions (used in making Figure 1), divided by observation type.
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(hdatstat_bt)
NULL

#' Kernel density estimates for bivariate dimensions comparisons
#' @name kderla
#' @docType data
#' @description A list of kernel density rasters and various plotting 
#' parameters, calculated across all observations and by observational type, for
#' Figure 2.
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(kderla)
NULL

#' Kernel density estimates for bandwidth sensitivity estimation
#' @name kderls
#' @docType data
#' @description A list of kernel density rasters calculated for bandwidth 
#' sensitivity analysis
#' @author Estes, Treuer, Elsen, Chang, Choi, Ahmed, Caylor, Ellis
#' @keywords data
#' @examples
#' data(kderls)
NULL
