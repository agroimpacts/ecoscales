#' Prepares scale data for kernel density estimates
#' @description Processes two variables from input scale data.table for KDE
#' @param dat Input data.table (an rbind of bootperturb)
#' @param x Name of variable in dat that will form x axis 
#' @param x Name of variable in dat that will form y axis 
#' @param xlim Two element vector setting limits for x axis
#' @param ylim Two element vector setting limits for y axis
#' @param drop.oneoff Should unrepeated observations be dropped (default=FALSE)
#' @param oneoff.var Axis containing unrepeated observations ("x" or "y")
#' @return SpatialPointsDataFrame
#' @examples 
#' odt <- kdat_setup(kdat, x = "plot_res", y = "t_btwn_samp", xlim = c(-5, 8), 
#'                   ylim = c(-5, 5.57), drop.oneoff = TRUE)  
#' @export
kdat_setup <- function(dat, x, y, xlim, ylim, drop.oneoff = FALSE, 
                       oneoff.var = "y") {
  odat <- dat[, list("x" = log10(get(x)), "y" = log10(get(y)))]
  if(drop.oneoff == TRUE) {
    odat <- odat[round(get(oneoff.var), 8) != 6.56229286]  # remove one-time obs
  }
  odat <- na.omit(odat)   # drop NA values
  odat[, n := 1]  # set up count column
  odat[x < xlim[1], x := xlim[1]]  # confine x to min
  odat[x > xlim[2], x := xlim[2]]  # confine x to max
  odat[y < ylim[1], y := ylim[1]]  # confine y to min
  odat[y > ylim[2], y := ylim[2]]  # confine y to max
  coordinates(odat) <- ~x + y  # to spdf
  odat
}
