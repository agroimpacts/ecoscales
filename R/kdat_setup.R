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
  odat <- dat[, list("x" = get(x), "y" = get(y))]
  if(drop.oneoff == TRUE) {
    odat <- odat[get(oneoff.var) != 365 * 10000]  # remove one-time obs
  }
  odat <- na.omit(odat)   # drop NA values

  mu <- odat[, lapply(.SD, mean)][, lapply(.SD, log10)]  # mean
  med <- odat[, lapply(.SD, median)][, lapply(.SD, log10)]  # median
  o <- odat[, lapply(.SD, log10)]  # convert to log10

  o[, n := 1]  # set up count column
  o[x < xlim[1], x := xlim[1]]  # confine x to min
  o[x > xlim[2], x := xlim[2]]  # confine x to max
  o[y < ylim[1], y := ylim[1]]  # confine y to min
  o[y > ylim[2], y := ylim[2]]  # confine y to max
  coordinates(o) <- ~x + y  # to spdf
  list("dat" = o, "mu" = mu, "med" = med)
}
