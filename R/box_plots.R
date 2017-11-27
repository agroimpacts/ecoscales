#' Calculates boxplot statistics
#' @description Calculates the usual boxplot statistics plus mean
#' @param x Vector of values from which to calculate statistics
#' @param whiskers A two-element vector of the percentiles for the lower and upper whiskers (c(0.025, 0.975))
#' @param weighted TRUE or FALSE (default) for weighted quantiles and mean
#' @param weight.opts A list of options to be passed to wtd.quantile
#' @param na.rm Should the quantile and mean functions exclude NA values in x? Default is TRUE, else FALSE
#' @return A vector containing the 2.5, 25, 50, 75, 97.5th percentile values and the mean
#' @note Function transplanted from lmisc package
#' @details If weighted is TRUE, the wtd.quantile and wtd.mean functions from Hmisc are called, and a weights
#' vector equal in length to x is needed, passed as the minimum necessary parameter for calculating weighted 
#' quantiles. A named list should be provided where the name for each element matches the argument name for 
#' Hmisc::wtd.quantile and Hmisd::wtd.mean. Other possible arguments include type and normwt.
#' @examples 
#' set.seed(234)
#' x <- rnorm(20)
#' w <- sample(1:10, size = 20, replace = TRUE)
#' box_stats(x)
#' box_stats(x, weighted = TRUE, weight.opts = list("weights" = w))
#' @export
box_stats <- function(x, whiskers = c(0.025, 0.975), weighted = FALSE, weight.opts = NULL, na.rm = TRUE) {
  p <- c(whiskers[1], 0.25, 0.5, 0.75, whiskers[2])
  if(weighted == FALSE) {
    bs <- c(quantile(x, p, na.rm = na.rm), "mu" = mean(x, na.rm = na.rm))
  } else if(weighted == TRUE) {
      if(is.null(weight.opts)) stop("Parameters for weights are needed, and optionallym type, and normwt")
      w <- weight.opts
      if(!"weights" %in% names(w)) stop("You are missing, or have incorrectly named, the weights vector")
      if(!"type" %in% names(w)) w$type <- "quantile"
      if(!"normwt" %in% names(w)) w$normwt <- "FALSE"
      a <- Hmisc::wtd.quantile(x, weights = w$weights, type = w$type, normwt = w$normwt, probs = p, 
                               na.rm = na.rm) 
      b <- Hmisc::wtd.mean(x, weights = w$weights, normwt = w$normwt, na.rm = na.rm)
      bs <- c(a, "mu" = b)
  }
  return(bs)
}

#' Makes horizontal boxplots 
#' 
#' @param x Vector of the 6 boxplot statistics (e.g. 5, 25, 50, 75, 95th percentile and mean)
#' @param ycoord Y coordinate for plotting boxplot
#' @param n Number of boxplots to be placed on plot
#' @param inhgt Height of the interquartile box, scaled to size of device and number of boxes to be plotted
#' @param whiskhgt Height of the 95th percentile whiskers, scaled in the same way as inhgt
#' @param bcol A 1 or 4 element vector of colors for whiskers, interquartile box, median line, and mean point
#' @param bfill Fill color for interquartile box
#' @param lwd A 1 or 3 element vector of widths for whiskers, interquartile box, and median line
#' @param pcex Size of point indicating the mean 
#' @return Boxplot   
#' @note Function transplanted from lmisc package
#' @details To be used with dummy call to plot(). Specify the number of boxes to
#'  be placed on the Y axis. The parameters for bcol and lwd will recycle the 
#'  first element if the vector length is less than 4, and will only use the 
#'  first 4 elements if it is longer.  
#' @examples
#' x <- lapply(1:4, function(x) box_stats(sample(1:100, 10)))
#' plot(c(1, 100), c(0, 5), pch = "", xlab = "value", ylab = "", yaxt = "n")  
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4)
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4, pch = 5, pcex = 2)
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4, bcol = "red", bfill = "grey", whiskcol = "grey")
#' for(i in 1:length(x)) {
#'   boxplot_h(x[[i]], ycoord = i, n = 4, bcol = c("black", "grey40", "red", "blue"), bfill = "grey80", 
#'             lwd = c(2, 1, 7), whiskcol = "grey")
#' }
#' @export
#' 
boxplot_h <- function(x, ycoord, n, inhgt = 20, whiskhgt = 5, bcol = "black", bfill = "grey", pcex = 1, 
                      lwd = 1, pch = 20) {
  if(length(lwd) < 3) lwd <- rep(lwd, 3)
  if(length(lwd) > 3) {
    paste("only first three elements of lwd parameter will be used")
    lwd <- lwd[1:3]
  }
  if(length(bcol) < 4) bcol <- rep(bcol, 4)
  if(length(bcol) > 4) {
    paste("only first three elements of bcol parameter will be used")
    bcol <- bcol[1:4]
  }
  insc <- par()$din[1] / n * inhgt / 100 / 2
  insc2 <- insc * 0.8
  whisksc <- par()$din[1] / n * whiskhgt / 100
  if((x[2] - x[1]) > 0) {
    arrows(x[1], ycoord, x[2], ycoord, angle = 90, code = 1, length = whisksc, col = bcol[1], lwd = lwd[1])
  }
  if((x[5] - x[4]) > 0) {
    arrows(x[4], ycoord, x[5], ycoord, angle = 90, code = 2, length = whisksc, col = bcol[1], lwd = lwd[1])
  }
  rect(xleft = x[2], ybottom = ycoord - insc, xright = x[4], ytop = ycoord + insc, col = bfill, 
       border = bcol[2], lwd = lwd[2])
  lines(rep(x[3], 2), c(ycoord - insc2, ycoord + insc2), lwd = lwd[3], col = bcol[3], lend = 2)
  points(x[6], ycoord, pch = pch, col = bcol[4], cex = pcex)
}

#' Makes vertical boxplots 
#' 
#' @param xcoord X coordinate for plotting boxplot
#' @param y Vector of the 6 boxplot statistics (e.g. 5, 25, 50, 75, 95th percentile and mean)
#' @param n Number of boxplots to be placed on plot
#' @param inhgt Height of the interquartile box, scaled to size of device and number of boxes to be plotted
#' @param whiskhgt Height of the 95th percentile whiskers, scaled in the same way as inhgt
#' @param bcol A 1 or 4 element vector of colors for whiskers, interquartile box, median line, and mean point
#' @param bfill Fill color for interquartile box
#' @param lwd A 1 or 3 element vector of widths for whiskers, interquartile box, and median line
#' @param pcex Size of point indicating the mean 
#' @note Function transplanted from lmisc package
#' @return Boxplot   
#' @examples
#' x <- lapply(1:4, function(x) box_stats(sample(1:100, 10)))
#' plot(c(1, 100), c(0, 5), pch = "", xlab = "value", ylab = "", yaxt = "n")  
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4)
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4, pch = 5, pcex = 2)
#' #for(i in 1:length(x)) boxplot_h(x[[i]], ycoord = i, n = 4, bcol = "red", bfill = "grey", whiskcol = "grey")
#' for(i in 1:length(x)) {
#'   boxplot_h(x[[i]], ycoord = i, n = 4, bcol = c("black", "grey40", "red", "blue"), bfill = "grey80", 
#'             lwd = c(2, 1, 7), whiskcol = "grey")
#' }
#' @export
#' 
boxplot_v <- function(xcoord, y, n, inhgt = 20, whiskhgt = 5, bcol = "black", bfill = "grey", pcex = 1, 
                      lwd = 1, pch = 20) {
  if(length(lwd) < 3) lwd <- rep(lwd, 3)
  if(length(lwd) > 3) {
    paste("only first three elements of lwd parameter will be used")
    lwd <- lwd[1:3]
  }
  if(length(bcol) < 4) bcol <- rep(bcol, 4)
  if(length(bcol) > 4) {
    paste("only first three elements of bcol parameter will be used")
    bcol <- bcol[1:4]
  }
  insc <- par()$din[1] / n * inhgt / 100 / 2
  insc2 <- insc * 0.8
  whisksc <- par()$din[2] / n * whiskhgt / 100
  if((y[2] - y[1]) > 0) {
    arrows(xcoord, y[1], xcoord, y[2], angle = 90, code = 1, length = whisksc, col = bcol[1], lwd = lwd[1])
  }
  if((y[5] - y[4]) > 0) {
    arrows(xcoord, y[4], xcoord, y[5], angle = 90, code = 2, length = whisksc, col = bcol[1], lwd = lwd[1])
  }
  rect(xleft = xcoord - insc, ybottom = y[2], xright = xcoord + insc, ytop = y[4], col = bfill, 
       border = bcol[2], lwd = lwd[2])
  lines(c(xcoord - insc2, xcoord + insc2), rep(y[3], 2), lwd = lwd[3], col = bcol[3], lend = 2)
  points(xcoord, y[6], pch = 20, col = bcol[4], cex = pcex)
}