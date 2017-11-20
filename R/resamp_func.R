#' Resampling with perturbation
#' @description Randomly perturbs scale values within specified uncertainty 
#' bounds
#' @param dat data.table of combined calibration and main scale results 
#' @param p Vector of uncertainty bounds, one per assessed dimension
#' @param iter Number of resampling iterations
#' @return List of data.table equal to length iter
#' @examples 
#' data(datf)
#' set.seed(1)
#' bootperturb <- resamp_func(datf, p = p, iter = 1000)
#' @export
resamp_func <- function(dat, p, iter) {
  vars <- c("plot_res", "act_ext", "eff_ext", "t_btwn_samp", "act_dur", 
            "eff_dur")
  bout <- lapply(1:iter, function(x) {  # x <- 1
    if((x / 100) %in% 1:100) print(x)
    if(length(p) < length(var)) stop("p must be length of var", call. = FALSE)
    dnew <- copy(dat[, vars, with = FALSE])
    # modifiers
    modvec <- sapply(p, function(y) {
      mv <- runif(nrow(dnew), min = 1 - y, max = 1 + y)
      mv[mv < 0] <- 0.00000001  # can't have reduction of more than 100%
      mv
    })
    
    # perturb and set any values < 0 to very small amount
    for(j in 1:ncol(modvec)) set(dnew, j = j, value = dnew[[j]] * modvec[, j])
    
    # correct physically impossible results 
    dnew[, plot_res := plot_res / 10000]  # set plot_res to ha
    dnew[plot_res > act_ext, plot_res := act_ext]  # plot_res can't be > act_ext
    dnew[plot_res > eff_ext, plot_res := eff_ext]  # or eff_ext
    dnew[act_ext > eff_ext, act_ext := eff_ext] # act_ext can't be > eff_ext
    dnew[t_btwn_samp > eff_dur, t_btwn_samp := eff_dur]  # int can't be > eff_dur
    dnew[act_dur > eff_dur, act_dur := eff_dur]  # act_dur can't be > eff_dur
    dnew[, plot_res := plot_res * 10000]  # set plot_res back to m2
    
    # set unreplicated to high value for first figure 
    dnew[, t_btwn_samp := to_infin_byond(t_btwn_samp)]  # set high
    cbind("type" = dat[, study_type], dnew)
  })
  return(bout)
}
