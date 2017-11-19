#' Sets interal of non-replicated samples to 365 * 10000 days
#' @param tbtwn Vector of interval values
#' @return Vector
#' @export
to_infin_byond <- function(tbtwn) ifelse(tbtwn == 0, 365 * 10000, tbtwn)