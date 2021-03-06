#' Function to estimate maturity based on corresponding volume and parameters
#'
#' This function creates S3 objects of class "numeric".
#'
#' @param vol Volume based on which the maturity is estimated
#' @param ag Growth parameter 1
#' @param am Mortality parameter 2
#' @param bg Growth parameter 2
#' @param am Mortality parameter 2
#' @param theta Respiration factor
#' @param pdef Proportion of defective stems
#' @param tmax Maximum value of tmax (results interval is then c(0, tmax)).
#'   Default value is 1000.
#'
#' @return A data frame xxx
#'
#' @export
#' 
getMaturity <- function(vol, ag, am, bg, bm, theta, pdef = 0, tmax = 1000) { 
  t_est <- c()
  for (i in 1:length(vol)) {
    equ = function(x) volume(x, ag, am, bg, bm, theta, pdef) - vol[i]
    t_est[i] <- uniroot(equ,c(0,tmax))$root
  }
  return(t_est) 
}
