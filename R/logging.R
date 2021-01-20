#' Simulate effect of logging on timber volumes
#'
#' This function creates S3 objects of class "numeric".
#'
#' @param vol0
#' @param om0
#' @param logIntensity objective extracted volume
#' @param pdef Proportion of defective stems
#' @param psi Parameter of the damage model 1
#' @param e Parameter of the damage model 2
#' @param uncertainties 
#' 
#' @return A data frame of spatial parameters (columns)
#'
#' @export
#'
logging <- function(vol0, om0, logIntensity, pdef, psi, e, uncertainties) 
{
  ## load all functions
  file.sources = list.files(path = "R/", pattern="*.R", full.names = TRUE)
  sapply(file.sources, source, .GlobalEnv)
  
  vol0Real <- vol0 * (1-pdef)
  vextReal <- apply(cbind(vol0Real * om0, logIntensity), 1, min)
  
  ## total volume loss
  deltaV <-
    deltaVPrediction(
      V0 = vol0Real,
      Vext = vextReal,
      om0 = om0,
      psi = psi,
      e = e, 
      uncertainties = uncertainties
    )
  
  ## post logging proportion of commercial species
  omega1 <- (om0 * vol0Real - vextReal) / (vol0Real - deltaV)
  
  ## if deltaV = V0 (ie all trees > 50 cm have been removed) : reset to om0
  omega1[vol0Real == deltaV] <- om0[vol0Real == deltaV]
 
  return(data.frame(volPost = (vol0Real-deltaV)/(1-pdef), omPost = omega1, vextReal = vextReal))
}
