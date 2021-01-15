#' Function to prepare spatial data
#'
#' This function creates S3 objects of class "numeric".
#'
#' @param coords Coordinates (longlat) of all the locations for which you need
#'   spatial parameters. Should be a matrix with 2 columns (long, lat) and one
#'   row for each location.
#' @param uncert Logical: is the
#'
#' @return A data frame of spatial parameters (columns)
#'
#' @export
#' 
spatialPars <- function(coords, uncert = TRUE) {
  ## load data
  if (uncert) {
    load("data/paramStan.Rdata")
    load("data/FORMINDVariables.Rdata")
    load("data/stemMortKrig.Rdata")
  } else {
    load("data/paramStan_maxL.Rdata")
    load("data/FORMINDVariables_maxL.Rdata")
    load("data/stemMortKrig.Rdata")
    stemMortKrig$sd_logit_sm = 0
    spatVariables$iter = "maxL"
    pars_Vrec$iter = "maxL"
    pars_rho$iter = "maxL"
    pars_pR$iter = "maxL"
  }
  
  ## prepare data
  spatVariables = merge(spatVariables, stemMortKrig, by = c("long", "lat"))
  spatVariables$stem_mort = logist(
    truncnorm::rtruncnorm(
      nrow(spatVariables),
      spatVariables$mu_logit_sm,
      spatVariables$sd_logit_sm,
      a = logit(0.005),
      b = logit(0.05)
    )
  )
  spatVariables = spatVariables[, -grep("logit", colnames(spatVariables)), with = FALSE]
  
  ## prepare prediction data frame
  dfPred = data.table::data.table(
    site = 1:nrow(coords),
    long = round(coords[, 1] + 0.5) - 0.5,
    lat = round(coords[, 2] + 0.5) - 0.5
  )
  
  if (nrow(merge(dfPred, spatVariables, by = c("long", "lat"))) < nrow(dfPred))
    stop(paste(
      nrow(dfPred) - nrow(merge(
        dfPred, spatVariables, by = c("long", "lat")
      )),
      "locations are outside the Amazon region."
    ))
  
  dfPred = merge(dfPred, spatVariables, by = c("long", "lat"))
  dfPred = merge(dfPred, pars_Vrec[, -"lp__"], by = "iter")
  dfPred = merge(dfPred, pars_pR[, -"lp__"], by = "iter")
  dfPred = merge(dfPred, pars_rho[, -"lp__"], by = "iter")
  
  dfPred$aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND * pars_Vrec$theta
  ##!! problem: aM < 0
  ## for now: resample until all aM > 0
  counts = 0 ## for safety (could have an infinite loop)
  while (any(dfPred$aM < 0) & counts < 100) {
    new_df = dfPred[aM < 0, c("iter", "long", "lat")]
    new_df$iter = sample(1:max(spatVariables$iter), sum(dfPred$aM < 0), replace = TRUE)
    new_df = merge(new_df, spatVariables, by = c("iter", "long", "lat"))
    
    dfPred[aM < 0, c("aG_FORMIND", "vmax_FORMIND")] <-
      new_df[, c("aG_FORMIND", "vmax_FORMIND")]
    dfPred$aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND * pars_Vrec$theta
    
    counts = counts + 1
  }
  
  if (uncert) {
    dfPred = merge(dfPred, data.table::data.table(iter = 1:100, pdef = rbeta(100, 6, 14)), by = "iter")
  } else
    dfPred$pdef = 0.3
  
  return(dfPred)
}
