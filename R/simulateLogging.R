#' Function to simulate several cycles of selective logging in Amazonia and
#' associated timber volume stocks
#'
#' This function creates S3 objects of class "numeric".
#'
#' @param timeLength
#' @param logIntensity
#' @param logCycle
#' @param omega0
#' @param longitude
#' @param latitude
#' @param dti
#' @param uncertainties
#' @param silv Numeric vector of length 3: the first value is the % increase of
#' commercial trees growth due to silvicultural treatments, the second value is
#' the duration of those effects, and the third element controls the steepness
#' of the curve. Default is c(0,0,0) (no silvicultural treatment).
#' @param silv_upr Numeric vector of length 3: when uncertainties = TRUE, `silv`
#'   will be the median value of silviculture parameters and `silv_upr` as the
#'   97.5th percentile: values are then drawn from a lognormal distribution with 
#'   mean log(silv) and standard deviation -log(silv)/2.
#' @param keepAll Logical (cf recovery() function): keep all time steps? Default
#'   is FALSE
#'   
#' @return A data frame xxx
#'
#' @export
#' 
#' @examples 
#' 
#' dfsimu <- simulateLogging(
#'   timeLength = 200,
#'   logIntensity = 20,
#'   logCycle = 30,
#'   omega0 = 0.5,
#'   uncertainties = FALSE,
#'   longitude = -60,
#'   latitude = 0
#' )
#' 
simulateLogging <- function(timeLength,
                            logIntensity,
                            logCycle,
                            omega0,
                            longitude,
                            latitude,
                            dti = NA,
                            uncertainties = TRUE,
                            silv = c(0,0,0), 
                            silv_upr = NULL,
                            keepAll = FALSE
) {
  ## without error propagation: timbRecovery
  
  ## error checks ####
  if (length(longitude) != length(latitude))
    stop("Coordinates should have the same size.")
  if (length(timeLength) != 1)
    stop("timeLength should be of length 1.")
  if (!(length(logIntensity) %in% c(1, length(longitude))))
    stop("logIntensity should be of length 1 or have the same size as coordinates.")
  if (!(length(logCycle) %in% c(1, length(longitude))))
    stop("logCycle should be of length 1 or have the same size as coordinates.")
  if (!(length(omega0) %in% c(1, length(longitude))))
    stop("omega0 should be of length 1 or have the same size as coordinates.")
  if (!(length(dti) %in% c(1, length(longitude))))
    stop("dti should be of length 1 or have the same size as coordinates.")
  
  ## load all functions - remove when made a package ####
  file.sources = list.files(path = "R/", pattern="*.R", full.names = TRUE)
  sapply(file.sources, source, .GlobalEnv)
  
  ## prepare prediction data frame ####
  variables <- data.frame(site = 1:length(longitude), 
                          longitude, latitude, logIntensity, logCycle, omega0)
  
  dfPred <- spatialPars(variables[, c("longitude", "latitude")], uncert = uncertainties)
  dfPred <- merge(dfPred, variables, by = "site")
  
  ## initial maturity and volume
  if (any(is.na(dti))) {
    load("data/paramStan.Rdata")
    if (uncertainties) {
      dfPred$dti <- sample(pars_dti, nrow(dfPred), replace = TRUE)
    } else {
      dfPred$dti <- mean(pars_dti)
    }
  }
  dfPred[, matInit := stem_mort ^ (-lambda_ti) * (1 - dti)]
  dfPred[, vol0 := volume(matInit, aG_FORMIND, aM, bP, bM, theta)]
  
  ## silviculture treatment ##
  if (uncertainties & all(silv > 0) & !is.null(silv_upr)) {
    simsilv <- function(med, upr) rlnorm(100, log(med), log(upr/med)/2)
    silvs <- data.frame(iter = 1:100, 
                        silv1 = simsilv(silv[1], silv_upr[1]),
                        silv2 = simsilv(silv[2], silv_upr[2]), 
                        silv3 = simsilv(silv[3], silv_upr[3]))
    dfPred <- merge(dfPred, silvs, by = "iter")
  } else {
    dfPred$silv1 <- silv[1]
    dfPred$silv2 <- silv[2]
    dfPred$silv3 <- silv[3]
  }
  
  ## simulate n logging cycles ####
  ## number of cycles to reach timeLength
  nCycles <- max(ceiling(timeLength / logCycle))
  ## make a data frame with n = cycle, stage = pre or post (logging), and omega and volume as data
  
  ## initialize volume and omega
  dfPred$volPre <- dfPred$vol0
  dfPred$omPre <- dfPred$omega0
  list_results <- vector(mode = "list", length = nCycles)
  
  for (i in 1:nCycles) {
    results <- dfPred[, logging(volPre, omPre, logIntensity, pdef, rho, e, uncertainties)]
    results <- cbind(dfPred[, c("site","iter", "pdef", "vol0")], results)
    ## post logging vol and omega
    dfPred$vol1 <- results$volPost
    dfPred$om1 <- results$omPost
    ## proportion of commercial trees in recruits
    ## between prelogging proportion of commercial trees omPreLog 
    ## and initial proportion of commercial species omega0
    ## rm: with silviculture, omPre can be > than omega0
    dfPred$omR <- runif(nrow(dfPred), 
                        min = apply(cbind(dfPred$omPre, dfPred$omega0), 1, min), 
                        max = apply(cbind(dfPred$omPre, dfPred$omega0), 1, max))
    
    ## recovery process
    rectot <- c()
    for (k in 1:nrow(dfPred)) {
      rec <- recovery(vol1 = dfPred$vol1[k], om1 = dfPred$om1[k], 
                      logCycle = dfPred$logCycle[k], 
                      silv = c(dfPred$silv1[k], dfPred$silv2[k], dfPred$silv3[k]), 
                      ag = dfPred$aG_FORMIND[k], bg = dfPred$bP[k], 
                      am = dfPred$aM[k], bm = dfPred$bM[k],
                      theta = dfPred$theta[k], omR = dfPred$omR[k], 
                      intR = dfPred$intercept[k], sloR = dfPred$slope[k],
                      keepAll = keepAll)
      rec$site <- dfPred$site[k]
      rec$iter <- dfPred$iter[k]
      rec$volume[rec$volume > dfPred$vol0[k]] <- dfPred$vol0[k]
      rectot <- rbind(rectot, rec)
    } 
    results <- merge(results, rectot, by = c("site", "iter"))
    
    if (any(is.na(results$omega))) stop("problem with omega")
    
    ## update volPre and omPre
    data.table::setDT(rectot)
    rectot[, tcycle := max(trec), .(iter, site)]
    dfPred$volPre <- rectot[trec==tcycle, "volume"]
    dfPred$omPre <- rectot[trec==tcycle, "omega"]
    
    ## prepare output ##
    # 1. multiply volume by pdef
    results$volume <- results$volume*(1-results$pdef)
    # 2. melt results dataframe
    results <- data.table::melt(results, id.vars = c("site", "trec"), 
                                measure.vars = c("volume", "omega", "vextReal"))
    # 2b. calculate summary statistics
    if (uncertainties) {
      results <- results[, .(lwr = quantile(value, probs = 0.025), 
                             med = quantile(value, probs = 0.5), 
                             upr = quantile(value, probs = 0.975)), 
                         .(site, trec, variable)]
    } 
    results <- subset(results, !(variable == "vextReal" & trec > 1))
    ## TODO check that prelogging proportion of commercial trees < proportion of commercial species in recruited trees < initial proportion of commercial species
    list_results[[i]] <- results
  }
  simu_cycles <- data.table::rbindlist(list_results, idcol = "ncycle")
  return(simu_cycles)
}

logit = function(x){
  log(x / (1 - x))}
logist = function(x){
  1 / (exp(-x) + 1)}