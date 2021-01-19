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
#' @param area
#' @param silv Numeric vector of length 2: the first value is the % increase of
#'   commercial trees growth due to silvicultural treatments, and the second
#'   value is the duration of those effects. Default is c(0,0) (no silvicultural
#'   treatment).
#' @param keepAll Logical (cf recovery() function): keep all time steps? Default is FALSE
#'
#' @return A data frame xxx
#'
#' @export
#' 
simulateLogging <- function(timeLength,
                            logIntensity,
                            logCycle,
                            omega0,
                            longitude,
                            latitude,
                            dti = NA,
                            uncertainties = TRUE,
                            area = NA,
                            silv = c(0,0), 
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
  if (sum(is.na(area)) == 0 & length(area) != length(longitude))
    stop("area should have the same size as coordinates.")
  
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
  dfPred[, vol0 := volume(matInit, aG_FORMIND, aM, bP, bM, theta, pdef)]
  
  ## simulate n logging cycles ####
  ## number of cycles to reach timeLength
  nCycles <- max(ceiling(timeLength / logCycle))
  ## make a data frame with n = cycle, stage = pre or post (logging), and omega and volume as data
  
  ## initialize volume and omega
  dfPred$volPre <- dfPred$vol0
  dfPred$omPre <- dfPred$omega0
  list_results <- vector(mode = "list", length = nCycles)
  
  for (i in 1:nCycles) {
    results <- dfPred[, logging(volPre, omPre, logIntensity, rho, e)]
    results <- cbind(dfPred[, c("site","iter")], results)
    ## post logging vol and omega
    dfPred$vol1 <- results$volPost
    dfPred$om1 <- results$omPost
    ## recovery process
    rectot = c()
    for (k in 1:nrow(dfPred)) {
      rec <- recovery(vol1 = dfPred$vol1[k], om1 =dfPred$om1[k], 
                      logCycle = dfPred$logCycle[k], silv = silv, 
                      ag = dfPred$aG_FORMIND[k], bg = dfPred$bP[k], 
                      am = dfPred$aM[k], bm = dfPred$bM[k],
                      theta = dfPred$theta[k], omR = dfPred$omPre[k], 
                      intR = dfPred$intercept[k], sloR = dfPred$slope[k], 
                      keepAll = keepAll)
      rec$site <- dfPred$site[k]
      rec$iter <- dfPred$iter[k]
      rectot <- rbind(rectot, rec)
    } 
    results <- merge(results, rectot, by = c("site", "iter"))
    
    ## update volPre and omPre
    data.table::setDT(rectot)
    rectot[, tcycle := max(trec), .(iter, site)]
    dfPred$volPre <- rectot[trec==tcycle, "volume"]
    dfPred$omPre <- rectot[trec==tcycle, "omega"]
    
    results <- data.table::melt(results, id.vars = c("site", "trec"), 
                                    measure.vars = c("volume", "omega", "vextReal"))
    if (uncertainties) {
        results <- results[, .(lwr = quantile(value, 0.025), 
                               med = quantile(value, 0.5), 
                               upr = quantile(value, 0.975)), 
                           .(site, trec, variable)]
    } 
    results <- subset(results, !(variable == "vextReal" & trec > 1))
    ## TODO check that pdef is correctly accounted for
    
    list_results[[i]] <- results
  }
  
  simu_cycles <- data.table::rbindlist(list_results, idcol = "ncycle")
  return(simu_cycles)
}

logit = function(x){
  log(x / (1 - x))}
logist = function(x){
  1 / (exp(-x) + 1)}