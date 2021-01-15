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
                            silv = NULL, 
                            keepAll = FALSE
) {
  ## without error propagation: timbRecovery
  
  ## error checks
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
  
  ## load all functions
  file.sources = list.files(path = "R/", pattern="*.R", full.names = TRUE)
  sapply(file.sources, source, .GlobalEnv)
  
  ## prepare prediction data frame
  variables <- data.frame(site = 1:length(longitude), 
                          longitude, latitude, logIntensity, logCycle, omega0)
  
  dfPred <- spatialPars(variables[, c("longitude", "latitude")], uncert = uncertainties)
  dfPred <- merge(dfPred, variables, by = "site")
  
  ## number of cycles to reach timeLength
  nCycles = max(ceiling(timeLength / logCycle))
  
  # matPreLog = matrix(dfPred$matInit, ncol = 1)
  # omPreLog = matrix(dfPred$omega0, ncol = 1)
  # omPostLog = c()
  # vPreLog = c()
  # vPostLog = c()
  # omRecruits = c()
  # vextReal = c()
  
  ## initial maturity and volume
  if (any(is.na(dti))) {
    load("data/paramStan.Rdata")
    dfPred$dti = sample(pars_dti, nrow(dfPred), replace = TRUE)
  }
  dfPred[, matInit := stem_mort ^ (-lambda_ti) * (1 - dti)]
  dfPred[, vol0 := volume(matInit, aG_FORMIND, aM, bP, bM, theta, pdef)]
  
  ## make a data frame with n = cycle, stage = pre or post (logging), and omega and volume as data
  ## initialize 
  dfPred$volPre <- dfPred$vol0
  dfPred$omPre <- dfPred$omega0
  list_results <- lapply(1:nCycles, function(i) {
    results <- dfPred[, logging(volPre, omPre, logIntensity, pdef, rho, e)]
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
      if (keepAll) {
        rec$site <- dfPred$site[k]
        rec$iter <- dfPred$iter[k]
      }
      rectot = rbind(rectot, rec)
    } 
    if (!keepAll) {
      colnames(rectot) <- c("volRec", "omRec")
      results <- cbind(results, rectot)
      dfPred$volPre <- rectot[, 1]
      dfPred$volPre <- rectot[,2]
    } else {
      results <- merge(results, rectot, by = c("site", "iter"))
      ## update volPre and omPre
      data.table::setDT(rectot)
      rectot[, tcycle := max(trec), .(iter, site)]
      dfPred$volPre <- rectot[trec==tcycle, 1]
      dfPred$volPre <- rectot[trec==tcycle,2]
    }
    
    ## TODO check that pdef is correctly accounted for
    ## TODO remove volPost, omPost when keepAll = TRUE (not needed)
    return(results)
  })
  
  simu_cycles <- data.table::rbindlist(list_results, idcol = "ncycle")
  
  # ### predTime: trajectories
  # predTime = merge(dfPred, data.table(expand.grid(
  #   iter = unique(dfPred$iter),
  #   site = 1:length(longitude),
  #   t = 0:timeLength
  # )), by = c("iter", "site"))
  # 
  # predTime[, ncycle := ceiling(t / logCycle)] ## nb of the logging cycle
  # predTime[, tPL := t - floor((t - 1) / logCycle) * logCycle] ## time since last logging event
  # 
  # ## cycle-specific parameters -- maturity
  # dfmatPreLog = data.table(matPreLog)
  # colnames(dfmatPreLog) = as.character(0:(nCycles))
  # dfmatPreLog$site = dfPred$site
  # dfmatPreLog$iter = dfPred$iter
  # dfmatPreLog = melt(
  #   dfmatPreLog,
  #   id.vars = c("iter", "site"),
  #   variable.name = "ncycle",
  #   value.name = "matFinal",
  #   variable.factor = FALSE
  # )
  # dfmatPreLog[, ncycle := as.numeric(ncycle)]
  # predTime = merge(predTime, dfmatPreLog, by = c("iter", "site", "ncycle"))
  # 
  # ## get maturity
  # predTime[, mat := matFinal - logCycle + tPL]
  # 
  # ## cycle-specific parameters -- omega : proportion of commercial volume
  # 
  # ## post-logging omega value
  # dfomPostLog = data.table(0, omPostLog)
  # colnames(dfomPostLog) = as.character(0:nCycles)
  # dfomPostLog$site = dfPred$site
  # dfomPostLog$iter = dfPred$iter
  # dfomPostLog = melt(
  #   dfomPostLog,
  #   id.vars = c("iter", "site"),
  #   variable.name = "ncycle",
  #   value.name = "omegaPL",
  #   variable.factor = FALSE
  # )
  # dfomPostLog[, ncycle := as.numeric(ncycle)]
  # predTime = merge(predTime, dfomPostLog, by = c("iter", "site", "ncycle"))
  # 
  # ## pre-logging value
  # dfomPreLog = data.table(omPreLog)
  # colnames(dfomPreLog) = as.character(0:(nCycles))
  # dfomPreLog$site = dfPred$site
  # dfomPreLog$iter = dfPred$iter
  # dfomPreLog = melt(
  #   dfomPreLog,
  #   id.vars = c("iter", "site"),
  #   variable.name = "ncycle",
  #   value.name = "omegaFinal",
  #   variable.factor = FALSE
  # )
  # dfomPreLog[, ncycle := as.numeric(ncycle)]
  # predTime = merge(predTime, dfomPreLog, by = c("iter", "site", "ncycle"))
  # 
  # ## get omega
  # predTime[, omega := omegaPL + (tPL) * (omegaFinal - omegaPL) / (logCycle)]
  # 
  # ## calculate volume
  # predTime[, volume := volume(mat, aG_FORMIND, aM, bP, bM, theta, pdef)]
  # 
  # if (length(area) == 1 & sum(is.na(area)) == 1) {
  #   if (uncertainties) {
  #     volInterval = predTime[, .(
  #       inf = quantile(volume * omega, 0.025),
  #       med = quantile(volume * omega, 0.5),
  #       sup = quantile(volume * omega, 0.975)
  #     ) , .(t, site)]
  #   } else {
  #     predTime[, volTimb := volume * omega]
  #     volInterval = predTime[, c("t", "site", "volTimb")]
  #   }
  #   
  #   dfVextReal = data.table(vextReal)
  #   colnames(dfVextReal) = as.character(1:nCycles)
  #   dfVextReal$site = dfPred$site
  #   dfVextReal$iter = dfPred$iter
  #   dfVextReal = melt(
  #     dfVextReal,
  #     id.vars = c("iter", "site"),
  #     variable.name = "ncycle",
  #     value.name = "vextReal"
  #   )
  #   dfVextReal[, ncycle := as.numeric(ncycle)]
  #   if (uncertainties) {
  #     dfVextReal = dfVextReal[, .(
  #       inf = quantile(vextReal, 0.025),
  #       med = quantile(vextReal, 0.5),
  #       sup = quantile(vextReal, 0.975)
  #     ) , .(ncycle, site)]
  #   }
  #   
  # } else {
  #   predTime = merge(predTime, data.table(site = 1:length(longitude), area), by = "site")
  #   
  #   if (uncertainties) {
  #     volInterval = predTime[, .(
  #       inf = quantile(sum(volume * omega * area), 0.025),
  #       med = quantile(sum(volume * omega * area), 0.5),
  #       sup = quantile(sum(volume * omega * area), 0.975)
  #     ) , .(t)]
  #   } else {
  #     predTime[, .(volTimb = sum(volume * omega * area)), .(t)]  ## xxx maybe add here a silviculture module
  #     volInterval = predTime[, c("t", "volTimb")]
  #   }
  #   
  #   dfVextReal = data.table(vextReal)
  #   colnames(dfVextReal) = as.character(1:nCycles)
  #   dfVextReal$site = dfPred$site
  #   dfVextReal$iter = dfPred$iter
  #   dfVextReal = merge(dfVextReal, data.table(site = 1:length(longitude), area), by = "site")
  #   
  #   dfVextReal = melt(
  #     dfVextReal,
  #     id.vars = c("iter", "site", "area"),
  #     variable.name = "ncycle",
  #     value.name = "vextReal"
  #   )
  #   dfVextReal[, ncycle := as.numeric(ncycle)]
  #   if (uncertainties) {
  #     dfVextReal = dfVextReal[, .(
  #       inf = quantile(sum(vextReal * area), 0.025),
  #       med = quantile(sum(vextReal * area), 0.5),
  #       sup = quantile(sum(vextReal * area), 0.975)
  #     ) , .(ncycle)]
  #   } else {
  #     dfVextReal = dfVextReal[, .(vextReal = sum(vextReal * area)) , .(ncycle)]
  #   }
  #   
  # }
  # 
  # return(list(volInterval, dfVextReal))
}

logit = function(x){
  log(x / (1 - x))}
logist = function(x){
  1 / (exp(-x) + 1)}