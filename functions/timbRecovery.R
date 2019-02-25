timbRecovery = function(timeLength, logIntensity, logCycle, omega0, longitude, latitude, dti = NA, 
                        uncertainties = TRUE, area = NA) {
  ## without error propagation: timbRecovery
  
  if (length(longitude) != length(latitude) )
    stop( "Coordinates should have the same size.")
  
  if ( length(timeLength) != 1 ) 
    stop( "timeLength should be of length 1.")
  
  if (!(length(logIntensity) %in% c(1, length(longitude))) )
    stop( "logIntensity should be of length 1 or have the same size as coordinates.")
  
  if (!(length(logCycle) %in% c(1, length(longitude))) )
    stop( "logCycle should be of length 1 or have the same size as coordinates.")
  
  if (!(length(omega0) %in% c(1, length(longitude))) )
    stop( "omega0 should be of length 1 or have the same size as coordinates.")
  
  if (!(length(dti) %in% c(1, length(longitude))) )
    stop( "dti should be of length 1 or have the same size as coordinates.")  
  
  if (!is.na(area) & length(area) != length(longitude)) 
    stop( "area should have the same size as coordinates.")
  
  if (uncertainties){
    load("variables and parameters/paramStan.Rdata")
    load("variables and parameters/FORMINDVariables.Rdata")
    load("variables and parameters/stemMortKrig.Rdata")
  } else {
    load("variables and parameters/paramStan_maxL.Rdata")
    load("variables and parameters/FORMINDVariables_maxL.Rdata")
    load("variables and parameters/stemMortKrig.Rdata")
    stemMortKrig$sd_logit_sm = 0
    spatVariables$iter = "maxL"
    pars_Vrec$iter = "maxL"
    pars_rho$iter = "maxL"
    pars_pR$iter = "maxL"
  }
  source("functions/omega_t.R")  
  source("functions/updateLoggingParams.R")
  
  spatVariables = merge(spatVariables, stemMortKrig, by = c("long","lat"))
  
  logist = function(x) 1 / ( exp(-x) + 1 )
  
  spatVariables$stem_mort = logist(rnorm(nrow(spatVariables), spatVariables$mu_logit_sm, spatVariables$sd_logit_sm))
  spatVariables = spatVariables[,-grep("logit",colnames(spatVariables)), with = FALSE]
  
  dfPred = data.table(site = 1:length(longitude), 
                      long = round(longitude + 0.5) - 0.5, 
                      lat = round(latitude + 0.5) - 0.5, 
                      logIntensity = logIntensity, 
                      logCycle = logCycle,
                      omega0 = omega0, 
                      dti = dti)
  
  if (nrow(merge(dfPred, spatVariables, by=c("long","lat"))) < nrow(dfPred))
    stop(paste(nrow(dfPred) - nrow(merge(dfPred, spatVariables, by=c("long","lat"))), 
               "locations are outside the Amazon region."))
  
  dfPred = merge(dfPred, spatVariables, by=c("long","lat"))
  dfPred = merge(dfPred, pars_Vrec[,-"lp__"], by = "iter") 
  dfPred = merge(dfPred, pars_pR[,-"lp__"], by = "iter") 
  dfPred = merge(dfPred, pars_rho[,-"lp__"], by = "iter") 
  
  dfPred$aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND*pars_Vrec$theta
  ##!! problem: aM < 0
  ## for now
  counts = 0
  while (any(dfPred$aM < 0) & counts < 120) {  
    new_df = dfPred[aM < 0, c("iter","long","lat")]
    new_df$iter = sample(1:max(spatVariables$iter), sum(dfPred$aM < 0), replace = TRUE)
    new_df = merge(new_df, spatVariables, by = c("iter", "long", "lat"))
    
    dfPred[aM < 0, c("aG_FORMIND","vmax_FORMIND")] <- new_df[, c("aG_FORMIND","vmax_FORMIND")]
    dfPred$aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND*pars_Vrec$theta
    
    counts = counts + 1
  }
  
  if (uncertainties) {
    dfPred = merge(dfPred, data.table(iter = 1:100, pdef = rbeta(100, 6, 14)), by = "iter")
  } else dfPred$pdef = 0.3
  
  ## initial maturity 
  if (any(is.na(dti))) 
    dfPred$dti = sample(pars_dti, nrow(dfPred), replace = TRUE)
  
  dfPred[, matInit := stem_mort^(-lambda_ti)*( 1 - dti)]
  
  ## number of cycles to reach timeLength
  nCycles = max(ceiling(timeLength / logCycle))
  
  matPreLog = matrix(dfPred$matInit, ncol = 1)
  omPreLog = matrix(dfPred$omega0, ncol = 1)
  omPostLog = c()
  omRecruits = c()
  vextReal = c()
  
  for (i in 1:nCycles) {
    
    ## update stand parameters (maturity, proportion of commercial species) after logging
    newParams = updateLoggingParams(mat0 = matPreLog[,i], omega0 = omPreLog[,i], logIntensity = dfPred$logIntensity, 
                                    aG = dfPred$aG, aM = dfPred$aM, bG = dfPred$bP, bM = dfPred$bM, 
                                    theta = dfPred$theta, pdef = dfPred$pdef, psi = dfPred$rho, e = dfPred$e)
    vextReal = cbind(vextReal, newParams[[3]])
    
    ## update the proprotion of commercial recruits
    omR <- omPreLog[,i]
    ## prelogging proportion of commercial trees < proportion of commercial species in recruited trees < initial proportion of commercial species
    omR[omPreLog[,i] <= omPreLog[,1]] <- runif(sum(omPreLog[,i] <= omPreLog[,1]), 
                                               min = omPreLog[omPreLog[,i] <= omPreLog[,1], i], 
                                               max = omPreLog[omPreLog[,i] <= omPreLog[,1], 1])
    # in some cases om > omR (due probably to the discretisation of om calculation, but difference is not very important)
    omR[omPreLog[,i] > omPreLog[,1]] <- runif(sum(omPreLog[,i] > omPreLog[,1]), 
                                              min = omPreLog[omPreLog[,i] > omPreLog[,1], 1], 
                                              max = omPreLog[omPreLog[,i] > omPreLog[,1], i])   
    omRecruits = cbind(omRecruits, omR)
    
    ## simulate the proportion of recruits after logging
    omT = apply(cbind(dfPred$logCycle, newParams[[1]],  newParams[[2]], omRecruits[,i], dfPred$aG, dfPred$aM, 
                      dfPred$bP, dfPred$bM, dfPred$theta, dfPred$intercept, dfPred$slope), 1, omega_t)
    
    matPreLog = cbind(matPreLog, newParams[[1]] + dfPred$logCycle)
    omPreLog = cbind(omPreLog, omT)
    omPostLog = cbind(omPostLog, newParams[[2]])
    
  }
  
  ### predTime: trajectories
  predTime = merge(dfPred, data.table(expand.grid(iter = unique(dfPred$iter), 
                                                  site = 1:length(longitude), 
                                                  t = 1:timeLength)), by = c("iter", "site"))
  
  predTime[, ncycle := ceiling(t/logCycle)] ## nb of the logging cycle
  predTime[, tPL := t - floor(t/logCycle)*logCycle] ## time since last logging event
  
  ## cycle-specific parameters -- maturity 
  dfmatPreLog = data.table(matPreLog)
  colnames(dfmatPreLog) = as.character(1:(nCycles+1))
  dfmatPreLog$site = dfPred$site
  dfmatPreLog$iter = dfPred$iter
  dfmatPreLog = melt(dfmatPreLog, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "mat0")
  dfmatPreLog[, ncycle := as.numeric(ncycle)]
  predTime = merge(predTime, dfmatPreLog, by = c("iter","site","ncycle"))
  
  ## get maturity
  predTime[, mat := mat0 + tPL]
  
  ## cycle-specific parameters -- omega : proportion of commercial volume
  
  ## post-logging value
  dfomPostLog = data.table(omPostLog)
  colnames(dfomPostLog) = as.character(1:nCycles)
  dfomPostLog$site = dfPred$site
  dfomPostLog$iter = dfPred$iter
  dfomPostLog = melt(dfomPostLog, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "omegaPL")
  dfomPostLog[, ncycle := as.numeric(ncycle)]
  predTime = merge(predTime, dfomPostLog, by = c("iter","site","ncycle"))
  
  ## pre-logging value
  dfomPreLog = data.table(omPreLog)
  colnames(dfomPreLog) = as.character(0:(nCycles))
  dfomPreLog$site = dfPred$site
  dfomPreLog$iter = dfPred$iter
  dfomPreLog = melt(dfomPreLog, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "omegaFinal")
  dfomPreLog[, ncycle := as.numeric(ncycle)]
  predTime = merge(predTime, dfomPreLog, by = c("iter","site","ncycle"))
  
  ## get omega
  predTime[, omega := omegaPL + (tPL) * (omegaFinal - omegaPL)/(logCycle)]
  
  ## calculate volume 
  predTime[, volume := volume(mat, aG_FORMIND, aM, bP, bM, theta, pdef)]
  
  if (is.na(area)) {
    if (uncertainties) {
      volInterval = predTime[,.(inf = quantile(volume*omega, 0.025), 
                                med = quantile(volume*omega, 0.5), 
                                sup = quantile(volume*omega, 0.975)) , .(t, site)]
    } else {
      predTime[,volTimb := volume * omega]
      volInterval = predTime[,c("t", "site", "volTimb")]
    }
    
    dfVextReal = data.table(vextReal)
    colnames(dfVextReal) = as.character(1:nCycles)
    dfVextReal$site = dfPred$site
    dfVextReal$iter = dfPred$iter
    dfVextReal = melt(dfVextReal, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "vextReal")
    dfVextReal[, ncycle := as.numeric(ncycle)]
    if (uncertainties) {
      dfVextReal = dfVextReal[,.(inf = quantile(vextReal, 0.025), 
                                 med = quantile(vextReal, 0.5), 
                                 sup = quantile(vextReal, 0.975)) , .(ncycle, site)]
    } 
    
  } else {
    
    predTime = merge(predTime, data.table(site = 1:length(longitude), area), by = "site")
    
    if (uncertainties) {
      
      volInterval = predTime[,.(inf = quantile(sum(volume*omega*area), 0.025), 
                                med = quantile(sum(volume*omega*area), 0.5), 
                                sup = quantile(sum(volume*omega*area), 0.975)) , .(t)]
    } else {
      predTime[, .(volTimb = sum(volume * omega * area)), .(t)]
      volInterval = predTime[,c("t", "volTimb")]
    }
    
    dfVextReal = data.table(vextReal)
    colnames(dfVextReal) = as.character(1:nCycles)
    dfVextReal$site = dfPred$site
    dfVextReal$iter = dfPred$iter
    dfVextReal = merge(dfVextReal, data.table(site = 1:length(longitude), area), by = "site")
    
    dfVextReal = melt(dfVextReal, id.vars = c("iter","site","area"), variable.name = "ncycle", value.name = "vextReal")
    dfVextReal[, ncycle := as.numeric(ncycle)]
    if (uncertainties) {
      dfVextReal = dfVextReal[,.(inf = quantile(sum(vextReal*area), 0.025), 
                                 med = quantile(sum(vextReal*area), 0.5), 
                                 sup = quantile(sum(vextReal*area), 0.975)) , .(ncycle)]
    } else {
      dfVextReal = dfVextReal[,.(vextReal = sum(vextReal*area)) , .(ncycle)]
    }
    
  }
  
  return(list(volInterval, dfVextReal))
}