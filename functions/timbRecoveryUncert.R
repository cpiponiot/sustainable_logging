timbRecoveryUncert = function(timeLength, logIntensity, logCycle, omega0, longitude, latitude, dti = NA) {
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
  
  load("variables and parameters/paramStan.Rdata")
  load("variables and parameters/FORMINDVariables.Rdata")
  load("variables and parameters/stemMortKrig.Rdata")
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
  # dfPred = expand.grid(dfPred, iter = 1:100)
  
  if (nrow(merge(dfPred, spatVariables, by=c("long","lat"))) < nrow(dfPred))
    stop(paste(nrow(dfPred) - nrow(merge(dfPred, spatVariables, by=c("long","lat"))), 
               "locations are outside the Amazon region."))
  
  dfPred = merge(dfPred, spatVariables, by=c("long","lat"))
  dfPred = merge(dfPred, pars_Vrec[,-"lp__"], by = "iter") 
  dfPred = merge(dfPred, pars_pR[,-"lp__"], by = "iter") 
  dfPred = merge(dfPred, pars_rho[,-"lp__"], by = "iter") 
  
  dfPred$aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND*pars_Vrec$theta
  ##!! problem: aM < 0
  dfPred = merge(dfPred, data.table(iter = 1:100, pdef = rbeta(100, 6, 14)), by = "iter")
  
  ## initial maturity 
  if (any(is.na(dti))) 
    dfPred$dti = sample(pars_dti, nrow(dfPred))
  
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
  predTime = merge(dfPred, data.table(expand.grid(iter = 1:100, 
                                                  site = 1:length(longitude), 
                                                  t = 1:timeLength)), by = c("iter", "site"))
  
  ## get maturity
  predTime[, ncycle := ceiling(t/logCycle)]
  predTime[, tPL := t - floor(t/logCycle)*logCycle]
  
  dfmatPreLog = data.table(matPreLog)
  colnames(dfmatPreLog) = as.character(1:(nCycles+1))
  dfmatPreLog$site = dfPred$site
  dfmatPreLog$iter = dfPred$iter
  dfmatPreLog = melt(dfmatPreLog, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "mat0")
  dfmatPreLog[, ncycle := as.numeric(ncycle)]
  predTime = merge(predTime, dfmatPreLog, by = c("iter","site","ncycle"))
  predTime[, mat := mat0 + tPL]
  
  ## calculate volume 
  predTime[, volume := volume(mat, aG_FORMIND, aM, bP, bM, theta, pdef)]
  
  volInterval = predTime[,.(inf = quantile(volume, 0.025), 
                            med = quantile(volume, 0.5), 
                            sup = quantile(volume, 0.975)) , .(t, site)]
  
  
  dfVextReal = data.table(vextReal)
  colnames(dfVextReal) = as.character(1:nCycles)
  dfVextReal$site = dfPred$site
  dfVextReal$iter = dfPred$iter
  dfVextReal = melt(dfVextReal, id.vars = c("iter","site"), variable.name = "ncycle", value.name = "vextReal")
  dfVextReal[, ncycle := as.numeric(ncycle)]
  vextInterval = dfVextReal[,.(inf = quantile(vextReal, 0.025), 
                               med = quantile(vextReal, 0.5), 
                               sup = quantile(vextReal, 0.975)) , .(ncycle, site)]
  
  
  return(list(volInterval, vextInterval))
}