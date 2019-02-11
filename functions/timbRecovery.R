timbRecovery = function(timeLength, logIntensity, logCycle, omega0, longitude, latitude, dti = NA) {
  ## with error propagation: timbRecoveryUncert
  
  if (length(longitude) != length(latitude) )
    stop( "Coordinates should have the same size.")
  
  if (!(length(timeLength) %in% c(1, length(longitude))) )
    stop( "timeLength should be of length 1 or have the same size as coordinates.")
  
  if (!(length(logIntensity) %in% c(1, length(longitude))) )
    stop( "logIntensity should be of length 1 or have the same size as coordinates.")
  
  if (!(length(logCycle) %in% c(1, length(longitude))) )
    stop( "logCycle should be of length 1 or have the same size as coordinates.")
  
  if (!(length(omega0) %in% c(1, length(longitude))) )
    stop( "omega0 should be of length 1 or have the same size as coordinates.")
  
  if (!(length(dti) %in% c(1, length(longitude))) )
    stop( "dti should be of length 1 or have the same size as coordinates.")
  
  load("variables and parameters/paramStan_maxL.Rdata")
  load("variables and parameters/FORMINDVariables_maxL.Rdata")
  load("variables and parameters/stemMortKrig.Rdata")
  source("functions/omega_t.R")  
  source("functions/updateLoggingParams.R")
  
  spatVariables = merge(spatVariables, stemMortKrig, by = c("long","lat"))
  
  logist = function(x) 1 / ( exp(-x) + 1 )
  
  spatVariables$stem_mort = logist(spatVariables$mu_logit_sm)
  spatVariables = spatVariables[,-grep("logit",colnames(spatVariables)), with = FALSE]
  
  dfPred = data.table(long = round(longitude + 0.5) - 0.5, lat = round(latitude + 0.5) - 0.5)
  
  if (nrow(merge(dfPred, spatVariables, by=c("long","lat"))) < nrow(dfPred))
    stop(paste(nrow(dfPred) - nrow(merge(dfPred, spatVariables, by=c("long","lat"))), 
               "locations are outside the Amazon region."))
  
  dfPred = merge(dfPred, spatVariables, by=c("long","lat"))
  
  aM = dfPred$aG_FORMIND - dfPred$vmax_FORMIND*pars_Vrec$theta
  ##!! problem: aM < 0
  pdef = 6/20 ### rbeta(1, 6, 14)
  
  ## initial maturity 
  if (is.na(dti)) 
    sample(pars_dti, nrow(dfPred))
  matInit = dfPred$stem_mort^(-pars_Vrec$lambda_ti)*( 1 - dti)
  
  ## number of cycles to reach timeLength
  nCycles = ceiling(timeLength / logCycle)
  
  matPreLog = matrix(matInit, ncol = 1)
  omPreLog = matrix(omega0, ncol = 1, nrow = length(matInit))
  omPostLog = c()
  omRecruits = c()
  
  for (i in 1:nCycles) {
    
    ## update stand parameters (maturity, proportion of commercial species) after logging
    newParams = updateLoggingParams(mat0 = matPreLog[,i], omega0 = omPreLog[,i], logIntensity = logIntensity, 
                                    aG = dfPred$aG, aM = aM, bG = pars_Vrec$bP, bM = pars_Vrec$bM, 
                                    theta = pars_Vrec$theta, pdef = pdef, psi = pars_rho$rho, e = pars_rho$e)
    
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
    omT = apply(cbind(logCycle, newParams[[1]],  newParams[[2]], omRecruits[,i], dfPred$aG, aM, 
                      pars_Vrec$bP, pars_Vrec$bM, pars_Vrec$theta, pars_pR$intercept, pars_pR$slope), 1, omega_t)
    
    matPreLog = cbind(matPreLog, newParams[[1]] + logCycle)
    omPreLog = cbind(omPreLog, omT)
    omPostLog = cbind(omPostLog, newParams[[2]])
    
  }
  
  if (length(logCycle == 1)) 
    logCycle = rep(logCycle, length(longitude))
  
  lapply(1:length(longitude), function(i) {
    t = 1:timeLength
    mat = c(matPreLog[i,1], sapply(matPreLog[i,-1], function(x) x - logCycle[i]:1) )
    volume(mat, ag = dfPred$aG_FORMIND[i], am = aM[i], bg = pars_Vrec$bP, bm = pars_Vrec$bM, th = pars_Vrec$theta, pdef)
  })
  
}