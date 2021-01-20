## when parameter "area" is provided, the function calculates logging trajectories as the sum of all
## sites' outputs (timber volume, extracted volume) multiplied by their area

timbRecovery = function(timeLength,
                        logIntensity,
                        logCycle,
                        omega0,
                        longitude,
                        latitude,
                        dti = NA,
                        uncertainties = TRUE,
                        area = NA
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
  file.sources <- list.files(path = "R/", pattern="*.R", full.names = TRUE)
  sapply(file.sources,source,.GlobalEnv)
  
  ## prepare prediction data frame
  variables <- data.frame(site = 1:length(longitude), 
                          longitude, latitude, logIntensity, logCycle, omega0)
  
  dfPred <- spatialPars(variables[, c("longitude", "latitude")], uncert = uncertainties)
  dfPred <- merge(dfPred, variables, by = "site")
  
  load("data/paramStan_maxL.Rdata")
  ## initial maturity
  if (any(is.na(dti))) {
    load("data/paramStan.Rdata")
    if (uncertainties) {
      dfPred$dti <- sample(pars_dti, nrow(dfPred), replace = TRUE)
    } else {
      dfPred$dti <- mean(pars_dti)
    }
  } 
  dfPred[, matInit := stem_mort ^ (-lambda_ti) * (1 - dti)]
  
  ## number of cycles to reach timeLength
  nCycles <- max(ceiling(timeLength / logCycle))
  
  matPreLog <- matrix(dfPred$matInit, ncol = 1)
  omPreLog <- matrix(dfPred$omega0, ncol = 1)
  omPostLog <- c()
  vPreLog <- c()
  vPostLog <- c()
  omRecruits <- c()
  vextReal <- c()
  
  for (i in 1:nCycles) {
    updatedLogIntens <- dfPred$logIntensity
    
    ## update stand parameters (maturity, proportion of commercial species) after logging
    newParams <- updateLoggingParams(
      mat0 = matPreLog[, i],
      matInit = matPreLog[, 1],
      omega0 = omPreLog[, i],
      logIntensity = updatedLogIntens,
      aG = dfPred$aG,
      aM = dfPred$aM,
      bG = dfPred$bP,
      bM = dfPred$bM,
      theta = dfPred$theta,
      pdef = dfPred$pdef,
      psi = dfPred$rho,
      e = dfPred$e, 
      uncertainties
    )
    vextReal <- cbind(vextReal, newParams[[3]])
    
    ## update the proportion of commercial recruits
    omR <- omPreLog[, i]
    ## prelogging proportion of commercial trees < proportion of commercial species in recruited trees < initial proportion of commercial species
    omR[omPreLog[, i] <= omPreLog[, 1]] <-
      runif(sum(omPreLog[, i] <= omPreLog[, 1]),
            min = omPreLog[omPreLog[, i] <= omPreLog[, 1], i],
            max = omPreLog[omPreLog[, i] <= omPreLog[, 1], 1])
    # in some cases om > omR (due probably to the discretization of om calculation, but difference is not very important)
    omR[omPreLog[, i] > omPreLog[, 1]] <-
      runif(sum(omPreLog[, i] > omPreLog[, 1]),
            min = omPreLog[omPreLog[, i] > omPreLog[, 1], 1],
            max = omPreLog[omPreLog[, i] > omPreLog[, 1], i])
    omRecruits = cbind(omRecruits, omR)
    
    ## simulate the proportion of recruits after logging
    omT <- apply(
      cbind(
        dfPred$logCycle,
        newParams[[1]],
        newParams[[2]],
        omRecruits[, i],
        dfPred$aG,
        dfPred$aM,
        dfPred$bP,
        dfPred$bM,
        dfPred$theta,
        dfPred$intercept,
        dfPred$slope
      ),
      1,
      omega_t
    )
    
    matPreLog <- cbind(matPreLog, newParams[[1]] + dfPred$logCycle)
    omPreLog <- cbind(omPreLog, omT)
    omPostLog <- cbind(omPostLog, newParams[[2]])
    
  }
  
  ### predTime: trajectories
  predTime <- merge(dfPred, data.table::data.table(expand.grid(
    iter = unique(dfPred$iter),
    site = 1:length(longitude),
    t = 0:timeLength
  )), by = c("iter", "site"))
  
  predTime[, ncycle := ceiling(t / logCycle)] ## nb of the logging cycle
  predTime[, tPL := t - floor((t - 1) / logCycle) * logCycle] ## time since last logging event
  
  ## cycle-specific parameters -- maturity
  dfmatPreLog <- data.table::data.table(matPreLog)
  colnames(dfmatPreLog) = as.character(0:(nCycles))
  dfmatPreLog$site = dfPred$site
  dfmatPreLog$iter = dfPred$iter
  dfmatPreLog = data.table::melt(
    dfmatPreLog,
    id.vars = c("iter", "site"),
    variable.name = "ncycle",
    value.name = "matFinal",
    variable.factor = FALSE
  )
  dfmatPreLog[, ncycle := as.numeric(ncycle)]
  predTime <- merge(predTime, dfmatPreLog, by = c("iter", "site", "ncycle"))
  
  ## get maturity
  predTime[, mat := matFinal - logCycle + tPL]
  
  ## cycle-specific parameters -- omega : proportion of commercial volume
  
  ## post-logging omega value
  dfomPostLog <- data.table::data.table(0, omPostLog)
  colnames(dfomPostLog) = as.character(0:nCycles)
  dfomPostLog$site = dfPred$site
  dfomPostLog$iter = dfPred$iter
  dfomPostLog = data.table::melt(
    dfomPostLog,
    id.vars = c("iter", "site"),
    variable.name = "ncycle",
    value.name = "omegaPL",
    variable.factor = FALSE
  )
  dfomPostLog[, ncycle := as.numeric(ncycle)]
  predTime <- merge(predTime, dfomPostLog, by = c("iter", "site", "ncycle"))
  
  ## pre-logging value
  dfomPreLog <- data.table::data.table(omPreLog)
  colnames(dfomPreLog) <- as.character(0:(nCycles))
  dfomPreLog$site <- dfPred$site
  dfomPreLog$iter <- dfPred$iter
  dfomPreLog <- data.table::melt(
    dfomPreLog,
    id.vars = c("iter", "site"),
    variable.name = "ncycle",
    value.name = "omegaFinal",
    variable.factor = FALSE
  )
  dfomPreLog[, ncycle := as.numeric(ncycle)]
  predTime <- merge(predTime, dfomPreLog, by = c("iter", "site", "ncycle"))
  
  ## get omega
  predTime[, omega := omegaPL + (tPL) * (omegaFinal - omegaPL) / (logCycle)]
  
  ## calculate volume
  predTime[, volume := volume(mat, aG_FORMIND, aM, bP, bM, theta, pdef)]
  
  if (length(area) == 1 & sum(is.na(area)) == 1) {
    if (uncertainties) {
      volInterval = predTime[, .(
        inf = quantile(volume * omega, 0.025),
        med = quantile(volume * omega, 0.5),
        sup = quantile(volume * omega, 0.975)
      ) , .(t, site)]
    } else {
      predTime[, volTimb := volume * omega]
      volInterval = predTime[, c("t", "site", "volTimb")]
    }
    
    dfVextReal <- data.table::data.table(vextReal)
    colnames(dfVextReal) <- as.character(1:nCycles)
    dfVextReal$site <- dfPred$site
    dfVextReal$iter <- dfPred$iter
    dfVextReal <- data.table::melt(
      dfVextReal,
      id.vars = c("iter", "site"),
      variable.name = "ncycle",
      value.name = "vextReal"
    )
    dfVextReal[, ncycle := as.numeric(ncycle)]
    if (uncertainties) {
      dfVextReal = dfVextReal[, .(
        inf = quantile(vextReal, 0.025),
        med = quantile(vextReal, 0.5),
        sup = quantile(vextReal, 0.975)
      ) , .(ncycle, site)]
    }
    
  } else {
    predTime <- merge(predTime, data.table::data.table(site = 1:length(longitude), area), by = "site")
    
    if (uncertainties) {
      volInterval <- predTime[, .(
        inf = quantile(sum(volume * omega * area), 0.025),
        med = quantile(sum(volume * omega * area), 0.5),
        sup = quantile(sum(volume * omega * area), 0.975)
      ) , .(t)]
    } else {
      predTime[, .(volTimb = sum(volume * omega * area)), .(t)]  ## xxx maybe add here a silviculture module
      volInterval = predTime[, c("t", "volTimb")]
    }
    
    dfVextReal <- data.table::data.table(vextReal)
    colnames(dfVextReal) = as.character(1:nCycles)
    dfVextReal$site = dfPred$site
    dfVextReal$iter = dfPred$iter
    dfVextReal = merge(dfVextReal, data.table::data.table(site = 1:length(longitude), area), by = "site")
    
    dfVextReal <- data.table::melt(
      dfVextReal,
      id.vars = c("iter", "site", "area"),
      variable.name = "ncycle",
      value.name = "vextReal"
    )
    dfVextReal[, ncycle := as.numeric(ncycle)]
    if (uncertainties) {
      dfVextReal = dfVextReal[, .(
        inf = quantile(sum(vextReal * area), 0.025),
        med = quantile(sum(vextReal * area), 0.5),
        sup = quantile(sum(vextReal * area), 0.975)
      ) , .(ncycle)]
    } else {
      dfVextReal = dfVextReal[, .(vextReal = sum(vextReal * area)) , .(ncycle)]
    }
    
  }
  
  return(list(volInterval, dfVextReal))
}

logit = function(x){
  log(x / (1 - x))}
logist = function(x){
  1 / (exp(-x) + 1)}