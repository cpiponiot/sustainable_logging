#' Simulate post-logging recovery of timber volume
#'
#' This function creates S3 objects of class "numeric".
#'
#' @param vol1
#' @param om1
#' @param logCycle length of the logging cycle
#' @param ag Growth parameter 1
#' @param am Mortality parameter 2
#' @param bg Growth parameter 2
#' @param am Mortality parameter 2
#' @param theta Respiration factor
#' @param intR Intercept of recruits model
#' @param sloR Slope of recruits model
#' @param omR Proportion of commercial volume in recruits
#' @param silv silvicultural treatment
#' @param keepAll keep all intermediate values. Default is FALSE: only the
#'   values at the end of the logging cycle are kept
#'
#' @return A data frame of spatial parameters (columns)
#'
#' @export
#' 
recovery <- function (vol1,
                      om1,
                      logCycle,
                      ag,
                      am,
                      bg,
                      bm,
                      theta,
                      intR,
                      sloR,
                      omR,
                      silv = c(0,0,0), 
                      keepAll = FALSE) {
  ## load all functions
  file.sources = list.files(path = "R/", pattern="*.R", full.names = TRUE)
  sapply(file.sources, source, .GlobalEnv)
  
  t1 <- getMaturity(vol = vol1, ag, am, bg, bm, theta)
  volume <- c(vol1) 
  omega <- c(om1) 
  
  ## silviculture effect 
  sf <- 1 + silv[1]*(1 - 1 / (exp(silv[3]*(silv[2] - (1:logCycle))+log(4))  + 1))
  
  for (j in 1:(logCycle-1)) {
    pR <- 1 / (1 + exp(-(intR + sloR * log(volume[j]))))
    eta <- pR * (omR*sf[j] + 1 - omR) + (1 - pR) * (omega[j]*sf[j] + 1 - omega[j]) 
    g <- (ag*(1-exp(-bg*(t1+j))) - theta*volume[j])*eta
    m <- am*(1-exp(-bm*(t1+j)))
    volume[j+1] <- volume[j] + (g - m)
    omega[j+1] <- min((volume[j]*omega[j] + (ag*(1-exp(-bg*(t1+j))) - theta*volume[j]) * sf[j] * (omR*pR + omega[j]*(1-pR)) - m*omega[j] ) / volume[j+1], 1)
  }
  
  results <- data.frame(trec = 1:logCycle, volume, omega)
  
  if (keepAll) {
    return(results)
  } else {
    return(results[c(1, logCycle),])
  }
}