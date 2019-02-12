updateLoggingParams <- function(mat0, omega0, logIntensity, aG, aM, bG, bM, theta, pdef, psi, e) {

  source("functions/volume.R")
  source("functions/deltaVPrediction.R")
  source("functions/t0Prediction.R")
  source("functions/omega_t.R")
  
  volume0 = volume(mat0, aG, aM, bG, bM, theta, pdef)
  
  vextReal = apply(cbind(volume0*omega0, logIntensity), 1, min)
  
  ## total volume loss
  ## / pdef ... * pdef : all trees can be damaged (with or without defects)
  deltaV <- deltaVPrediction(volume0 / (1-pdef), vextReal, omega0, psi, e) * (1 - pdef)
  
  ## post logging proportion of commercial species
  omega1 = ( omega0 * volume0 - vextReal ) / ( volume0 - deltaV )
  
  ## if deltaV = V0 (ie all trees > 50 cm have been removed) : reset to om0
  omega1[volume0 == deltaV] <- omega0[volume0 == deltaV]
  
  # post-logging maturity
  mat1 <- apply(cbind(mat0, deltaV, aG, aM, bG, bM, theta, pdef), 1, t0Prediction)
  
  return(list(mat1, omega1, vextReal))
}