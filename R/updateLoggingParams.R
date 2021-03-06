updateLoggingParams <- function(mat0,
                                matInit,
                                omega0,
                                logIntensity,
                                aG,
                                aM,
                                bG,
                                bM,
                                theta,
                                pdef,
                                psi,
                                e, 
                                uncertainties) 
{
  source("R/volume.R")
  source("R/deltaVPrediction.R")
  source("R/t0Prediction.R")
  source("R/omega_t.R")
  
  volume0 = volume(mat0, aG, aM, bG, bM, theta, pdef)  ## proposition: transformer omega en omega * (1-pdef) ou faire un truc similaire a omega avec pdef
  
  vextReal = apply(cbind(volume0 * omega0, logIntensity), 1, min)
  
  ## total volume loss
  ## / pdef ... * pdef : all trees can be damaged (with or without defects)
  deltaV <-
    deltaVPrediction(
      V0 = volume0 ,
      Vext = vextReal,
      om0 = omega0,
      psi = psi,
      e = e, 
      uncertainties = uncertainties
    )
  
  ## post logging proportion of commercial species
  omega1 = (omega0 * volume0 - vextReal) / (volume0  - deltaV)
  
  ## if deltaV = V0 (ie all trees > 50 cm have been removed) : reset to om0
  omega1[volume0 == deltaV] <- omega0[volume0 == deltaV]
  
  # post-logging maturity
  mat1 <- apply(cbind(mat0, deltaV, aG, aM, bG, bM, theta, pdef), 1, t0Prediction)
  
  mat1 <- apply(cbind(mat1, matInit), 1, min) ## maturity cannot be higher than the initial value
  
  return(list(mat1, omega1, vextReal))
}