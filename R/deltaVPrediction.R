deltaVPrediction <- function(V0, Vext, om0, psi, e){
  
  # get ome (proportion of commercial volume in volume loss due to logging operations)
  
  mu <- om0^(1-psi)
  Var <- (1-mu)*mu^2*e
  
  alpha <- (mu/Var - 1/mu) * mu^2 
  beta <- alpha * (1/mu - 1) 
  
  if (uncertainties) {
    ome <- apply(cbind(alpha,beta,om0), 1, function(X) {
      if (X[3]>0 & X[3]<1) {
        truncdist::rtrunc(1, "beta", shape1 = X[1], shape2 = X[2], a = X[3])
      } else if (X[3]>1) {
        1 
      } else 0
    })
  } else {
    ome <- mu
  }
  
  deltaV <- apply(cbind(Vext/ome,V0), 1, min)
  deltaV[om0==0] <- 0
  deltaV[om0==1] <- Vext[om0==1]
  return(deltaV)
}