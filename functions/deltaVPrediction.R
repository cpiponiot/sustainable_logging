
library(truncdist)

deltaVPrediction <- function(V0, Vext, om0, rho, e){

  # get ome (proportion of commercial volume in volume loss due to logging operations)
  mu = om0^(1-rho); Var = (1-mu)*mu^2*e; 
  alpha =   (mu/Var - 1/mu) * mu^2;beta = alpha * (1/mu - 1);
  ome = apply(cbind(alpha,beta,om0), 1, function(X) {
    if (X[3]>0 & X[3]<1) rtrunc(1,"beta",shape1=X[1],shape2=X[2],a=X[3]) else if (X[3]>1) 1 else 0
  })
  
  deltaV = apply(cbind(Vext/ome,V0), 1, min)
  deltaV[om0==0] <- 0
  return(deltaV)
}