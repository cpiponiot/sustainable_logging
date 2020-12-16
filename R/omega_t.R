#### Arguments ####
## logCycle: maturity vector
## t1: first maturity
## om1: first (post-logging) proportion of timber species in the total volume
## omR: proportion of timber species in recruits' volume
## ag, am, bg, bm, th: volume model parameters
## int, slope: logging precision model parameters (intercept, slope)

omega_t = function(X){
  
  logCycle = X[1]; t1 = X[2]; om1 = X[3]; omR = X[4]; ag = X[5]; am = X[6]; 
  bg = X[7]; bm = X[8]; th = X[9]; int = X[10]; slope = X[11]; 
  
  t = seq.int(t1+1,t1+max(logCycle)); ord_t = floor(logCycle - t1 + 1)
  
  V = ag/th*(1-(th*exp(-bg*t)-bg*exp(-th*t))/(th-bg))-am/th*(1-(th*exp(-bm*t)-bm*exp(-th*t))/(th-bm))
  
  dVG = ag*bg/(th-bg)*(exp(-bg*t)-exp(-th*t))+am*(1-(th*exp(-bm*t)-bm*exp(-th*t))/(th-bm))
  
  dVM = am*(1-exp(-bm*t))
  
  pR = 1/(1+exp(-(int+slope*log(V))))
  
  om = om1
  
  for (i in 1:(length(t)-1)) {
    ## !! pb: if aM < 0, dVG and dVM can be negative => om_t < 0
    om2 =min((om[i]*V[i] + dVG[i]*(pR[i]*omR + (1-pR[i])*om[i]) - dVM[i]*om[i])/V[i+1], 1)
    # if (V[i+1] == 0) om2 = omR
    om = c(om,om2)
  }
  
  return(last(om))
}