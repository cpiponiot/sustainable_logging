
t0Prediction <- function(X){ 
  
  X[1] -> ti; X[2] -> deltaV; X[3] -> aG; X[4] -> aM; X[5] -> bP; X[6] -> bM; X[7] -> theta; X[8] -> pdef
  
  equ = function(x) volume(x,aG,aM,bP,bM,theta,pdef) - (volume(ti,aG,aM,bP,bM,theta,pdef) - deltaV)
  
  return(uniroot(equ,c(0,ti))$root) 
  
}