volume = function(t, ag , am, bg, bm, th, pdef=0) { 
  (ag/th * (1 - (th*exp(-1*bg*t) - bg*exp(-th*t))/(th - bg)) - am/th*(1 - (th*exp(-1*bm*t) - bm*exp(-th*t))/(th - bm)))*(1-pdef) 
}