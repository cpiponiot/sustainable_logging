## test discretization volume
library(data.table)
library(ggplot2)

## parameters
tmax = 100

ag = 5
bg = 0.01
am = 4
bm = 0.005
theta = 0.002
intpR = 5
slopepR = -1.5
om0 = 0.8


# test different silvilcultural effects
## increase in growth of commercial trees
silv_effect = list(rep(1, tmax), ## no effect
                   rep(1.1, tmax), ## 10%
                   rep(1.2, tmax), ## 20%
                   rep(1.5, tmax), ## 50%
                   c(rep(1.5, 15), rep(1.5, 5) - 0.1*0:4, rep(1, 80)) ##50% with decrease after 15 years
)
names(silv_effect) <- c("none", "10%", "20%", "50%", "50% with decrease")

simulations <- lapply(silv_effect, function(K) {
  # initialization
  V = c(100) 
  om = c(0.2) 
  vtest = volume(t = 1:500, ag = ag, am = am, bg = bg, bm = bm, th = theta)
  t0_test = which.min(abs(vtest-100))
  for (t in 1:(tmax-1)) {
    pR = 1 / (1 + exp(-(intpR + slopepR * log(V[t]))))
    eta = pR * (om0*K[t] + 1 - om0) + (1 - pR) * (om[t]*K[t] + 1 - om[t]) 
    g = (ag*(1-exp(-bg*(t0_test+t))) - theta*V[t])*eta
    m = am*(1-exp(-bm*(t0_test+t)))
    V[t+1] = V[t] + g - m
    om[t+1] = min((V[t]*om[t] + (ag*(1-exp(-bg*(t0_test+t))) - theta*V[t]) * K[t] * (om0*pR + om[t]*(1-pR)) - m*om[t] ) / V[t+1], 1)
  }
  return(data.frame(t = 1:tmax, silv = (K-1)*100, vol = V, omega = om*100))
})

simulations <- rbindlist(simulations, idcol = "treatment")
simulations[, vcom := vol*omega/100]
dfsim <- melt(simulations, measure.vars = c("silv", "vol", "omega", "vcom"))
levels(dfsim$variable) <- c("Effect of silvicultural treatments\non commercial volume growth (%)", 
                                  "Total volume (m3/ha)", "Proportion of commercial volume (%)", "Commercial volume (m3/ha)")

## graph
library(ggplot2)
ggplot(dfsim, aes(x = t, y = value, color = treatment)) + 
  geom_line() +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() + 
  expand_limits(x = 0, y = 0) + 
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Recovery time (yr)", y = "")


## compare with analytic results under hypothesis that omega(t) does not depend on total volume 

simulations[, pR := 1 / (1 + exp(-(intpR + slopepR * log(vol))))]
simulations[, eta := pR * (om0*(silv/100 + 1) + 1 - om0) + (1 - pR) * (omega/100*(silv/100 + 1) + 1 - omega/100)]
simulations[, vbis := volume(t = t0_test+t-1, ag = ag*eta, am = am, bg = bg, bm = bm, th = theta*eta)]

ggplot(simulations, aes(x = vol, y = vbis, color = treatment)) + 
  geom_line() + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

## analytic solution doesn't work, we need to discretize the function
## BUT when K=1 (no silviculture effect) we get the same results as the analytic version -> good sign