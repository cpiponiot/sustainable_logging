# test new method

source("R/simulateLogging.R")


## test that simulateLogging works with one set of inputs
timeLength = 100
logIntensity = 20
logCycle = 35
omega0 = 0.5
longitude = -60.6
latitude = -3.3
dti = NA
uncertainties = TRUE
area = NA
silv = c(0,0)
keepAll = FALSE

result = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result), ceiling(timeLength/logCycle)*(2*2 + 1))

## test that simulateLogging works when uncertainties = FALSE
timeLength = 100
logIntensity = 20
logCycle = 35
omega0 = 0.5
longitude = -60.6
latitude = -3.3
dti = NA
uncertainties = FALSE
area = NA
silv = c(0,0)
keepAll = FALSE
result2 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result2), ceiling(timeLength/logCycle)*(2*2 + 1))


## test that simulateLogging works when keepAll = TRUE
timeLength = 100
logIntensity = 20
logCycle = 35
omega0 = 0.5
longitude = -60.6
latitude = -3.3
dti = NA
uncertainties = TRUE
area = NA
silv = c(0,0)
keepAll = TRUE

result3 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result3), ceiling(timeLength/logCycle)*(2*logCycle + 1))

## test that simulateLogging works with multiple sets of inputs
timeLength = 100
logIntensity = rep(10, 4); logIntensity[2] = 30
logCycle = rep(35, 4); logCycle[3] = 65
omega0 = rep(0.90, 4); omega0[4] = 0.30
longitude = rep(-60.6, 4)
latitude = rep(-3.3, 4)
dti = NA
uncertainties = TRUE
area = NA
silv = c(0,0)
keepAll = FALSE

result4 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result4), max(ceiling(timeLength/logCycle))*length(logCycle)*(2*2 + 1))

## test that simulateLogging works with multiple sets of inputs and keepAll = TRUE
timeLength = 100
logIntensity = rep(10, 4); logIntensity[2] = 30
logCycle = rep(35, 4); logCycle[3] = 65
omega0 = rep(0.90, 4); omega0[4] = 0.30
longitude = rep(-60.6, 4)
latitude = rep(-3.3, 4)
dti = NA
uncertainties = TRUE
area = NA
silv = c(0,0)
keepAll = TRUE

result5 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result5), max(ceiling(timeLength/logCycle))*sum(2*logCycle + 1))


## test that simulateLogging works with multiple sets of inputs and uncertainties = FALSE
timeLength = 100
logIntensity = rep(10, 4); logIntensity[2] = 30
logCycle = rep(35, 4); logCycle[3] = 65
omega0 = rep(0.90, 4); omega0[4] = 0.30
longitude = rep(-60.6, 4)
latitude = rep(-3.3, 4)
dti = NA
uncertainties = FALSE
area = NA
silv = c(0,0)
keepAll = FALSE

result6 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result6), max(ceiling(timeLength/logCycle))*length(logCycle)*(2*2 + 1))

## test that simulateLogging works with multiple sets of inputs and uncertainties = FALSE and keepAll = TRUE
timeLength = 100
logIntensity = rep(10, 4); logIntensity[2] = 30
logCycle = rep(35, 4); logCycle[3] = 65
omega0 = rep(0.90, 4); omega0[4] = 0.30
longitude = rep(-60.6, 4)
latitude = rep(-3.3, 4)
dti = NA
uncertainties = FALSE
area = NA
silv = c(0,0)
keepAll = TRUE

result7 = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
testthat::expect_equal(nrow(result7), max(ceiling(timeLength/logCycle))*sum(2*logCycle + 1))


## test that we get same results with analystical solution used in `timbRecovery()`
timeLength = 100
logIntensity = 20
logCycle = 35
omega0 = 0.5
longitude = -60.6
latitude = -3.3
dti = NA
uncertainties = TRUE
area = NA
silv = c(0,0)
keepAll = TRUE

result = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
resultbis = timbRecovery(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area)
vcomm1 = result$med[result$variable=="volume"][1:100]*result$med[result$variable=="omega"][1:100]
vcomm2 = resultbis[[1]]$med[2:101]

testthat::expect_true(all(abs(vcomm1-vcomm2) < 2))

plot(y = vcomm1,x = 1:100, 
     xlab = "time", ylab = "Commercial volume", 
     type = "l", ylim = c(0,40), col = 2) 
lines(x = 1:100, y = vcomm2)

