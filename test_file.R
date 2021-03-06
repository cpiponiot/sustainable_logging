# test new method

file.sources = list.files(path = "R/", pattern="*.R", full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)


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


## test that we get same results with analytical solution used in `timbRecovery()`
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
keepAll = TRUE

result = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
resultbis = timbRecovery(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area)
vcomm1 = result$value[result$variable=="volume"][1:100]*result$value[result$variable=="omega"][1:100]
vcomm2 = resultbis[[1]]$volTimb[2:101]

testthat::expect_true(all(abs(vcomm1-vcomm2) < 2))
testthat::expect_true(all(abs(vcomm1-vcomm2) < 0.5+1/100*1:length(vcomm1)))

plot(y = vcomm1,x = 1:100, 
     xlab = "time", ylab = "Commercial volume", 
     type = "l", ylim = c(0,80), col = 2) 
lines(x = 1:100, y = vcomm2)
legend(x = "topright", col = c(1,2), lty = 1,
       legend = c("timbRecov()", "simulateLogging()"))

## with 2 different sets of logging rules
timeLength = 100
logIntensity = c(10, 20)
logCycle = c(60, 35)
omega0 = c(0.4, 0.8)
longitude = rep(-60.6, 2)
latitude = rep(-3.3, 2)
dti = NA
uncertainties = FALSE
area = NA
silv = c(0,0)
keepAll = TRUE

result = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
resultbis = timbRecovery(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area)
vcomm1 = result$value[result$variable=="volume"&result$site==1][1:100]*result$value[result$variable=="omega"&result$site==1][1:100]
vcomm2 = result$value[result$variable=="volume"&result$site==2][1:100]*result$value[result$variable=="omega"&result$site==2][1:100]
vcomm1b = subset(resultbis[[1]], site==1)$volTimb[2:101]
vcomm2b = subset(resultbis[[1]], site==2)$volTimb[2:101]

testthat::expect_true(all(abs(vcomm1-vcomm1b) < 2))
testthat::expect_true(all(abs(vcomm2-vcomm2b) < 2))

plot(y = vcomm1,x = 1:100, 
     xlab = "time", ylab = "Commercial volume", 
     type = "l", ylim = c(0,80), col = 2) 
lines(x = 1:100, y = vcomm1b)
legend(x = "topright", col = c(1,2), lty = 1,
       legend = c("timbRecov()", "simulateLogging()"))
plot(y = vcomm2,x = 1:100, 
     xlab = "time", ylab = "Commercial volume", 
     type = "l", ylim = c(0,80), col = 2) 
lines(x = 1:100, y = vcomm2b)
legend(x = "topright", col = c(1,2), lty = 1,
       legend = c("timbRecov()", "simulateLogging()"))

## with uncertainties
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

t0 = Sys.time()
result = simulateLogging(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area, silv, keepAll) 
(Sys.time()-t0)
t0 = Sys.time()
resultbis = timbRecovery(timeLength, logIntensity, logCycle, omega0, longitude, latitude,  dti, uncertainties, area)
(Sys.time()-t0)

vcomm1 = result$med[result$variable=="volume"][1:100]*result$med[result$variable=="omega"][1:100]
vinf1 = result$lwr[result$variable=="volume"][1:100]*result$lwr[result$variable=="omega"][1:100]
vsup1 = result$upr[result$variable=="volume"][1:100]*result$upr[result$variable=="omega"][1:100]

vcomm2 = resultbis[[1]]$med[2:101]
vinf2 = resultbis[[1]]$inf[2:101]
vsup2 = resultbis[[1]]$sup[2:101]

testthat::expect_true(all(abs(vcomm1-vcomm2) < 5))

plot(y = vcomm1,x = 1:100, 
     xlab = "time", ylab = "Commercial volume", 
     type = "l", ylim = c(0,80), col = 2) 
lines(x = 1:100, y = vcomm2)
polygon(x = c(1:100, 100:1), y = c(vinf1, rev(vsup1)), col = "#00000020", border = NA)
polygon(x = c(1:100, 100:1), y = c(vinf2, rev(vsup2)), col = "#FF000020", border = NA)
legend(x = "topright", col = c(1,2), lty = 1,
       legend = c("timbRecov()", "simulateLogging()"))
