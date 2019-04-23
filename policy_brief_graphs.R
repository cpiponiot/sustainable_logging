##################################################################
#############     graphs for the policy brief     ################
##################################################################

### load libraries
library(data.table)
library(ggplot2)

### load functions
source("functions/volume.R")
source("functions/timbRecovery.R")


### graph 1

timeLength = 1000

coordinates = data.table(siteName = c("Itoupe", "RioBranco", "Iquitos", "Manaus"), 
                         longitude = c(-53.6, -68, -73.4, -60.6), 
                         latitude = c(3.5, -10, -3.75, -3.3))
tabScenario = data.table(expand.grid(omega0 = c(0.3, 0.9), 
                                     logIntensity = seq(2,20,2),
                                     siteName = coordinates$siteName))
tabScenario = merge(tabScenario, coordinates, by = "siteName") 

recov = timbRecovery(timeLength = timeLength, logIntensity = tabScenario$logIntensity, 
                     logCycle = 35, omega0 = tabScenario$omega0, 
                     longitude = tabScenario$longitude, latitude = tabScenario$latitude, 
                     uncertainties = TRUE, firstIntensity = 20)
dfrecov = recov[[1]]
tabScenario$site = 1:nrow(tabScenario)
dfrecov = merge(dfrecov, tabScenario, by = "site")

## productivity
dfprod = merge(recov[[2]], tabScenario, by = "site")
dfprod = dfprod[ncycle>1, .(rel_extraction = mean(med/logIntensity)), .(site)]
dfrecov = subset(dfrecov, site %in% dfprod$site[dfprod$rel_extraction>0.99])
dfrecov$omega0 = factor(dfrecov$omega0)
levels(dfrecov$omega0) = paste0("omega0 = ", c(30,90), "%")

ggplot(dfrecov, aes(x=t, y = med, color = as.factor(logIntensity), ymin = inf, ymax = sup)) + 
  geom_ribbon(aes(fill = as.factor(logIntensity)), colour = NA, alpha = 0.1) + geom_line() + 
  theme(panel.background = element_rect(fill = 'white', colour="black")) + 
  xlab("Time since first logging event (yr)") + ylab("Commercial timber volume (m3/ha)")  + 
  labs(fill = "Logging intensity (m3/ha)", colour = "Logging intensity (m3/ha)") +
  facet_grid(omega0 ~ siteName, scales = "free") 
ggsave("graphs/PB_fig1_v1.pdf", height = 4, width = 12)
