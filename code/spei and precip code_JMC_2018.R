#### 2018 Synth Reboot
#### Precip Metrics
#### Jane M Cowles
#### 11 Dec 2018


#### functions to use

#far to celsius
# (32°F − 32) × 5/9 = 0°C

fahrtocels <- function(x){(x-32)*(5/9)}
inchtomm <- function(x){x*25.4}

#### Calculate SPEI if I can!!! So I can use up to 2018
### from https://cran.r-project.org/web/packages/SPEI/SPEI.pdf

setwd("~/Documents/SYNTH REBOOT")

library(SPEI)
library(data.table)

clim <- read.csv("spei and precip data/e080_Daily climate summary.csv")
str(clim)

#make year and month columns
##Make date column a date
clim$Date<-as.Date(clim$Date,"%m/%d/%y")
#fix the dates that say they are e.g. 2062 -- these should be in the 1900s.
clim$Date<-as.Date(ifelse(clim$Date>Sys.Date(),format(clim$Date,"19%y-%m-%d"),format(clim$Date)))

clim$Year <- as.numeric(format(clim$Date,'%Y'))
clim$Month <- as.numeric(format(clim$Date,'%m'))

clim$MaxT <- fahrtocels(clim$MaxTemp.degF.)
clim$MinT <- fahrtocels(clim$MinTemp.degF.)
clim$MeanT <- (clim$MaxT+clim$MinT)/2
clim$Prec <- inchtomm(clim$Precip.inches.)
setDT(clim)
clim.mo <- clim[,.(MaxT=mean(MaxT),MinT=mean(MinT),MeanT=mean(MeanT),Prec=mean(Prec)),by=.(Year,Month)]


clim.mo$PET <- thornthwaite(clim.mo$MeanT,lat=45.401)
plot(clim.mo$PET)
clim.mo$BAL <- clim.mo$Prec - clim.mo$PET



