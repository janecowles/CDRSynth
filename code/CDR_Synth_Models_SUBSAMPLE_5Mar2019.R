### JMC MODEL CODE
### CDR SYNTHESIS -- 5 March 2019
### with 2018 data

rm(list=ls())
graphics.off()


library(plyr)
library(nlme)
library(lme4)
library(car)
library(ggplot2)
library(vegan)
library(gridExtra)
library(ggeffects)

##Set working directory 
setwd("~/Documents/SYNTH REBOOT/") #r


#### GROWING SEASON

GSe120.154 <- read.csv("Processed Data/GSe120.154_clean.csv")
GSe120.154 <- GSe120.154[GSe120.154$Month!=6,] #whoops I left June 2001 in there. No june data.
GSe120.154$Plot <- as.factor(GSe120.154$Plot)
GSe120.154.bcy <- GSe120.154[GSe120.154$Year %in% c(1998:2015),]
GSe120.154.bcy$Year <- factor(GSe120.154.bcy$Year)

GSe120.154$invCDD_Y <- -1*GSe120.154$CDD_Y

GSe120.154$scaletotal_precip_mm_Y<- scale(GSe120.154$total_precip_mm_Y)
GSe120.154$scaleSPEI_4<- scale(GSe120.154$SPEI_4)



GSe1e2 <- read.csv("Processed Data/GSe1e2_clean.csv")
GSe1e2$FieldExpPlot <- as.factor(GSe1e2$FieldExpPlot)
GSe1e2$invCDD_Y <- -1*GSe1e2$CDD_Y

GSe1e2$scaletotal_precip_mm_Y<- scale(GSe1e2$total_precip_mm_Y)
GSe1e2$scaleSPEI_4<- scale(GSe1e2$SPEI_4)

GSe141 <- read.csv("Processed Data/GSe141_clean.csv")
GSe141$Plot <- as.factor(GSe141$Plot)
GSe141$Ring <- as.factor(GSe141$Ring)

GSe141$invCDD_Y <- -1*GSe141$CDD_Y

GSe141$BURN <- as.factor(ifelse(GSe141$Year %in% c(2000, 2002, 2003, 2005, 2007, 2009, 2011, 2012, 2013:2017),"Burned","NotBurned"))


GSe141$scaletotal_precip_mm_Y<- scale(GSe141$total_precip_mm_Y)
GSe141$scaleSPEI_4<- scale(GSe141$SPEI_4)

##############################################################################################################
###################      end of prep, on to analyses       ###################################################
##############################################################################################################
###################################      end of prep, on to analyses       ###################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

##########################################################################
##########################################################################
##########################################################################
###############          subsamples             ##########################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
par(mfrow=c(2,2))
e120Out <- NULL
for(i in 1:max(GSe120.154$ExpYear)){
  removeyr <- GSe120.154[GSe120.154$ExpYear!=i,]
  print(levels(factor(removeyr$ExpYear)))
  mod1 <- lme(sqrt(AGB)~logDiv * scaletotal_precip_mm_Y + logDiv * ExpYear, random=~1|Plot,data=removeyr,na.action=na.exclude,method="ML")
  e120Out <- rbind(e120Out,c(i,coef(summary(mod1))[5,1],coef(summary(mod1))[5,2]))
}
  
modfull <- lme(sqrt(AGB)~logDiv * scaletotal_precip_mm_Y + logDiv * ExpYear, random=~1|Plot,data=GSe120.154,na.action=na.exclude,method="ML")
height <- coef(summary(modfull))[5,1]
heightup <- coef(summary(modfull))[5,1]+coef(summary(modfull))[5,2]
heightdown <- coef(summary(modfull))[5,1]-coef(summary(modfull))[5,2]

plot(e120Out[,1],e120Out[,2],xlim=c(0,24),ylim=c(0,.25),type="n",main="e120",xlab="year removed",ylab="trt*precip effect")
polygon(c(-1,25,25,-1),c(heightup,heightup,heightdown,heightdown),col="orange")
abline(h=height,col="red",lwd=3)
points(e120Out[,1],e120Out[,2])
arrows(e120Out[,1], e120Out[,2]-e120Out[,3], e120Out[,1], e120Out[,2]+e120Out[,3], length=0.05, angle=90, code=3)

e1e2Out <- NULL
for(i in as.numeric(levels(factor(GSe1e2$ExpYear)))){
  removeyr <- GSe1e2[GSe1e2$ExpYear!=i,]
  print(levels(factor(removeyr$ExpYear)))
  mod1 <- lme(sqrt(AGB)~Field+Experiment+NAdd * scale(total_precip_mm_Y) + NAdd * ExpYear, random=~1|FieldExpPlot,data=removeyr,na.action=na.exclude,method="ML")
  e1e2Out <- rbind(e1e2Out,c(i,coef(summary(mod1))[5,1],coef(summary(mod1))[5,2]))
}
  
modfull <- lme(sqrt(AGB)~Field+Experiment+NAdd * scale(total_precip_mm_Y) + NAdd * ExpYear, random=~1|FieldExpPlot,data=GSe1e2,na.action=na.exclude,method="ML")
height <- coef(summary(modfull))[5,1]
heightup <- coef(summary(modfull))[5,1]+coef(summary(modfull))[5,2]
heightdown <- coef(summary(modfull))[5,1]-coef(summary(modfull))[5,2]

plot(e1e2Out[,1],e1e2Out[,2],xlim=c(0,38),ylim=c(0.15,.25),type="n",main="e1 and e2",xlab="year removed",ylab="trt*precip effect")
polygon(c(-1,40,40,-1),c(heightup,heightup,heightdown,heightdown),col="orange")
abline(h=height,col="red",lwd=3)
points(e1e2Out[,1],e1e2Out[,2])
arrows(e1e2Out[,1], e1e2Out[,2]-e1e2Out[,3], e1e2Out[,1], e1e2Out[,2]+e1e2Out[,3], length=0.05, angle=90, code=3)

e141Out <- NULL
for(i in 1:(max(GSe141$ExpYear))){
  removeyr <- GSe141[!GSe141$ExpYear%in%c(i),]
  print(levels(factor(removeyr$ExpYear)))
  mod1 <- lme(sqrt(AGB)~NTrt * scale(total_precip_mm_Y) + logDiv * scale(total_precip_mm_Y) + NTrt * ExpYear + logDiv * ExpYear, random=~1|Ring/Plot,data=removeyr,na.action=na.exclude,method="ML")
  e141Out <- rbind(e141Out,c(i,coef(summary(mod1))[6,1],coef(summary(mod1))[6,2],coef(summary(mod1))[7,1],coef(summary(mod1))[7,2]))
}
  
modfull <- lme(sqrt(AGB)~NTrt * scale(total_precip_mm_Y) + logDiv * scale(total_precip_mm_Y) + NTrt * ExpYear + logDiv * ExpYear, random=~1|Ring/Plot,data=GSe141,na.action=na.exclude,method="ML")
heightN <- coef(summary(modfull))[6,1]
heightNup <- coef(summary(modfull))[6,1] + coef(summary(modfull))[6,2]
heightNdown <- coef(summary(modfull))[6,1] - coef(summary(modfull))[6,2]
heightDiv <- coef(summary(modfull))[7,1]
heightDivup <- coef(summary(modfull))[7,1] + coef(summary(modfull))[7,2]
heightDivdown <- coef(summary(modfull))[7,1] - coef(summary(modfull))[7,2]





plot(e141Out[,1],e141Out[,2],xlim=c(0,22),ylim=c(0,.5),type="n",main="e141 N",xlab="year removed",ylab="trt*precip effect")
polygon(c(-1,24,24,-1),c(heightNup,heightNup,heightNdown,heightNdown),col="orange")
points(e141Out[,1],e141Out[,2])
arrows(e141Out[,1], e141Out[,2]-e141Out[,3], e141Out[,1], e141Out[,2]+e141Out[,3], length=0.05, angle=90, code=3)
abline(h=heightN,col="red",lwd=3)

plot(e141Out[,1],e141Out[,4],xlim=c(0,22),ylim=c(-0.1,.2),type="n",main="e141 Div",xlab="year removed",ylab="trt*precip effect")
polygon(c(-1,24,24,-1),c(heightDivup,heightDivup,heightDivdown,heightDivdown),col="orange")
points(e141Out[,1],e141Out[,4])
arrows(e141Out[,1], e141Out[,4]-e141Out[,5], e141Out[,1], e141Out[,4]+e141Out[,5], length=0.05, angle=90, code=3)

abline(h=heightDiv,col="red",lwd=3)
