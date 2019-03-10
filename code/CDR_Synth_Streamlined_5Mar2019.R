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



#### THIS (IN CONJUNCTION WITH sink() at the end) WILL SAVE ALL THE OUTPUT TO a file in the wd.

# sink(paste("output_",Sys.Date(),".txt",sep=""))

##########################################################################
##########################################################################
##########################################################################
###############             GSe120              ##########################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################




print("e120 tot");mod1 <- lme(sqrt(AGB)~logDiv * scaletotal_precip_mm_Y + logDiv * ExpYear, random=~1|Plot,data=GSe120.154,na.action=na.exclude,method="ML");summary(mod1);print("e120 tot");Anova(mod1,type="III")
print("e120 spei");mod2 <- lme(sqrt(AGB)~logDiv * scaleSPEI_4 + logDiv * ExpYear, random=~1|Plot,data=GSe120.154,na.action=na.exclude,method="ML");summary(mod2);print("e120 spei");Anova(mod2,type="III")

mydf <- ggeffect(mod1, terms = c("logDiv","scaletotal_precip_mm_Y [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()
mydf <- ggeffect(mod2, terms = c("logDiv","scaleSPEI_4 [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

##########################################################################
##########################################################################
##########################################################################
###########            GSe1e2 (COMBINED)              ####################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################

print("e001 and e002 TOT");mod3 <- lme(sqrt(AGB)~Field+Experiment+NAdd * scaletotal_precip_mm_Y + NAdd * ExpYear, random=~1|FieldExpPlot,data=GSe1e2,na.action=na.exclude,method="ML");summary(mod3);print("e001 and e002 TOT");Anova(mod3,type="III")
print("e001 and e002 spei");mod4 <- lme(sqrt(AGB)~Field+Experiment+NAdd * scaleSPEI_4 + NAdd * ExpYear, random=~1|FieldExpPlot,data=GSe1e2,na.action=na.exclude,method="ML");summary(mod4);print("e001 and e002 spei");Anova(mod4,type="III")

mydf <- ggeffect(mod3, terms = c("NAdd","scaletotal_precip_mm_Y [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()
mydf <- ggeffect(mod4, terms = c("NAdd","scaleSPEI_4 [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

##########################################################################
##########################################################################
##########################################################################
###############             GSe141		      ##############################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################


# with N*Year and div*year (n*div*year not significant (nor N*Div) so removed))
print("e141");mod5 <- lme(sqrt(AGB)~NTrt * scaletotal_precip_mm_Y + logDiv * scaletotal_precip_mm_Y + NTrt * ExpYear + logDiv * ExpYear ,random=~1|Ring/Plot,data=GSe141,na.action=na.exclude,method="ML");summary(mod5);print("e141");Anova(mod5,type="III")
print("e141");mod6 <- lme(sqrt(AGB)~NTrt * scaleSPEI_4 + logDiv * scaleSPEI_4 + NTrt * ExpYear + logDiv * ExpYear ,random=~1|Ring/Plot,data=GSe141,na.action=na.exclude,method="ML");summary(mod6);print("e141");Anova(mod6,type="III")

mydf <- ggeffect(mod5, terms = c("NTrt","scaletotal_precip_mm_Y [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()
mydf <- ggeffect(mod6, terms = c("NTrt","scaleSPEI_4 [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

mydf <- ggeffect(mod5, terms = c("logDiv","scaletotal_precip_mm_Y [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()
mydf <- ggeffect(mod6, terms = c("logDiv","scaleSPEI_4 [n=10]"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

#ends creation of text file
# sink()



###### Plots 5 Mar 2019


ggplot(GSe120.154,aes(logDiv,sqrt(AGB),color=total_precip_mm_Y,group=total_precip_mm_Y)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")

ggplot(GSe141,aes(NTrt,sqrt(AGB),color=total_precip_mm_Y,group=total_precip_mm_Y)) +geom_point(stat="summary")+ stat_smooth(method=lm, level=0.95,se=F)+scale_color_continuous(low="yellow",high="blue")
ggplot(GSe141,aes(logDiv,sqrt(AGB),color=total_precip_mm_Y,group=total_precip_mm_Y)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")

ggplot(GSe1e2,aes(NAdd,sqrt(AGB),color=total_precip_mm_Y,group=total_precip_mm_Y)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")
ggplot(GSe1e2[GSe1e2$Year>1988,],aes(NAdd,sqrt(AGB),color=total_precip_mm_Y,group=total_precip_mm_Y)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")


###SPEI
ggplot(GSe120.154,aes(logDiv,sqrt(AGB),color=SPEI_4,group=SPEI_4)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")

ggplot(GSe141,aes(NTrt,sqrt(AGB),color=SPEI_4,group=SPEI_4)) +geom_point(stat="summary")+ stat_smooth(method=lm, level=0.95,se=F)+scale_color_continuous(low="yellow",high="blue")
ggplot(GSe141,aes(logDiv,sqrt(AGB),color=SPEI_4,group=SPEI_4)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")

ggplot(GSe1e2,aes(NAdd,sqrt(AGB),color=SPEI_4,group=SPEI_4)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")
ggplot(GSe1e2[GSe1e2$Year>1988,],aes(NAdd,sqrt(AGB),color=SPEI_4,group=SPEI_4)) +geom_point(position = "jitter")+ stat_smooth(method=lm, level=0.95)+scale_color_continuous(low="yellow",high="blue")





