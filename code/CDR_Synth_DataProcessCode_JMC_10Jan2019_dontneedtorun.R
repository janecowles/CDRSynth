# 20 May 2017
# JMC code/annotations
# for reading in precipitation data + SPEI information + e120 data + e001 data + e141 data
# note that some of the code is to fix errors in the data file. make sure to read and run carefully as such.

##Clear all existing data
rm(list=ls())

##Load libraries

#install.packages(c('plyr','nlme','lme4','car'))

library(plyr)
library(nlme)
library(lme4)
library(car)
library(ggplot2)
library(vegan)

##Set working directory 
setwd("~/Documents/SYNTH REBOOT/") #r


### read in GSyear and GSyearlagged precip metrics
GSprecipdat <- read.csv("spei and precip data/GSyearandLagPrecip_012019.csv")

### Read in 11 month SPEI, make sure to use JULY and 4 month!
spei <- read.csv("spei and precip data/SPEI_45.25_-93.25.csv")
str(spei)
spei <- spei[,c("DATA","SPEI_4")]
spei$MONTH <- as.character(substr(spei$DATA,1,3))
spei$YEAR <- as.numeric(substr(spei$DATA,4,7))
spei_july <- spei[spei$MONTH=="Jul",c("SPEI_4","MONTH","YEAR")]

#names(GSspei)

GSdat1 <- merge(GSprecipdat,spei_july,by.x="GSyear",by.y="YEAR",all=T)


GSspei.touseforlag <- GSdat1
GSspei.touseforlag$Yearlagged <- GSspei.touseforlag$GSyear + 1
GSspei.touseforlag$SPEI_4lag <- GSspei.touseforlag$SPEI_4
GSspei.touseforlag <- subset(GSspei.touseforlag,select=c(Yearlagged,SPEI_4lag))

GSdat <- merge(GSdat1,GSspei.touseforlag,by.x="GSyear",by.y="Yearlagged",all.x=T)
#write.csv(GSdat,"PrecipDat_4month.csv",row.names=F)



###################################
####### E120 DATA WRANGLING #######
###################################

e120big <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/e120mega96-18.csv")
#str(e120big)
names(e120big)
#the below problem is not a problem in the e120mega96-15 dataset but were in the old dataset so i'm keeping this here.
#tapply(e120big$NumSp,e120big$Plot,sd)
#there are four mistakes in the NumSp (planted diversity) column. In plots 75 and 197 in years 2005 and 2006, NumSp says 1 species, while in all other years NumSp is 2 for these plots. The code below changes that.
# e120big$NumSp <- ifelse(e120big$Plot==75,2,e120big$NumSp)
# e120big$NumSp <- ifelse(e120big$Plot==197,2,e120big$NumSp)

#changing to names I like better

e120big$AGB <- e120big$AbvBioAnnProd
e120big$DivTrt <- e120big$NumSp

#subsetting to managable dataset size

e120small <- subset(e120big,select=c("Exp","Plot","Year","Month","DivTrt","AGB","RootBio0_30"))

## merge together with dat (the precip+spei info)
e120 <- merge(e120small,GSdat,by.x="Year",by.y="GSyear",all.x=T)
## but then there were extra plots. 23,100, and 140 were in the dataset in 2008 only (and do not have an assigned diversity level, so I figure it's a mistake and I cut them out.)
#e120 <- e120wrong[!e120wrong$Plot %in% c(23,100,140),]

#A subset of plots were no longer sampled after 2006. To account for any differences this may make in the analyses, I cut these plots out. Now there are only 154 plots, and these plots were sampled consistently from 1996-2011.

e120.154 <- e120[!e120$Plot %in% c(32,73,127,151,153,161,189,221,255,256,272,296,322,333),]

#releveling to get rid of "ghost" levels for the plots that I didn't keep in.
e120.154$Plot <- factor(e120.154$Plot)
tapply(e120.154$AGB,e120.154$Year,sum,na.rm=T)
e120.154$ExpYear <- e120.154$Year - min(e120.154$Year)+1
e120.154$logDiv <- log(e120.154$DivTrt)


write.csv(e120.154,"Processed Data/GSe120.154_clean.csv",row.names=F)


###################################
####### E001 DATA WRANGLING #######
###################################

# new data in folder is e001mega15.csv
e001datnew <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/e001mega18.csv")
str(e001datnew)
names(e001datnew)
e001datnew1 <- e001datnew[!e001datnew$NTrt==9,] #take out the one without micros
e001datnew1$AGB <- e001datnew1$TotBio
e001datnew1$Plot <- as.factor(e001datnew1$Plot)
plot(AGB~RootBio,e001datnew1)
hist(e001datnew1$AGB[!e001datnew1$AGB>2000])
tail(sort(e001datnew1$AGB))
e001datnew[e001datnew1$AGB %in% tail(sort(e001datnew1$AGB)),]

with(e001datnew1,tapply(NTrt,interaction(Plot,Field),mean))

###OK, CUTTING OUT ALL PLOTS WITH BIOMASS GREATER THAN 2000. There's some slightly bigger than 1600, but after 1650 there is a big gap to 2113 and it seems like the rest of these are outliers.
names(e001datnew1)
e001datshort <- e001datnew1[!e001datnew1$AGB>2000,c(1:9,15,292)]
e001datshort$FieldPlot <- as.factor(paste(e001datshort$Field,e001datshort$Plot,sep=""))
head(e001datshort)
table(e001datshort$Fenced)
table(e001datshort$Burned,e001datshort$Year,e001datshort$Field,e001datshort$Fenced)

table(interaction(e001datshort$Burned,e001datshort$Fenced),e001datshort$Year,e001datshort$Field)

# 1982-2004 for A B and C are usable
#### REMEMBER THAT IN NTrt BOTH 0 and 9 have zero nitrogen added!!!! Do not use this factor (use NAdd instead unless remove the one that had no n and no micronutrients (I think this is 9, but if it is 1, would then have to relevel so that they go in order of increasing N addition). So probably just use NAdd AND THINK ABOUT REMOVING THE LEVEL WITH NO N AND NO MICROS? Should we?

#### GROWING SEASON
# GSe001 <- merge(e001datshort[e001datshort$Field!="D"&e001datshort$Year %in% c(1984:2002,2004),],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe001 <- merge(e001datshort[e001datshort$Field!="D",],GSdat,by.x="Year",by.y="GSyear",all.x=T)

GSe001$Plot <- factor(GSe001$Plot)
GSe001$ExpYear <- GSe001$Year - min(GSe001$Year)+1


GSe001c <- merge(e001datshort[e001datshort$Field=="C"&e001datshort$Year %in% c(1984:2002,2004),],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe001c$Plot <- factor(GSe001c$Plot)
GSe001c$ExpYear <- GSe001c$Year - min(GSe001c$Year)+1

GSe001a <- merge(e001datshort[e001datshort$Field=="A"&e001datshort$Year %in% c(1984:2002,2004),],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe001a$Plot <- factor(GSe001a$Plot)
GSe001a$ExpYear <- GSe001a$Year - min(GSe001a$Year)+1

GSe001b <- merge(e001datshort[e001datshort$Field=="B"&e001datshort$Year %in% c(1984:2002,2004),],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe001b$Plot <- factor(GSe001b$Plot)
GSe001b$ExpYear <- GSe001b$Year - min(GSe001b$Year)+1

#### made subset for 1990 onward, explained below in model code
GSe001.1990on<-GSe001[GSe001$Year>=1990,] #exclude the 1988 drought

write.csv(GSe001,"Processed Data/GSe001_clean.csv",row.names=F)
write.csv(GSe001.1990on,"Processed Data/GSe001.1990on_clean.csv",row.names=F)

GSe001c.1990on<-GSe001c[GSe001c$Year>=1990,] #exclude the 1988 drought

write.csv(GSe001c,"Processed Data/GSe001c_clean.csv",row.names=F)
write.csv(GSe001c.1990on,"Processed Data/GSe001c.1990on_clean.csv",row.names=F)

GSe001a.1990on<-GSe001a[GSe001a$Year>=1990,] #exclude the 1988 drought

write.csv(GSe001a,"Processed Data/GSe001a_clean.csv",row.names=F)
write.csv(GSe001a.1990on,"Processed Data/GSe001a.1990on_clean.csv",row.names=F)



GSe001b.1990on<-GSe001b[GSe001b$Year>=1990,] #exclude the 1988 drought

write.csv(GSe001b,"Processed Data/GSe001b_clean.csv",row.names=F)
write.csv(GSe001b.1990on,"Processed Data/GSe001b.1990on_clean.csv",row.names=F)


###################################
####### E002 DATA WRANGLING #######
###################################

e002dat <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/2018 e002Mega.csv")
str(e002dat)
names(e002dat)
e002dat1 <- e002dat[!e002dat$Ntrt==9,]
table(e002dat1$BurnTrt,e002dat1$Year,e002dat1$Field,e002dat1$NtrtRec)
table(interaction(e002dat1$BurnTrt,e002dat1$NtrtRec),e002dat1$Year,e002dat1$Field)


# e002dat2 <- e002dat1[e002dat1$Year<2015,c(1:13,15)] #2015 is weird in field B, so excluded! #Nope i've figured this out -- i just need to get rid of the ntrt rec = 0 bits. There was another cessation treatment

e002dat2 <- e002dat1[e002dat1$NtrtRec==1,c(1:13,15)]

table(interaction(e002dat2$BurnTrt,e002dat2$NtrtRec),e002dat2$Year,e002dat2$Field)

#there are 25 plots in field A in 2018?
##### 4 March 2019 -- stopped here!! (JMC)

str(e002dat2)
e002dat2$FieldPlot <- as.factor(paste(e002dat2$Field,e002dat2$Plot,sep="."))
plotlist <- factor(e002dat2$FieldPlot[e002dat2$Year==2008&e002dat2$NtrtRec==1&e002dat2$BurnTrt==0])
e002cont <-e002dat2[e002dat2$FieldPlot %in% plotlist,]
table(interaction(e002cont$BurnTrt,e002cont$NtrtRec),e002cont$Year,e002cont$Field)
str(e002cont)
hist(e002cont$TotBio)
e002cont$AGB <- e002cont$TotBio
e002cont <- e002cont[e002cont$AGB<2000,]
e002cont$FieldPlot <- as.factor(paste(e002cont$Field,e002cont$Plot,sep=""))

GSe002 <- merge(e002cont,GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe002$Plot <- factor(GSe002$Plot)
GSe002$ExpYear <- GSe002$Year - min(GSe002$Year)+1
GSe002 <- GSe002[GSe002$Year %in% c(1984:2002,2004),]

GSe002c <- merge(e002cont[e002cont$Field=="C",],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe002c$Plot <- factor(GSe002c$Plot)
GSe002c$ExpYear <- GSe002c$Year - min(GSe002c$Year)+1

GSe002a <- merge(e002cont[e002cont$Field=="A",],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe002a$Plot <- factor(GSe002a$Plot)
GSe002a$ExpYear <- GSe002a$Year - min(GSe002a$Year)+1

GSe002b <- merge(e002cont[e002cont$Field=="B",],GSdat,by.x="Year",by.y="GSyear",all.x=T)
GSe002b$Plot <- factor(GSe002b$Plot)
GSe002b$ExpYear <- GSe002b$Year - min(GSe002b$Year)+1

#### made subset for 1990 onward
GSe002.1990on<-GSe002[GSe002$Year>=1990,] #exclude the 1988 drought

write.csv(GSe002,"Processed Data/GSe002_clean.csv",row.names=F)
write.csv(GSe002.1990on,"Processed Data/GSe002.1990on_clean.csv",row.names=F)

GSe002c.1990on<-GSe002c[GSe002c$Year>=1990,] #exclude the 1988 drought

write.csv(GSe002c,"Processed Data/GSe002c_clean.csv",row.names=F)
write.csv(GSe002c.1990on,"Processed Data/GSe002c.1990on_clean.csv",row.names=F)

GSe002a.1990on<-GSe002a[GSe002a$Year>=1990,] #exclude the 1988 drought

write.csv(GSe002a,"Processed Data/GSe002a_clean.csv",row.names=F)
write.csv(GSe002a.1990on,"Processed Data/GSe002a.1990on_clean.csv",row.names=F)

###########
GSe001merge <- GSe001 #continuous plots in e001 1984-2002,2004 (48 per field)
GSe001merge$FieldExpPlot <- as.factor(paste(GSe001merge$FieldPlot,"E001",sep=""))
GSe001merge$Experiment <- "E001"
GSe002merge <- GSe002 #continuous plots in e002 1984-2002,2004 (24 per field)
GSe002merge$FieldExpPlot <- as.factor(paste(GSe002merge$FieldPlot,"E002",sep=""))
GSe002merge$NTrt <- GSe002merge$Ntrt
GSe002merge$Experiment <- "E002"

GSe001merge<-GSe001merge[,c("Year","Experiment", "Field", "Plot","NTrt","NAdd","AGB","FieldPlot","total_precip_mm_Y","avg_event_size_Y","num_events_Y" ,"CDD_Y", "CWD_Y","total_precip_mm_Ylag","avg_event_size_Ylag" ,"num_events_Ylag", "CDD_Ylag","CWD_Ylag", "MONTH", "SPEI_4","SPEI_4lag","ExpYear", "FieldExpPlot")]

GSe002merge<-GSe002merge[,c("Year", "Experiment","Field", "Plot","NTrt","NAdd","AGB","FieldPlot","total_precip_mm_Y","avg_event_size_Y","num_events_Y" ,"CDD_Y", "CWD_Y","total_precip_mm_Ylag","avg_event_size_Ylag" ,"num_events_Ylag", "CDD_Ylag","CWD_Ylag" ,"MONTH", "SPEI_4","SPEI_4lag","ExpYear", "FieldExpPlot")]

names(GSe001merge)
names(GSe002merge)

GSe1e2 <- rbind(GSe001merge,GSe002merge)
names(GSe1e2)
write.csv(GSe1e2,"Processed Data/GSe1e2_clean.csv",row.names=F)


###################################
####### E141 DATA WRANGLING #######
###################################


e141dat <- read.csv("~/Dropbox/UMN Postdoc/CDR DATA/BioCON Master Harvest_190104 for DB.csv")
str(e141dat)
names(e141dat)

e141dat <- e141dat[e141dat$Season=="August"&e141dat$CountOfSpecies!=0&e141dat$Water.Treatment!="H2Oneg"&e141dat$Temp.Treatment!="HTelv"&e141dat$CO2.Treatment=="Camb",c(1:41)]

e141dat$Year <- e141dat$year
e141dat$AGB <- e141dat$AbovegroundTotal.Biomass..g.m.2
e141dat$DivTrt <- e141dat$CountOfSpecies
e141dat$NTrt <- e141dat$Nitrogen.Treatment
e141dat$Plot <- factor(e141dat$Plot)
e141dat$Ring <- factor(e141dat$Ring)

e141short <- e141dat[,c("Year","Ring","Plot","NTrt","DivTrt","AGB")]

GSe141 <- merge(e141short,GSdat,by.x="Year",by.y="GSyear",all.x=T)

tapply(GSe141$AGB,GSe141$Year,sum,na.rm=T)
GSe141$ExpYear <- GSe141$Year - min(GSe141$Year)+1
GSe141$logDiv <- log(GSe141$DivTrt)

write.csv(GSe141,"Processed Data/GSe141_clean.csv",row.names=F)


