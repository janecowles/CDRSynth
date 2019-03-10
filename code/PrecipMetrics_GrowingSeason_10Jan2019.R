####Cedar Creek Climate Data####

###Calculating metrics: average event size, number of events, consecutive dry days, consecutive wet days, total precip

###Time periods: hydrologic year (Sep previous - Aug current), May, June, July, August, May-August

###JMC 12Jan2016 changes: hydrologic year changing to Sep previous to July current -- to avoid the month during which harvesting occurred 
### JMC 20Mar2016 changes: Growing season (April-July)

###Definitions:
##average event size: average daily precip for days with >0mm
##number of events: number of days with precip >0mm
##consecutive dry days: average number of days of the dry period between precipitation events
##consecutive wet days: average number of consecutive days with preip >0mm
##total precip: total cumulative precip when precip >0mm

###Note: Knapp et al. 2015 set their minimum rainfall amount to 0.3 mm because that was the strictest limitation in resolution by one of their sites. Our minimum is 0.254 mm, which isn't too far off anyway

##Clear all existing data
rm(list=ls())

##Load libraries
library(plyr)
library(weathermetrics)
##Set working directory 
setwd("~/Documents/SYNTH REBOOT") #r


##Import data
daily<-read.csv("spei and precip data/e080_Daily climate summary.csv")
	#contains 1962-2015

##Make date column a date
daily$Date<-as.Date(daily$Date,"%m/%d/%y")
#get rid of dates in the future
daily$Date<-as.Date(ifelse(daily$Date>Sys.Date(),format(daily$Date,"19%y-%m-%d"),format(daily$Date)))

daily$Year <- as.numeric(format(daily$Date,'%Y'))
daily$Month <- as.numeric(format(daily$Date,'%m'))
#daily$Month <- as.character(format(daily$Date,'%B'))

daily$Precip.mm. <- inches_to_metric(daily$Precip.inches.,"mm",round = 2)

##Define GS year of -- April-July
daily$GSyear<-with(daily,ifelse(Month %in% c(4:7),Year,NA)) ###JMC: Now should just be April-July
#check
#cbind(daily$Month,daily$Year,daily$GSyear)


################
# i'm not sure if this is how to do this.... I may have to create a new file and merge it on, but will try
################


#Define the growing season for the year prior -- lagged effects
daily$GSyearlagged <- with(daily,ifelse(Month %in% c(4:7),Year+1,NA))
#check
#cbind(daily$Month,daily$Year,daily$GSyearlagged)

##Subset dataset for days with preicp
precip<-subset(daily,Precip.mm.>0)

##Subset dataset for days without precip
noPrecip<-subset(daily,Precip.mm.==0)

##Consecutive dry days function
#Needs precip$Date
CDDfun<-function(data){
	
	#Make column for days between precip dates
	dayDiff<-c(0,rep(NA,length(data)-1))
	
	#Substract number of days between current and previous precip
	for(i in 2:length(data)){
		dayDiff[i]<-data[i]-data[i-1]
		}
	
	#Subset for at least one dry day (dayDiff=1 means yesterday it rained as well as today) and take the mean of the number of days

	CDD<-mean(subset(dayDiff,dayDiff>1))
	
	return(CDD)

}

##Consecutive wet days function
#Needs noPrecip$Date
CWDfun<-function(data){
	
	#Make column for days between dry dates
	dayDiff<-c(0,rep(NA,length(data)-1))
	
	#Substract number of days between current and previous dry day
	for(i in 2:length(data)){
		dayDiff[i]<-data[i]-data[i-1]
		}
	
	#Subset for at least one dry day (dayDiff=1 means yesterday it was dry as well as today) and take the mean of the number of days

	CWD<-mean(subset(dayDiff,dayDiff>1))
	
	return(CWD)

}

##Hydrologic year metrics
#Most metrics
GSyearMetrics<-ddply(precip,.(GSyear),summarise,total_precip_mm=sum(Precip.mm.),avg_event_size=mean(Precip.mm.),num_events=length(Precip.mm.),CDD=CDDfun(Date))
#CWD
GSyearCWD<-ddply(noPrecip,.(GSyear),summarise,CWD=CWDfun(Date))
#Combined
GSyearPrecip<-merge(GSyearMetrics,GSyearCWD)

#LAGGED Hydrologic year metrics
#Most metrics
GSyearlaggedMetrics <- ddply(precip,.(GSyearlagged),summarise,total_precip_mm=sum(Precip.mm.),avg_event_size=mean(Precip.mm.),num_events=length(Precip.mm.),CDD=CDDfun(Date))
#CWD
GSyearlaggedCWD<-ddply(noPrecip,.(GSyearlagged),summarise,CWD=CWDfun(Date))
#Combined
GSyearlaggedPrecip<-merge(GSyearlaggedMetrics,GSyearlaggedCWD)

##Merge all precip together


MasterPrecip <- merge(GSyearPrecip,GSyearlaggedPrecip,by.x="GSyear",by.y="GSyearlagged",all=T,suffixes=c("_Y","_Ylag"))
#check
#cbind(MasterPrecip$CDD_Y,MasterPrecip$CDD_Ylag)

#an extra row (row 53) is in there because of the removal of august from the hydrologic year, so delete it here, and again for the masterprecip5 one (because these are two files I imagine more as final files to use)
MasterPrecip <- MasterPrecip[!is.na(MasterPrecip$GSyear),]


# #Unique column names
# colnames(MasterPrecip5)[2:31]<-c("total_precip_mm_Y","avg_event_size_Y","num_events_Y","CDD_Y","CWD_Y","total_precip_mm_Ylag","avg_event_size_Ylag","num_events_Ylag","CDD_Ylag","CWD_Ylag","total_precip_mm_G","avg_event_size_G","num_events_G","CDD_G","CWD_G","total_precip_mm_5","avg_event_size_5","num_events_5","CDD_5","CWD_5","total_precip_mm_6","avg_event_size_6","num_events_6","CDD_6","CWD_6","total_precip_mm_7","avg_event_size_7","num_events_7","CDD_7","CWD_7","total_precip_mm_8","avg_event_size_8","num_events_8","CDD_8","CWD_8")

##Write data to files

#new file! These are the metrics JMC and CEK decided were useful for the first run analysis 8 Jan 2016
write.csv(MasterPrecip,"GSyearandLagPrecip_012019.csv",row.names=F)
