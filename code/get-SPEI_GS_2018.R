## script from Eric Lind
# 21 Jan 2016
# Clare edited to adjust location references 
# and JANE adjusted 20 May 2017 to get newest data and update to new ncdf package and change all the functions (why did they do that? jerks.)

### edited again to get growing season (april-July)

rm(list=ls())

## script to obtain SPEI drought index through time for NutNet sites (but clare changed to CDR)
#install.packages(c('chron','ncdf4','raster')) #NOW ncdf=ncdf4 and the functions are all the ones below
require(chron)
require(ncdf4)
require(raster)

## need local copy of spei04.nc (four-month interpolation of drought index)
## http://digital.csic.es/handle/10261/128892
## ~316 MB files

## 4-month version (so that I can calculate from Apr-July)
spei_04.nc <- nc_open('~/Documents/SYNTH REBOOT/spei and precip data/spei04.nc')
print(spei_04.nc) # header metadata from file

t <- ncvar_get(spei_04.nc,'time') # extract information about the time series
nt <- dim(t) #length of time series
tunits <- ncatt_get(spei_04.nc,'time','units')
tunits
nc_close(spei_04.nc) # close connection (keep tidy)

# following depends on form of attribute information
# for SPEI, time attribute is in form 'days since 1900-1-1'
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3],"-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
realt <- chron(dates.=t,origin=c(tmonth,tday,tyear),format=c(dates="m/dd/yyyy"))

# extract given date (single layer is raster)
kai <- grep('Feb/../2007',realt)
spei04.kai <- raster('~/Documents/jmc cdr synth folder/2017/spei04.nc',band=kai)
require(RColorBrewer)
image(spei04.kai,col=brewer.pal(9,'YlGn'))

# a 'brick' is a 3D compilation of raster layers. think of phyllo dough. mmmm.
spei04 <- brick('~/Documents/SYNTH REBOOT/spei and precip data/spei04.nc')

## extract single site
sp <- SpatialPoints(data.frame(longitude=-93.201,latitude=45.401))
CDR.brick04 <- as.vector(extract(spei04,sp))

# plot timeseries
require(ggplot2)
qplot(x=as.POSIXct(realt),y=CDR.brick04)+geom_line()+xlab('year') + ylab('SPEI') + geom_hline(y=0) 

dat.x <- data.frame(site='CDR',month=months(as.POSIXct(realt)),year=years(as.POSIXct(realt)),spei_04=CDR.brick04)
head(dat.x)

recent <- dat.x[dat.x$year>1980,]
# plot August values
qplot(x=recent[recent$month=='July','year'],y=recent[recent$month=='July','spei_04'])+ geom_point(size=5)+xlab('year') + ylab('SPEI (04 month)') 

write.csv(dat.x[dat.x$year>1949,],'~/Documents/SYNTH REBOOT/spei and precip data/CDR-SPEI04.csv',row.names=F)
