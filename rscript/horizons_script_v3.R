

rm(list=ls())

library(geotopbricks)
library(horizons)
library(sf)
wpath <-  '/home/ecor/local/repos/geotop_r_templink/geotop_simulations/templates/TEST_Venosta_3D_034_template'


dem <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath)

nmeteo <- get.geotop.inpts.keyword.value("NumberOfMeteoStations",numeric=TRUE,wpath=wpath)

horizonfiles <- get.geotop.inpts.keyword.value("HorizonMeteoStationFile",wpath=wpath,add_wpath=TRUE) 
horizonfiles <- paste(horizonfiles,"%04d.txt",sep="")

horizonfiles <- sprintf(horizonfiles,1:nmeteo)

###

meteopoints <- as.data.frame(array(NA,c(nmeteo,2)))
names(meteopoints) <- c("x","y")

meteopoints$x <- get.geotop.inpts.keyword.value("MeteoStationCoordinateX",numeric=TRUE,wpath=wpath)
meteopoints$y <- get.geotop.inpts.keyword.value("MeteoStationCoordinateY",numeric=TRUE,wpath=wpath)
meteopoints_code <- get.geotop.inpts.keyword.value("MeteoStationCode",vector_sep=",",wpath=wpath)

names(horizonfiles) <- meteopoints_code

horizons <- horizon(r=dem,points=meteopoints,n=16,names=meteopoints_code)
## Default values for points out of Digital Elevation Model
nnn <-meteopoints_code[!(meteopoints_code %in% names(horizons))]
for ( i in nnn) {
  horizons[[i]] <- data.frame(AngleFromNorthClockwise=c(0:3)*90,HorizonHeight=5.0)
  names(horizons[[i]]) <- names(horizons[[1]])
}

horizons <- horizons[meteopoints_code]
####
### WEATHER STATIONS
meteo <- get.geotop.points(prefix="MeteoStation",suffixes=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
meteo <- meteo %>% st_transform(crs=4326)
# These lines were added to verify why some horizons are not calculated. Out of Digital Elevation Model! 
####


LINE <- 4

sr <- as.character(NA)
sr[1] <- "! Horizon file for met station or point # %s"
sr[2] <- "! All measures in degrees"
sr[3:LINE] <- ""
header <- sr



for (i in names(horizons)) {
	
	#sr <- readLines(horizonfiles[i])
	sr <- sprintf(header,as.character(i))
	title <- paste(names(horizons[[i]]),collapse=",")
	
	lines <- paste(horizons[[i]][,1],horizons[[i]][,2],sep=",")
	sr_new <- c(sr[1:LINE],title,lines)
	writeLines(text=sr_new,con=horizonfiles[i])
	
	
}
#


#
#



