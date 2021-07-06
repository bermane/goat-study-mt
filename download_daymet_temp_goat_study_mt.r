#code to download and extract hourly and daily temperature variables
#across a study area in glacier national park
#data extracted corresponds to goat population surveys over 2008-2019

#load packages
library(rgdal)
library(sp)
library(raster)
library(plyr)
library(lubridate)
library(daymetr)
library(rgdal)

#set wd
setwd('/Volumes/SSD/goat_surveys')

#load viewshed
viewshed <- rgdal::readOGR('reference/Viewsheds_clipped_by_lakes.shp')

#transform to CRS of daymet data
viewshed <- spTransform(viewshed, "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +ellps=WGS84 +lat_2=45")

#tiles needed: queried from website. Try 12453
tiles <- c(12453)
clim <- c('tmax', 'tmin')
years <- 2006:2017

for(tt in tiles){
  for(yr in years){
    for(param in clim) {
      download_daymet_tiles(tiles = tt, start = yr, end = yr, param = param, path = "./temp")
    }
  }
}

#redownload corrupt files if needed
#download_daymet_tiles(tiles = 12095, start = 2017, end = 2017, param = "tmax", path = "./temp")
