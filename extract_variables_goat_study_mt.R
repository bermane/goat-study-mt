#code calculater variables for goat study in Montana

#load packages
library(rgdal)
library(sp)
library(raster)
library(lubridate)
library(rgeos)
library(tidyverse)
library(R.utils)

#set wd
setwd('/Volumes/SSD/goat_surveys')

#set rasteroptions to use a bit more memory
rasterOptions()
rasterOptions(maxmemory = 1e+10)

#load emodis data
pirgd <- brick('./pirgd_mt_2001_2018.tif')

#change names of layers to years
names(pirgd) <- 2001:2018

#load goat survey grid
goatsurvey <- readOGR('reference/2009GoatSurvey_Box.shp')

#load goat occupancy raster to mask certain values
occu <- raster('reference/Predicted Occupancy_for_Tab.tif')

#load gnp and quadrant shapes
gnp <- readOGR('/Volumes/SSD/goat_surveys/reference/boundary2003.shp')
quad <- readOGR('/Volumes/SSD/goat_surveys/reference/gnp_quad.shp')

##############################
###PIRGd VARIANCE AND RANGE###
##############################

#calculate var and range for each grid cell
#masked with goat occupancy layer

#choose what years you want to extract from
years <- 2006:2018

#reproject occupany layer to match pirgd
occu_pirgd <- occu %>% raster::aggregate(fact = 2) %>% projectRaster(to = pirgd)

#set mask values in occupancy layer
occu_pirgd[occu_pirgd < 0.004] <- NA

#mask pirgd values
pirgd_mask <- mask(pirgd, occu_pirgd)

#transform goat survey grid to crs of pirgd
goatsurvey_pirgd <- goatsurvey %>% spTransform(CRSobj = crs(pirgd_mask))

#create output dataframe
goat_df <- data.frame(goatsurvey@data)

#extract values for individual years
for(i in 1:length(years)){
  #load in corrent year of pirgd
  pirgd_yr <- pirgd_mask[[which(names(pirgd_mask) %in% paste0("X", years[i]))]]
  
  #extract values and write to DF for viewshed
  #variance and range added
  #mean_vs <- raster::extract(pirgd_yr, goatsurvey, fun = mean, na.rm = T, df = T)
  #mean_vs[,2] <- round(mean_vs[,2])
  max_vs <- raster::extract(pirgd_yr, goatsurvey_pirgd, fun = max, na.rm = T, df = T)
  min_vs <- raster::extract(pirgd_yr, goatsurvey_pirgd, fun = min, na.rm = T, df = T)
  var_vs <- raster::extract(pirgd_yr, goatsurvey_pirgd, fun = var, na.rm = T, df = T)
  var_vs[,2] <- round(var_vs[,2], 2)
  #colnames(mean_vs) <- c("ID", paste0("mean_pirgd_", years[i]))
  colnames(max_vs) <- c("ID", paste0("max_pirgd_", years[i]))
  colnames(min_vs) <- c("ID", paste0("min_pirgd_", years[i]))
  colnames(var_vs) <- c("ID", paste0("var_pirgd_", years[i]))
  goat_df <- cbind(goat_df, max_vs[,2,drop = F], min_vs[,2,drop = F], var_vs[,2,drop = F])
  
  #clean up
  rm(pirgd_yr, max_vs, min_vs, var_vs)
}

#write to disk and clean up
#write.csv(goat_df, file = "output/pirgd_goatsurveybox_2006_2018_max_min_var.csv")
rm(pirgd_mask, occu_pirgd, goat_df, years, goatsurvey_pirgd)

#######################################
###CALC TEMP VARIABLES AT GRID LEVEL###
#######################################

#number of days average temp > 12.8 C July-August
#number of days average temp > 15.6 C July-August
#average daily temp July-August

#choose what years you want to extract from
years <- 2006:2019

#create output dataframe
goat_df <- data.frame(goatsurvey@data)

for(yr in years){
  
  #load temp file years
  tmax_f <- list.files(path = 'temp', pattern = glob2rx(str_c('tmax_', yr, '*.nc')), full.names = T)
  tmin_f <- list.files(path = 'temp', pattern = glob2rx(str_c('tmin_', yr, '*.nc')), full.names = T)
  
  if(length(tmax_f) == 2 & length(tmin_f) == 2) {print(str_c('two files for ', yr))} else{
    print(str_c('missing files for ', yr))
  }
  
  #load temp years
  tmax <- merge(stack(tmax_f[1]), stack(tmax_f[2]))
  tmin <- merge(stack(tmin_f[1]), stack(tmin_f[2]))
  
  #only keep july and august
  tmax <- tmax[[182:243]]
  tmin <- tmin[[182:243]]
  
  #calc average daily temp
  tavg <- (tmax + tmin)/2
  rm(tmax, tmin, tmax_f, tmin_f)
  
  #change goatsurvey to crs of raster
  goatsurvey_temp <- spTransform(goatsurvey, CRSobj = crs(tavg))
  
  #reproject occupany layer to match daymet
  occu_temp <- occu %>% raster::aggregate(fact = 10) %>% projectRaster(to = tavg)
  
  #set mask values in occupancy layer
  occu_temp[occu_temp < 0.004] <- NA
  
  #mask temp values
  tavg_mask <- mask(tavg, occu_temp)
  
  #extract average daily temp
  avg_daily <- raster::extract(tavg_mask, goatsurvey_temp, fun = mean, na.rm = T, df = T, weights = T)
  avg_daily <- avg_daily[,-1]
  
  #calc average temp
  avg_temp <- data.frame(rowMeans(avg_daily)) %>% round(2)
  colnames(avg_temp) <- str_c('avg_temp_july_august_', yr)
  
  #calc days above thresholds
  days_above_12.8 <- avg_daily
  days_above_12.8[days_above_12.8 <= 12.8] <- 0
  days_above_12.8[days_above_12.8 > 12.8] <- 1
  days_above_12.8 <- data.frame(rowSums(days_above_12.8))
  colnames(days_above_12.8) <- str_c('days_above_12.8_july_august_', yr)
  
  days_above_15.6 <- avg_daily
  days_above_15.6[days_above_15.6 <= 15.6] <- 0
  days_above_15.6[days_above_15.6 > 15.6] <- 1
  days_above_15.6 <- data.frame(rowSums(days_above_15.6))
  colnames(days_above_15.6) <- str_c('days_above_15.6_july_august_', yr)
  
  #bind to goat_df
  goat_df <- cbind(goat_df, avg_temp[, 1, drop = F], days_above_12.8[, 1, drop = F], days_above_15.6[, 1, drop = F])
  
  #clean up 
  rm(tavg, tavg_mask, goatsurvey_temp, occu_temp, avg_daily, avg_temp, days_above_12.8, days_above_15.6)
  removeTmpFiles(h = 0.000001)
}

#write to disk and clean up
#write.csv(goat_df, file = "output/temp_goatsurveybox_2006_2019_jul_aug_avg_and_days_above.csv")
rm(years, goat_df, i, yr)

##########
###PDSI###
##########

#code to organize preprocessed scPDSI from Westwide Drought Tracker
pdsi_f <- list.files(path = '/Volumes/SSD/climate_effects/drought/raw', pattern = '.nc$', full.names = T)

#sort so in order by month
pdsi_f <- str_sort(pdsi_f, numeric = T)

for(i in 1:length(pdsi_f)){
  #load raster
  pdsi <- stack(pdsi_f[i])
  
  #keep bands we want: years 2006-2019
  pdsi <- pdsi[[112:125]]
  
  #transform crs of GNP and buffer
  gnp_pdsi <- spTransform(gnp, crs(pdsi))
  
  #disag, reproject, crop
  pdsi <- pdsi %>% crop(., gnp_pdsi)
  
  #write to disk
  writeRaster(pdsi, filename = str_c('drought/bymonth/pdsi_2006_2019_', i, '.tif'), format = 'GTiff')
  
  #clean up
  rm(pdsi)
  removeTmpFiles(h = 0.000000001)
}

#clean up
rm(pdsi_f, gnp_pdsi)

#files are now organized by month. change them to by year.
#load files and make sure in correct order
pdsi_f <- list.files(path = 'drought/bymonth', pattern = '.tif$', full.names = T) %>%
  str_sort(numeric = T)

#which years
years <- 2006:2019

#load in files by band (month) and write out
for(i in 1:length(years)){
  pdsi <- stack(pdsi_f)
  pdsi <- stack(pdsi_f, bands = i)
  writeRaster(pdsi, filename = str_c('drought/pdsi_gnp_', 2005 + i, '.tif'),
              format = 'GTiff')
  rm(pdsi)
}

#clean up
rm(pdsi_f, i)

#create pdsi output data frame
pdsi_out <- data.frame(area = rep(c("whole", 'sw', 'se', 'nw', 'ne'), 2),
                       time = rep(c('year', 'jul_sep'), each = 5))

#loop through years to extract pdsi values
for(yr in years){
  
  #load pdsi and disagg to 1 km
  pdsi <- stack(str_c('drought/pdsi_gnp_', yr, '.tif'))
  pdsi <- raster::disaggregate(pdsi, fact = 4)
  
  #transform shapes
  gnp_pdsi <- spTransform(gnp, crs(pdsi))
  quad_pdsi <- spTransform(quad, crs(pdsi))
  
  #reproject occupany layer to match daymet
  occu_pdsi <- occu %>% raster::aggregate(fact = 10) %>% projectRaster(to = pdsi)
  
  #set mask values in occupancy layer
  occu_pdsi[occu_pdsi < 0.004] <- NA
  
  #mask pdsi values
  pdsi_mask <- mask(pdsi, occu_pdsi)
  
  #extract average values for entire year
  pdsi_yr <- rbind(raster::extract(pdsi_mask, gnp_pdsi, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(pdsi_mask, quad_pdsi, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(pdsi_mask, gnp_pdsi, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(pdsi_mask, quad_pdsi, fun = mean, na.rm = T, df = T, weights = T))
  pdsi_yr <- pdsi_yr[, -1]
  pdsi_yr[6:10, -(7:9)] <- NA
  pdsi_yr <- data.frame(rowMeans(pdsi_yr, na.rm = T)) %>% round(2)
  colnames(pdsi_yr) <- str_c('pdsi_', yr)
  
  #cbind to df
  pdsi_out <- cbind(pdsi_out, pdsi_yr[, 1, drop = F])
  
  #clean up
  rm(pdsi, gnp_pdsi, quad_pdsi, occu_pdsi, pdsi_mask, pdsi_yr)
}

#write to disk and clean up
#write.csv(pdsi_out, file = "output/pdsi_study_area_quads_2006_2019_year_and_jul_sep.csv")
rm(years, pdsi_out, yr)

########################################
###SUMMER PRECIP and TEMP FROM DAYMET###
########################################

#total precip july-sep
#total precip may 15 - june 15
#average temp may 15 - june 15
#all at quadrant level

#average temp oct - apr at study area and quad level

#choose what years you want to extract from
years <- 2006:2018 #2019 manually!!!!

#create output dataframe
clim_out <- data.frame(area = c(rep(c('sw', 'se', 'nw', 'ne'), 3), 'whole', 'sw', 'se', 'nw', 'ne'),
                       variable = c(rep(c('total_prcp_jul_sep', 'total_prcp_may15_jun15',
                                        'avg_temp_may15_jun15', 'avg_temp_oct_yr_april_yrplus1'), each = 4), 
                                    'avg_temp_oct_yr_april_yrplus1'))

for(yr in years){
  
  #load temp and prcp file years
  tmax_f <- list.files(path = 'temp', pattern = str_c('tmax_', yr), full.names = T)
  tmin_f <- list.files(path = 'temp', pattern = str_c('tmin_', yr), full.names = T)
  tmax_f2 <- list.files(path = 'temp', pattern = str_c('tmax_', yr + 1), full.names = T)
  tmin_f2 <- list.files(path = 'temp', pattern = str_c('tmin_', yr + 1), full.names = T)
  prcp_f <- list.files(path = 'temp', pattern = str_c('prcp_', yr), full.names = T)
  
  if(length(tmax_f) == 2 & length(tmin_f) == 2 & length(prcp_f) == 2) {print(str_c('two files for ', yr))} else{
    print(str_c('missing files for ', yr))
  }
  
  #load temp and prcp years
  tmax <- merge(stack(tmax_f[1]), stack(tmax_f[2]))
  tmin <- merge(stack(tmin_f[1]), stack(tmin_f[2]))
  tmax2 <- merge(stack(tmax_f2[1]), stack(tmax_f2[2]))
  tmin2 <- merge(stack(tmin_f2[1]), stack(tmin_f2[2]))
  prcp <- merge(stack(prcp_f[1]), stack(prcp_f[2]))
  
  #stack oct to apr temp separately
  tmax_win <- stack(tmax[[274:365]], tmax2[[1:120]])
  tmin_win <- stack(tmin[[274:365]], tmin2[[1:120]])
  
  #keep may 15 to sep for current year
  prcp <- prcp[[135:273]]
  tmax <- tmax[[135:273]]
  tmin <- tmin[[135:273]]
  
  #calc average daily temp
  tavg <- (tmax + tmin) / 2
  names(tavg) <- names(prcp)
  tavg_win <- (tmax_win + tmin_win) / 2
  rm(tmax, tmin, tmax_f, tmin_f, prcp_f, tmax_f2, tmin_f2, tmax_win, tmin_win,tmax2, tmin2)
  
  #change quad and gnp to crs of raster
  quad_temp <- spTransform(quad, CRSobj = crs(tavg))
  gnp_temp <- spTransform(gnp, CRSobj = crs(tavg))
  
  #reproject occupany layer to match daymet
  occu_temp <- occu %>% raster::aggregate(fact = 10) %>% projectRaster(to = tavg)
  
  #set mask values in occupancy layer
  occu_temp[occu_temp < 0.004] <- NA
  
  #mask prcp and temp values
  prcp_mask <- mask(prcp, occu_temp)
  tavg_mask <- mask(tavg, occu_temp)
  tavg_mwin <- mask(tavg_win, occu_temp)
  
  #extract average daily prcp and temp
  out <- rbind(raster::extract(prcp_mask, quad_temp, fun = mean, na.rm = T, df = T, weights = T),
               raster::extract(prcp_mask, quad_temp, fun = mean, na.rm = T, df = T, weights = T),
               raster::extract(tavg_mask, quad_temp, fun = mean, na.rm = T, df = T, weights = T))
  out <- out[,-1]
  
  out_win <- rbind(raster::extract(tavg_mwin, gnp_temp, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(tavg_mwin, quad_temp, fun = mean, na.rm = T, df = T, weights = T))
  out_win <- out_win[,-1]
  
  #calc sums of prcp and averages of tavg
  out[1:4, 1:47] <- NA
  out[5:12, -(1:32)] <- NA
  out <- rbind(data.frame(o = rowSums(out[1:8,], na.rm = T)),
               data.frame(o = rowMeans(out[9:12,], na.rm = T)),
               data.frame(o = rowMeans(out_win, na.rm = T))) %>% round(2)
  colnames(out) <- yr
  
  #bind to df
  clim_out <- cbind(clim_out, out[, 1, drop = F])
  
  #clean up 
  rm(quad_temp, prcp, tavg, prcp_mask, tavg_mask, out, occu_temp, tavg_win,tavg_mwin, out_win, gnp_temp)
  removeTmpFiles(h = 0.000001)
}

#write to disk and clean up
#write.csv(clim_out, file = "output/prcp_temp_study_area_quads_2006_2019.csv")
#rm(years, clim_out, yr)

#################
###SNODAS DATA###
#################

#total previous winter snowfall
#average snow depth
#average swe
#oct to apr at study area and quadrant scale

#choose what years you want to extract from
#Oct 2006 - Apr 2007 until Oct 2018 - Apr 2019
years <- 2006:2018

#create output dataframe
sno_out <- data.frame(area = rep(c('whole' ,'sw', 'se', 'nw', 'ne'), 3),
                       variable = rep(c('total_snow_prcp_oct_yr_apr_yrplus1',
                                        'avg_snow_depth_oct_yr_apr_yrplus1',
                                          'avg_swe_oct_yr_apr_yrplus1'), each = 5))

for(yr in years){
  
  #load list of all snodas files
  files <- list.files('snodas/extracted', pattern = '.tif', full.names = T)
  
  #convert to date object
  dates <- str_extract(files, '[:digit:]{8}') %>% ymd
  
  #create date object with dates we want
  target <- seq(ymd(str_c(yr, '1001')), ymd(str_c(yr + 1, '0430')), by = 1)
  
  #mask files based on dates we want
  files <- files[dates %in% target]
  rm(dates, target)
  
  #load raster stacks
  prcp <- stack(str_subset(files, 'prcp'))
  depth <- stack(str_subset(files, 'depth'))
  swe <- stack(str_subset(files, 'swe'))
  
  #transform shapes
  gnp_sno <- spTransform(gnp, crs(prcp))
  quad_sno <- spTransform(quad, crs(prcp))
  
  #reproject occupany layer to match snodas
  occu_sno <- occu %>% raster::aggregate(fact = 10) %>% projectRaster(to = prcp)
  
  #set mask values in occupancy layer
  occu_sno[occu_sno < 0.004] <- NA
  
  #mask values
  prcp_mask <- mask(prcp, occu_sno)
  depth_mask <- mask(depth, occu_sno)
  swe_mask <- mask(swe, occu_sno)
  
  #apply scale correction
  prcp_mask <- prcp_mask/10
  depth_mask <- depth_mask/1000
  swe_mask <- swe_mask/1000
  
  #extract average values for entire winter
  prcp_yr <- rbind(raster::extract(prcp_mask, gnp_sno, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(prcp_mask, quad_sno, fun = mean, na.rm = T, df = T, weights = T)) %>%
    .[, -1]
  
  depth_yr <- rbind(raster::extract(depth_mask, gnp_sno, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(depth_mask, quad_sno, fun = mean, na.rm = T, df = T, weights = T)) %>%
    .[, -1]
  
  swe_yr <- rbind(raster::extract(swe_mask, gnp_sno, fun = mean, na.rm = T, df = T, weights = T),
                   raster::extract(swe_mask, quad_sno, fun = mean, na.rm = T, df = T, weights = T)) %>%
    .[, -1]

  sno_yr <- rbind(data.frame(o = rowSums(prcp_yr, na.rm = T)),
                  data.frame(o = rowMeans(depth_yr, na.rm = T)),
                  data.frame(o = rowMeans(swe_yr, na.rm = T))) %>% round(4)
  colnames(sno_yr) <- str_c(yr)
  
  #cbind to df
  sno_out <- cbind(sno_out, sno_yr[, 1, drop = F])
  
  #clean up
  rm(gnp_sno, quad_sno, occu_sno, prcp_mask, sno_yr, depth_mask, swe_mask,
     prcp, depth, swe, files, prcp_yr, depth_yr, swe_yr)
}

#write to disk and clean up
write.csv(sno_out, file = "output/snow_study_area_quads_2006_2019_oct_apr.csv")
rm(years, sno_out, yr)

