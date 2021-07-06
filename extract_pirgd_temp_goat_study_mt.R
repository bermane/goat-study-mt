#code to stack and crop emodis PIRGd calculated from SOSd and PGSd
#calculate mean, max, and min pirgd within each goat survey viewshed
#calculate days after pirgd for each survey
#add a temperature component for each survey

#load packages
library(rgdal)
library(sp)
library(raster)
library(lubridate)
library(rgeos)
library(hms)
library(chillR)
library(tidyverse)

#set wd
setwd('/Volumes/SSD/goat_surveys')

#load montana shapefile
mt <- readOGR('/Volumes/SSD/Results_Ethan/QGIS/montana.shp')
#transform to CRS of emodis
mt <- spTransform(mt, "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

#load emodis data
#for some reason having problems changing values of raster stack... try one by one
#set empty pirgd stack and load sosd and pgsd file names
#pirgd <- brick()
#sosd_files <- list.files('./emodis/sosd/', '.bsq', full.names = T)
#pgsd_files <- list.files('./emodis/pgsd/', '.bsq', full.names = T)

#for(i in 1:length(sosd_files)){
#load sosd and pgsd raster
#sosd <- raster(sosd_files[i])
#pgsd <- raster(pgsd_files[i])

#crop them to wy
#sosd<- crop(sosd, mt)
#pgsd <- crop(pgsd, mt)

#mask bad values
#sosd[sosd %in% c(-1000, 1000)] <- NA
#pgsd[pgsd %in% c(-1000, 1000)] <- NA

#calculate pirgd and save to raster stack
#pirgd_hold <- sosd + (pgsd - sosd)/2

#if(i == 1){pirgd <- brick(pirgd_hold)} else{pirgd <- addLayer(pirgd, pirgd_hold)}

#remove raster files
#rm(sosd, pgsd, pirgd_hold)
#}

#change names of layers to years
#names(pirgd) <- 2001:2018

#write pirgd stack to disk
#writeRaster(pirgd, './emodis/pirgd/pirgd_mt_2001_2018.tif', format = "GTiff")

#remove and reload brick for summary stats processing
#rm(pirgd)
pirgd <- brick('./pirgd_mt_2001_2018.tif')

#change names of layers to years
names(pirgd) <- 2001:2018

#load viewshed and goat survey box and transform to CRS of emodis
viewshed <- readOGR('reference/Viewsheds_clipped_by_lakes.shp') %>%
  spTransform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>% gBuffer(byid=TRUE, width=0)
goatsurvey <- readOGR('reference/2009GoatSurvey_Box.shp') %>%
  spTransform("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") %>% gBuffer(byid=TRUE, width=0)

#combine shapes of separate parts of viewshead into single shapes
viewshed <- raster::aggregate(viewshed, by = "NAME_1")

#choose what years you want to extract from
years <- 2008:2018

#create output dataframe
viewshed_df <- data.frame(viewshed@data)

#extract values for individual years
for(i in 1:length(years)){
  #load in corrent year of pirgd
  pirgd_yr <- pirgd[[which(names(pirgd) %in% paste0("X", years[i]))]]
  
  #extract values and write to DF for viewshed
  #variance and range added
  mean_vs <- raster::extract(pirgd_yr, viewshed, fun = mean, na.rm = T, df = T)
  mean_vs[,2] <- round(mean_vs[,2])
  max_vs <- raster::extract(pirgd_yr, viewshed, fun = max, na.rm = T, df = T)
  min_vs <- raster::extract(pirgd_yr, viewshed, fun = min, na.rm = T, df = T)
  var_vs <- raster::extract(pirgd_yr, viewshed, fun = var, na.rm = T, df = T)
  var_vs[,2] <- round(var_vs[,2], 2)
  colnames(mean_vs) <- c("ID", paste0("mean_pirgd_", years[i]))
  colnames(max_vs) <- c("ID", paste0("max_pirgd_", years[i]))
  colnames(min_vs) <- c("ID", paste0("min_pirgd_", years[i]))
  colnames(var_vs) <- c("ID", paste0("var_pirgd_", years[i]))
  viewshed_df <- cbind(viewshed_df, mean_vs[,2,drop = F], max_vs[,2,drop = F], min_vs[,2,drop = F], var_vs[,2,drop = F])
  
  #clean up
  rm(pirgd_yr, mean_vs, max_vs, min_vs, var_vs)
  #rm(mean_gs, max_gs, min_gs)
}

#write.csv(viewshed_df, file = "./pirgd_viewsheds_mt_2008_2018_mean_max_min_var.csv")

###EXTRACT VALUES FOR EACH GOAT SURVEY DATE AND LOCATION###
#load viewshed and site name key sheet
site_key <- read.csv('reference/Viewshed_variables_SG13Mar20.csv')
site_key <- site_key[,colnames(site_key) %in% c('SiteName', 'vs')]
site_names <- as.character(site_key$SiteName)
names(site_names) <- site_key$vs
rm(site_key)

#load extracted viewshed csv
goat_pirgd <- read.csv('output/pirgd_viewsheds_mt_2008_2018.csv')
goat_pirgd <- goat_pirgd[,-1]
goat_pirgd$NAME_1 <- as.character(goat_pirgd$NAME_1)

#create a new column and add the full site names as used in surveys
goat_pirgd$site_name <- goat_pirgd$NAME_1 %>% recode(., !!!site_names)

#load survey data
surv <- read.csv('reference/GoatSurveyTimes_MJYedit.csv')
surv[surv == ""] <- NA
surv$SiteName <- as.character(surv$SiteName)

#check for matching names in goat survey data and polygons
goatsurvey_names <- goat_pirgd$site_name %>% unique %>% sort
surv_names <- surv$SiteName %>% unique %>% sort

#both have same length so find discrepancies
surv_names[!surv_names %in% goatsurvey_names]
goatsurvey_names[!goatsurvey_names %in% surv_names]

#change names in goatsurvey polygons to match
#goat_pirgd$Name[goat_pirgd$Name == "Beaver Woman"] <- "Beaver Woman Lake"
#goat_pirgd$Name[goat_pirgd$Name == "Ole Lookout"] <- "Ole Creek"
#goat_pirgd$Name[goat_pirgd$Name == "Pitimaken Pass"] <- "Pitamakin Pass"
#goat_pirgd$Name[goat_pirgd$Name == "Siyeh Pass"] <- "Siyeh Pass Loop"
#goat_pirgd$Name[goat_pirgd$Name == "Triple Divide"] <- "Triple Divide Pass"

#RE CHECK FOR MIS MATCHES#
#check for matching names in goat survey data and polygons
goatsurvey_names <- goat_pirgd$site_name %>% unique %>% sort
surv_names <- surv$SiteName %>% unique %>% sort

#both have same length so find discrepancies
surv_names[!surv_names %in% goatsurvey_names]
goatsurvey_names[!goatsurvey_names %in% surv_names]

#organize dates in surv
surv$Date <- surv$Date %>% mdy
surv$year <- year(surv$Date)
surv$doy <- yday(surv$Date)

#pre allocate columns for output
surv$days_after_mean_pirgd <- NA
surv$days_after_max_pirgd <- NA
surv$days_after_min_pirgd <- NA

#loop through sites and years to extract pirgd
for(site in unique(surv$SiteName)){
  for(yr in unique(surv$year)){
    #no pirgd for 2019
    if(yr == 2019){} else{
      #subset polygon data
      g_pirgd <- goat_pirgd[goat_pirgd$site_name %in% site,] %>% .[,c(grep("site_name", colnames(goat_pirgd)), grep(as.character(yr), colnames(goat_pirgd)))]
      
      #calc days after mean
      surv$days_after_mean_pirgd[surv$SiteName == site & surv$year == yr] <- 
        surv$doy[surv$SiteName == site & surv$year == yr] - g_pirgd[,grep(paste0("mean_pirgd_", as.character(yr)), colnames(g_pirgd))]
      
      #calc days after max
      surv$days_after_max_pirgd[surv$SiteName == site & surv$year == yr] <- 
        surv$doy[surv$SiteName == site & surv$year == yr] - g_pirgd[,grep(paste0("max_pirgd_", as.character(yr)), colnames(g_pirgd))]
      
      #calc days after min
      surv$days_after_min_pirgd[surv$SiteName == site & surv$year == yr] <- 
        surv$doy[surv$SiteName == site & surv$year == yr] - g_pirgd[,grep(paste0("min_pirgd_", as.character(yr)), colnames(g_pirgd))]
    }
  }
}

rm(g_pirgd)

#export csv
#write.csv(surv, file = '/Users/Ediz/Team Braintree Dropbox/Ethan Berman/USGS/goat pirgd/outputs/GoatSurveyDates_PIRGd.csv')

###LOAD AND MANIPULATE TEMPERATURE FROM DAYMET TO EXTRACT HOURLY VALUES###
###INCLUDE MAX, MIN, AND MEAN FOR EACH SURVEY DATE AS WELL###

#update viewshed shapefile with site name column
viewshed$site_name <- viewshed$NAME_1 %>% recode(., !!!site_names)

#extract daymet max and min temp within each area and for each relavent date
#add columns for max and min temp to survey data
surv$tmax <- NA
surv$tmin <- NA

for(yr in unique(surv$year)){
  #no pirgd for 2019
  #load temp file years
  tmax_f <- list.files(path = 'temp', pattern = str_c('tmax_', yr), full.names = T)
  tmin_f <- list.files(path = 'temp', pattern = str_c('tmin_', yr), full.names = T)
  
  if(length(tmax_f) == 2 & length(tmin_f) == 2) {print(str_c('two files for ', yr))} else{
    print(str_c('missing files for ', yr))
  }
  
  #load temp years
  tmax <- merge(stack(tmax_f[1]), stack(tmax_f[2]))
  tmin <- merge(stack(tmin_f[1]), stack(tmin_f[2]))
  rm(tmax_f, tmin_f)
  
  #transform viewshed crs
  viewshed <- spTransform(viewshed, crs(tmax))
  
    for(site in unique(surv$SiteName)){
      surv_hold <- surv[surv$SiteName == site & surv$year == yr,]
      if(NROW(surv_hold) != 0){
      #loop through rows to extract data
      for(i in 1:NROW(surv_hold)){
        surv_hold$tmax[i] <- raster::extract(tmax[[surv_hold$doy[i]]], viewshed[viewshed$site_name == site,], 
                                     fun = mean, na.rm = T, df = F, weights = T) %>% as.numeric
        surv_hold$tmin[i] <- raster::extract(tmin[[surv_hold$doy[i]]], viewshed[viewshed$site_name == site,], 
                                     fun = mean, na.rm = T, df = F, weights = T) %>% as.numeric
        
        surv[surv$SiteName == site & surv$year == yr,] <- surv_hold
      }}
      rm(surv_hold)
    }
    rm(tmax, tmin)
}

#calculate mean temp
surv$tmean <- (surv$tmax + surv$tmin)/2

#derive optimal hour of survey
#save df as surv2 just to have a backup
surv2 <- surv

#first coerce all time columns into time objects then numeric, so we can find max and min values

surv$N.Start <- surv$N.Start %>% hm %>% hms %>% as.numeric
surv$N.End <- surv$N.End %>% hm %>% hms %>% as.numeric
surv$S.Start <- surv$S.Start %>% hm %>% hms %>% as.numeric
surv$S.End <- surv$S.End %>% hm %>% hms %>% as.numeric
surv$DetectTime <- surv$DetectTime %>% hm %>% hms %>% as.numeric
surv$ManualTime <- surv$ManualTime %>% hm %>% hms %>% as.numeric

#create hour column
surv$hour <- NA

#next denote how to extract time
for(i in 1:NROW(surv)){
  if(sum(is.na(surv$N.Start[i]), is.na(surv$N.End[i]), is.na(surv$S.Start[i]), is.na(surv$S.End[i])) <= 2){
    surv$hour[i] <- (max(surv$N.Start[i], surv$N.End[i], surv$S.Start[i], surv$S.End[i], na.rm = T) + 
                       min(surv$N.Start[i], surv$N.End[i], surv$S.Start[i], surv$S.End[i], na.rm = T))/2
  } else if(sum(is.na(surv$N.Start[i]), is.na(surv$N.End[i]), is.na(surv$S.Start[i]), is.na(surv$S.End[i])) == 3){
    surv$hour[i] <- max(surv$N.Start[i], surv$N.End[i], surv$S.Start[i], surv$S.End[i], na.rm = T)
  } else if(sum(is.na(surv$N.Start[i]), is.na(surv$N.End[i]), is.na(surv$S.Start[i]), is.na(surv$S.End[i])) == 4){
    surv$hour[i] <- max(surv$DetectTime[i], surv$ManualTime[i], na.rm = T)
  }
}

#check for no missing hour values
sum(is.na(surv$hour))

#find centroid of each polygon and convert to lat lon
cent <- gCentroid(viewshed, byid = T) %>% spTransform(., "+init=epsg:4326")
cent$site_name <- viewshed$site_name

#add latitude to surv data
surv <- surv %>% mutate(lat = apply(surv, 1, function(x) cent@coords[cent$site_name == x['SiteName'],2]))

#calculate hourly temperature and extract value for each survey
#we need days before and after date of interest so makes sense to just 
#calculate on entire time series for each site location
#and use surv data to extract what we need

#first create time series using list so only have to load each year once
site_temp <- list()

for(i in 1:length(unique(surv$SiteName))){
  site_temp[[i]] <- data.frame(SiteName = unique(surv$SiteName)[i], Year = rep(2008:2019, each = 365),
                               JDay = rep(1:365, times = 12), Tmax = NA, Tmin = NA)
  names(site_temp)[i] <- unique(surv$SiteName)[i] %>% as.character
}

#load tmax and tmin data into lists
for(yr in 2008:2019){
  
  #load temp file years
  tmax_f <- list.files(path = 'temp', pattern = str_c('tmax_', yr), full.names = T)
  tmin_f <- list.files(path = 'temp', pattern = str_c('tmin_', yr), full.names = T)
  
  if(length(tmax_f) == 2 & length(tmin_f) == 2) {print(str_c('two files for ', yr))} else{
    print(str_c('missing files for ', yr))
  }
  
  #load temp years
  tmax <- merge(stack(tmax_f[1]), stack(tmax_f[2]))
  tmin <- merge(stack(tmin_f[1]), stack(tmin_f[2]))
  rm(tmax_f, tmin_f)
  
  #transform viewshed crs
  viewshed <- spTransform(viewshed, crs(tmax))
  
  for(i in 1:length(unique(surv$SiteName))){
    site_temp[[as.character(unique(surv$SiteName)[i])]]$Tmax[site_temp[[i]]$Year == yr] <- 
      raster::extract(tmax, viewshed[viewshed$site_name == as.character(unique(surv$SiteName)[i]),], 
            fun = mean, na.rm = T, df = F, weights = T) %>% as.numeric
    site_temp[[as.character(unique(surv$SiteName)[i])]]$Tmin[site_temp[[i]]$Year == yr] <- 
      raster::extract(tmin, viewshed[viewshed$site_name == as.character(unique(surv$SiteName)[i]),], 
                                                              fun = mean, na.rm = T, df = F, weights = T) %>% as.numeric
  }
}

#create hourly temps throughout lists
#back up lists
site_temp2 <- site_temp

#run using lapply
site_temp <- lapply(site_temp, function(x) make_hourly_temps(surv$lat[surv$SiteName == as.character(x$SiteName[1])][1],
                                                             x))
#extract hourly values we want
#convert hour column back to hms
surv$hour <- surv$hour %>% hms

#change hour column name to time
surv <- surv %>% rename(time = hour)

#generate rounded hour column
surv$hour <- surv$time %>% hour

#extract hourly values -- DIDN'T WORK WITH APPLY???
#surv <- surv %>% mutate(thour = apply(surv, 1, function(x) {
#  site_temp[[x['SiteName']]] %>% filter(Year == x['year'] & JDay == x['doy']) %>%
#    select(str_c('Hour_', x['hour']) %>% str_squish) %>% as.numeric
#}))

#extract hourly values -- easy and fast for loop
for(i in 1:NROW(surv)){
  surv$thour[i] <- site_temp[[surv[i,'SiteName']]] %>% filter(Year == surv[i,'year'] & JDay == surv[i,'doy']) %>%
    select(str_c('Hour_', surv[i,'hour']) %>% str_squish) %>% as.numeric
}

#coerce all time columns back into time objects
surv$N.Start <- surv$N.Start %>% hms
surv$N.End <- surv$N.End %>% hms
surv$S.Start <- surv$S.Start %>% hms
surv$S.End <- surv$S.End %>% hms
surv$DetectTime <- surv$DetectTime %>% hms
surv$ManualTime <- surv$ManualTime %>% hms

#clean up df
surv$tmax <- surv$tmax %>% round(digits = 2)
surv$tmin <- surv$tmin %>% round(digits = 2)
surv$tmean <- surv$tmean %>% round(digits = 2)
surv$thour <- surv$thour %>% round(digits = 2)

#write csv
#write.csv(surv, file = "output/GoatSurveyDates_PIRGd_TEMP.csv")
