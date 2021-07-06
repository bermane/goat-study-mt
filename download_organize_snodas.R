#load libraries
library(RCurl)
library(rgdal)
library(sp)
library(raster)
library(lubridate)
library(rgeos)
library(tidyverse)
library(R.utils)

#######################
###DOWNLOAD RAW DATA###
#######################

#set download directory
dir <- '/Volumes/SSD/goat_surveys/snodas/raw'

for(yr in 2006:2012){
  url<- str_c("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/", yr, "/")
  mon <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) #reading filenames from ftp-server
  mon <- strsplit(mon, "\n")
  mon = unlist(mon)
  mon <- mon %>% str_subset(pattern = "_")
  
  for(m in mon){
    url2 <- str_c("ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/", yr, "/", m, "/")
    filenames <- getURL(url2, ftp.use.epsv = FALSE, dirlistonly = TRUE) #reading filenames from ftp-server
    filenames <- strsplit(filenames, "\n")
    filenames = unlist(filenames)
    filenames <- filenames %>% str_subset(pattern = ".tar")
    
    for (filename in filenames) {
      download.file(paste(url2, filename, sep = ""), paste(dir, "/", filename,
                                                          sep = ""))
    }
    rm(url2, filenames)
}
rm(url, mon)
}

#######################
###ORGANIZE RAW DATA###
#######################

#set wd
setwd('/Volumes/SSD/goat_surveys')

#load gnp shape
gnp <- readOGR('/Volumes/SSD/goat_surveys/reference/boundary2003.shp')

#denote where raw data is
raw <- '/snodas/raw'

#create temp dir and output files dir
dir.create(str_c(getwd(), raw, '/temp'))
dir.create(str_c(getwd(), raw, '/extracted'))

#get files in dir
files <- list.files(str_c(getwd(), raw), '.tar', full.names = T)

#find leap year files
leap <- str_which(files, '0229')

#remove leap year files
file.remove(files[leap])

#get files in dir
files <- list.files(str_c(getwd(), raw), '.tar', full.names = T)

#check to see if any files are missing
#extract dates from file names and parse as Date object
check <- str_extract(files, '[:digit:]{8}')
check <- ymd(check)

#create sequence along timeseries from 2006 to end of 2019
dates <- seq(ymd("20060101"), ymd(20191231), by = 1)

#print which are missing
#seems these files just don't exist so when I stack the data i'll
#just leave whole bands as NA
dates[!(dates %in% check)]

#we only want oct to april so only extract those months
months <- c(1, 2, 3, 4, 10, 11, 12)
files <- files[month(check) %in% months]

#set to run for three variables
vari <- matrix(c('swe', 'snowdepth', 'snowprcp',
                 'us_ssmv11034', 'us_ssmv11036', 'us_ssmv01025SlL01'), ncol = 2)

#run loop to extract files
for(file in files){
  #untar main file
  untar(file, exdir = str_c(getwd(), raw, '/temp'))
  
  #loop through, unzip, crop, and write .tif for 3 variables
  for(i in 1:nrow(vari)){
    
    #find file for variable
    f <- list.files(str_c(getwd(), raw, '/temp'), pattern = glob2rx(str_c(vari[i,2], '*.dat.gz')), full.names = T)
    f_out <- str_c(vari[i,1], '_', str_extract(file, '[:digit:]{8}'), '.dat')
    gunzip(f[1], destname = str_c(getwd(), raw, '/temp/', f_out))
    
    #create hdr
    #different hdr for before and after oct 1 2013
    if(ymd(str_extract(file, '[:digit:]{8}')) < "2013-10-01"){
      hdr <- c('nrows 3351',
               'ncols 6935',
               'nbands 1',
               'nbits 16',
               'pixeltype signedint',
               'byteorder M',
               'layout dat',
               'ulxmap -124.729583333331703',
               'ulymap 52.871249516804028',
               'xdim 0.00833333333',
               'ydim 0.00833333333')
    }else{
      hdr <- c('nrows 3351',
               'ncols 6935',
               'nbands 1',
               'nbits 16',
               'pixeltype signedint',
               'byteorder M',
               'layout dat',
               'ulxmap -124.733333333333',
               'ulymap 52.8749999999999',
               'xdim 0.00833333333',
               'ydim 0.00833333333')
    }
    
    #write hdr to disk
    writeLines(hdr, str_c(getwd(), raw, '/temp/', str_replace(f_out, '.dat', '.hdr')))
    
    #load in dat file
    sno <- raster(str_c(getwd(), raw, '/temp/', f_out))
    
    #set crs
    crs(sno) <- CRS("+init=epsg:4326")
    
    #reproj gnp outline
    gnp_sno <- spTransform(gnp, crs(sno))
    
    #crop sno and write to extracted folder
    sno <- crop(sno, gnp_sno, filename = str_c(getwd(), raw, '/extracted/', str_replace(f_out, '.dat', '.tif')),
                format = 'GTiff', datatype = 'INT2S', NAflag = -9999)
    
    #clean up
    rm(f, f_out, hdr, gnp_sno, sno)
  }
  #clear temp folder
  invisible(do.call(file.remove, list(list.files(str_c(getwd(), raw, '/temp'), full.names = TRUE))))
}

