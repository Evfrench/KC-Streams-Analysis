library(dataRetrieval)
library(lubridate)
library(dplyr)
library(readr)
library(miscTools)

gages<-c(12113000,12112600,12121600,12113390)

for(gage in gages){
  usgs.flow1<-readNWISdv(gage,'00060',statCd = c('00003'))
  if(nrow(usgs.flow1)>0) usgs.flow1<-usgs.flow1[,c(2,3,4)] else next
  if(!exists('usgs.flow')){usgs.flow<-usgs.flow1 
  next
  }
  usgs.flow<-rbind(usgs.flow,usgs.flow1)
}

colnames(usgs.flow)<-c('SITE_CODE','Date','AveQ')


usgs.flow$Year<-year(usgs.flow$Date)
usgs.flow$Month<-month(usgs.flow$Date)
usgs.flow$Week<-week(usgs.flow$Date)


site_details<-readNWISsite(gages)[,c('site_no','station_nm','dec_lat_va', 'dec_long_va')]
colnames(site_details)<-c('SITE_CODE','SITE_NAME','LAT','LON')

usgsflow <- right_join(site_details,usgs.flow, by="SITE_CODE")

cache_name = paste0('./data_cache/cache_daily_flows-',paste0(gages,collapse='-'),'-dataset.csv')
write_csv(usgsflow, cache_name, col_name=TRUE)
