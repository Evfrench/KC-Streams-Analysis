## function to extract water quality data from Socrata

library(tidyverse)
library(RSocrata)
library(lubridate)
library(miscTools)
library(ggplot2)
library(forecast)

# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]
#source("https://github.com/Evfrench/KC-Streams-Analysis/blob/main/function_for_fixing_method_change_nutrients.R")
#source("https://github.com/Evfrench/KC-Streams-Analysis/blob/main/lab_chlorophyll_correction.R")


#example
Date<-c('2006-12-15','2006-12-15','2006-12-15')
Value<-c(0.012, 0.1,0.0005)
Parameter<-'Total Phosphorus'

lab_change_correction(Value = Value,Date=Date,Parameter=Parameter)


# get_socrata_data_func <- function(locns = c('0852'),parms = c('Chlorophyll a','Secchi Transparency','Total Suspended Solids'), SiteType = 'Large Lakes'){
get_socrata_data_func <- function(locns = c('0852'),parms = c("Chlorophyll a", "Chlorophyll, Field", "Density", "Dissolved Organic Carbon", "Dissolved Oxygen", 
                                                              "Dissolved Oxygen, Field", "E. coli", "Enterococcus", "Fecal Coliform", "Light Intensity (PAR)", 
                                                              "Surface Light Intensity (PAR)", "Light Transmissivity", "Ammonia Nitrogen", "Nitrite + Nitrate Nitrogen", 
                                                              "Orthophosphate Phosphorus", "Pheophytin a", "pH, Field", "Salinity", "Salinity, Field", "Secchi Transparency", 
                                                              "Silica", "Temperature", "Total Kjeldahl Nitrogen", "Total Nitrogen", "Total Organic Carbon", "Total Phosphorus", 
                                                              "Total Suspended Solids", "Turbidity", "Turbidity, Field", "Aragonite Saturation State", 
                                                              "Calcite Saturation State", "CO₂", "CO₃²⁻", "Dissolved Inorganic Carbon", "fCO₂", "HCO₃⁻", "pCO₂", "pH, total scale", "Revelle Factor", "Total Alkalinity", "Biochemical Oxygen Demand", "Conductivity", "Conductivity, Field", "Dissolved Oxygen Saturation, Field", "Fecal Streptococcus", "Hardness, Calc", "Nitrate Nitrogen", "Nitrite Nitrogen", "Organic Nitrogen", "Sampling Method", "Settleable Solids, Gravimetric", "Storm Or Non-Storm", "Total Coliform", "Total Hydrolyzable Phosphorus", "Volatile Suspended Solids", "pH", "BGA PC, Field"
), SiteType = 'Large Lakes'){
  
  loc_url_portal<-'https://data.kingcounty.gov/resource/wbhs-bbzf.csv'
  locs<-read.socrata(loc_url_portal) %>%
    transmute(SiteName=sitename,
              Locator=locator,
              lng=as.numeric(longitude),
              lat=as.numeric(latitude),
              SiteTypeName=site_type,
              Area=area) %>%
    filter(SiteTypeName==SiteType&!is.na(lng)) 
  
  
  # Limit to central lake locations and two long-term locations (0540 at Montlake Cut and 0804 at north end of Lake Washington)
  # locs <- filter(locs,Locator %in% c("0612","0852","A522","0804","0540"))
  #  locs <- filter(locs,Locator %in% c("A522"))
  locs <- filter(locs,Locator %in% locns)
  #locs <- filter(locs,Locator %in% c("0512"))
  
  data_url_start<-'https://data.kingcounty.gov/resource/vwmt-pvjw.csv' #entire wq portal
  download_query<-paste0("?$where=",
                         "(",paste0("locator='",locs$Locator,"'",collapse=' OR '),')',
                         # " AND (",paste0("parameter='",c('Temperature','Conductivity, Field','Chlorophyll a','Secchi Transparency','Total Suspended Solids'),"'",collapse=' OR '),')',
                         " AND (",paste0("parameter='",parms,"'",collapse=' OR '),')',
                         " AND ","NOT qualityid = 4") # qualityid = 4 for rejected data
  
  data_out<-read.socrata(paste0(data_url_start,download_query)) %>% filter(qualityid != 9) %>% # also remove missing (not data values) data (qualityid = 9)
    # filter(!is.na(depth)) %>% 
    transmute(CollectDate=collect_datetime,
              Year=year(CollectDate),
              Month=month(CollectDate),
              LabSampleNum=sample_number,
              Locator=locator,
              Depth=depth,
              Parameter=parameter,
              Value=value,#if_else(is.na(overridevalue),value,overridevalue),
              Units=units,
              Qualifier=lab_qualifier,
              MDL=mdl,
              RDL=rdl,
              Text=textvalue) 
  
  # fix some bad Locators
  data_out$Locator <- with(data_out,(ifelse(Locator=='612','0612',
                                            ifelse(Locator=='852','0852',
                                                   ifelse(Locator=='512','0512',
                                                          
                                                          ifelse(Locator=='826','0826',
                                                                 ifelse(Locator=='831','0831',
                                                                        ifelse(Locator=='804','0804',
                                                                               Locator))))))))
  
  #### to correctly handdle nondetects (i.e., NADA package) replacing <MDLs with MDL and creating nondetect flag
  data_out$Value_orig <- data_out$Value
  data_out$det_Flag <- with(data_out,ifelse(is.na(data_out$Value),TRUE,FALSE))
  data_out$Value <- with(data_out,ifelse(is.na(data_out$Value),MDL,Value))
  
  # any NAs left are nondetects with no MDLs
  
  tmp <- data_out[is.na(data_out$Value),]
  
  
  #### adjustments for lab method changes (nutrients and chlorophyll)
  ### note this only changes results reported prior to 2007 for nutrients and prior to July 1996 for chlorophyll a data
  data_out$Value <- lab_change_correction(Value = data_out$Value,Date=data_out$CollectDate,Parameter=data_out$Parameter)
  data_out$Value <- lab_chlorophyll_correction(Value = data_out$Value,Date=data_out$CollectDate,Parameter=data_out$Parameter)
  
  return(data_out)
  
  
}

#Query Socrata for the chosen site records, in this case Green River, Cedar River, and Issaquah Creek
GrCeIsRiverData<- get_socrata_data_func(locns = c('A319','0438','0631'),parms = c("Chlorophyll a", "Chlorophyll, Field", "Density", "Dissolved Organic Carbon", "Dissolved Oxygen", 
                                                                                  "Dissolved Oxygen, Field", "E. coli", "Enterococcus", "Fecal Coliform", "Light Intensity (PAR)", 
                                                                                  "Surface Light Intensity (PAR)", "Light Transmissivity", "Ammonia Nitrogen", "Nitrite + Nitrate Nitrogen", 
                                                                                  "Orthophosphate Phosphorus", "Pheophytin a", "pH, Field", "Salinity", "Salinity, Field", "Secchi Transparency", 
                                                                                  "Silica", "Temperature", "Total Kjeldahl Nitrogen", "Total Nitrogen", "Total Organic Carbon", "Total Phosphorus", 
                                                                                  "Total Suspended Solids", "Turbidity", "Turbidity, Field", "Aragonite Saturation State", 
                                                                                  "Calcite Saturation State", "CO₂", "CO₃²⁻", "Dissolved Inorganic Carbon", "fCO₂", "HCO₃⁻", "pCO₂", "pH, total scale", "Revelle Factor", "Total Alkalinity", "Biochemical Oxygen Demand", "Conductivity", "Conductivity, Field", "Dissolved Oxygen Saturation, Field", "Fecal Streptococcus", "Hardness, Calc", "Nitrate Nitrogen", "Nitrite Nitrogen", "Organic Nitrogen", "Sampling Method", "Settleable Solids, Gravimetric", "Storm Or Non-Storm", "Total Coliform", "Total Hydrolyzable Phosphorus", "Volatile Suspended Solids", "pH", "BGA PC, Field"
), SiteType = 'Streams and Rivers')

#put the data in log scale
GrCeIsRiverData$logData <- log(GrCeIsRiverData$Value)

# Loop through and generate our additional columns
# issue is properly assigning values to the columns
SampleID <- unique(GrCeIsRiverData$LabSampleNum)
newframe <- data.frame(SampleID)

for (id in SampleID){
  for (param in unique(GrCeIsRiverData$Parameter)){
    # TODO: Given SampleID and PArameter name, fetch each of the values in [""]
    # Figure out how to generate column names (the paste0(param,*)) parts
    #potential: copy both for loops, and have one just setup the dataframe and the other fill it in
    newFrame[id]$str(param) = GrCeIsRiverData %>% filter(LabSampleNum==id, Parameter==param)[0]["Value"]
    newFrame[id]$paste0(param,'_log') = GrCeIsRiverData %>% filter(LabSampleNum== id, Parameter==param)[0]["logData"]
    newFrame[id]$paste0(param,'_units') = GrCeIsRiverData %>% filter(LabSampleNum == id, Parameter==param)[0]["Units"]
    newFrame[id]$paste0(param,'_mdl') = GrCeIsRiverData %>% filter(LabSampleNum == id, Parameter==param)[0]["MDL"]
    newFrame[id]$paste0(param,'_rdl') = GrCeIsRiverData %>% filter(LabSampleNum == id, Parameter==param)[0]["RDL"]
    newFrame[id]$paste0(param,'_text') = GrCeIsRiverData %>% filter(LabSampleNum == id, Parameter==param)[0]["Text"]
    newFrame[id]$paste0(param,'_orig') = GrCeIsRiverData %>% filter(LabSampleNum == id, Parameter==param)[0]["Value_orig"]
  }
}



#Temperature Check, in log space
GreenTemp <- GrCeIsRiverData %>% filter(Locator=="A319",Parameter=="Temperature")
GreenTemp %>%
  ggplot(aes(x=CollectDate, y=Value)) +
  geom_line() +
  ggtitle("Green River Temperature")

#Create Dissolved Oxygen Plots
GreenDO <- GrCeIsRiverData %>% filter(Locator=="A319",Parameter=="Dissolved Oxygen" | Parameter=="Dissolved Oxygen, Field")
CedarDO <- GrCeIsRiverData %>% filter(Locator=="0438",Parameter=="Dissolved Oxygen" | Parameter=="Dissolved Oxygen, Field")
IssaquahDO <- GrCeIsRiverData %>% filter(Locator=="0631",Parameter=="Dissolved Oxygen" | Parameter=="Dissolved Oxygen, Field")

GreenDO %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Green River DO")

CedarDO %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Cedar River DO")

IssaquahDO %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Issaquah Creek DO")

#Create Plots of Fecal Coliform
GreenFecColi <- GrCeIsRiverData %>% filter(Locator=="A319",Parameter=="Fecal Coliform")
CedarFecColi <- GrCeIsRiverData %>% filter(Locator=="0438",Parameter=="Fecal Coliform")
IssaquahFecColi <- GrCeIsRiverData %>% filter(Locator=="0631",Parameter=="Fecal Coliform")

GreenFecColi %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Green River Fecal Coliform")

CedarFecColi %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Cedar River Fecal Coliform")

IssaquahFecColi %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Issaquah Creek Coliform")

#Create TP Plots
GreenTP <- GrCeIsRiverData %>% filter(Locator=="A319", Parameter=="Total Phosphorus")
CedarTP <- GrCeIsRiverData %>% filter(Locator=="0438", Parameter=="Total Phosphorus")
IssaquahTP <- GrCeIsRiverData %>% filter(Locator=="0631", Parameter=="Total Phosphorus")

GreenTP %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Green River Total Phosphorus")

CedarTP %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Cedar River Total Phosphorus")

IssaquahTP %>%
  ggplot(aes(x=CollectDate, y=logData)) +
  geom_line() +
  ggtitle("Issaquah Creek Total Phosphorus")
