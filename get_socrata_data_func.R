## function to extract water quality data from Socrata

library(tidyverse)
library(RSocrata)
library(lubridate)
library(miscTools)
library(ggplot2)
library(forecast)
library(readr)

# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]

# Checks for the 'Chlorophyll a' parameter
source("lab_chlorophyll_correction.R")

# Checks the following parameters:
# Total Phosphorus, Total Nitrogen, Orthophosphate Phosphorus, Nitrite + Nitrate Nitrogen
source("lab_nutrient_correction.R")

# example of correcting data
Date<-c('2006-12-15','2006-12-15','2006-12-15')
Value<-c(0.012, 0.1,0.0005)
Parameter<-'Total Phosphorus'
# Function is pass by reference (edits variables in place)
lab_nutrient_correction(Value = Value,Date=Date,Parameter=Parameter)

# default parameters
default_data_parms = c(
    "Ammonia Nitrogen", 
    "Chlorophyll a", "Chlorophyll, Field", "Density", 
    "Dissolved Organic Carbon", "Total Organic Carbon", "Dissolved Inorganic Carbon", "Revelle Factor",
    "Nitrate Nitrogen", "Nitrite Nitrogen", "Organic Nitrogen",
    "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
    "Dissolved Oxygen", "Dissolved Oxygen, Field", "Dissolved Oxygen Saturation, Field", "Biochemical Oxygen Demand",
    "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus", 
    "Pheophytin a",
    "CO₂", "CO₃²⁻", "fCO₂", "HCO₃⁻", "pCO₂", 

    "BGA PC, Field", "Total Coliform", "E. coli", "Enterococcus", 
    "Fecal Coliform", "Fecal Streptococcus",

    "Aragonite Saturation State", "Calcite Saturation State",
    "Conductivity", "Conductivity, Field",
    "pH, Field", "pH, total scale",
    "Salinity", "Salinity, Field",
    "Total Alkalinity", 
    "Secchi Transparency",
    "Light Intensity (PAR)", "Surface Light Intensity (PAR)", "Light Transmissivity",
    "Silica",
    "Hardness, Calc",
    "Temperature",
    "Total Suspended Solids", "Volatile Suspended Solids", "Settleable Solids, Gravimetric", 
    "Turbidity", "Turbidity, Field",

    "Sampling Method", 
    "Storm Or Non-Storm"   
)

#
# Reformats the data for easier referencing
# Performs the following transformations:
# - Adds a logData column that stores log of reading value
# - Creates a row for each LabSampleNum
# - Adds multiple columns for each parameter, condensing the data from a LabSampleNum x Parameter format to just LabSampleNum
normalize_water_quality_data_parameters <- function(input_data = data.frame()) {
  # Store the data on the log scale
  input_data$logData <- log(input_data$Value)
  # Start By building a new data frame
  # Use the LabSampleNums as the primary key
  SampleID = unique(input_data$LabSampleNum)
  newFrame <- data.frame(SampleID)

  # Add some additional fields we care about
  newFrame <- mutate(newFrame,
    "CollectDate" = as.POSIXct("2999-01-01"),
    "Year" = 0,
    "Month" = 0,
    "Locator" = ""
  )
  # For each Parameter type, we want to create a number of new columns
  ParameterVals = unique(input_data$Parameter)
  for (param in ParameterVals) {
    # Replace spaces with underscores for column naming
    normalized_param = gsub(" ", "_", param)
    # Add a number of columns, and set to sensible, empty defaults
    # Acts on all rows at once
    newFrame <- mutate(newFrame,
        "{normalized_param}" := 0,
        "{paste0(normalized_param,'_log')}" := 0,
        "{paste0(normalized_param,'_units')}" := "",
        "{paste0(normalized_param,'_mdl')}" := NA,
        "{paste0(normalized_param,'_rdl')}" := NA,
        "{paste0(normalized_param,'_text')}" := "",
        "{paste0(normalized_param,'_orig')}" := 0
      )
  }  
  
  # Now we start filling in data values

  # For each SampleID, we fill in parameter values
  for (id in SampleID){
    for (param in unique(input_data$Parameter)){
      # Normalize column name same as above
      normalized_param = gsub(" ", "_", param)
      # Fetch parameter row for sampleID  
      prow <- filter(input_data,LabSampleNum==id, Parameter==param)
      # If we don't get anything, continue
      if (nrow(prow) == 0) { next }
      # For each of the meta-parameter values, we select the row id using `newFrame$SameplID==id`
      # Set the column by pulling the corresponding value from the prow variable
      newFrame[newFrame$SampleID==id,"CollectDate"] = prow["CollectDate"]
      newFrame[newFrame$SampleID==id,"Year"] = prow["Year"]
      newFrame[newFrame$SampleID==id,"Month"] = prow["Month"]
      newFrame[newFrame$SampleID==id,"Locator"] = prow["Locator"]        
      newFrame[newFrame$SampleID==id,normalized_param] = prow["Value"]
      newFrame[newFrame$SampleID==id,paste0(normalized_param,'_log')] = prow["logData"]
      newFrame[newFrame$SampleID==id,paste0(normalized_param,'_units')] = prow["Units"]
      newFrame[newFrame$SampleID==id,paste0(normalized_param,'_mdl')] = prow["MDL"]
      newFrame[newFrame$SampleID==id,paste0(normalized_param,'_rdl')] = prow["RDL"]
      newFrame[newFrame$SampleID==id,paste0(normalized_param,'_text')] = prow["Text"]
      #newFrame[newFrame$SampleID==id,paste0(normalized_param,'_orig')] = prow["Value_orig"]
    }
  }
  return(newFrame)
}



#
# Reformats the data for easier referencing
# Performs the following transformations:
# - Adds a logData column that stores log of reading value
# - Creates a row for each LabSampleNum
# - Adds multiple columns for each parameter, condensing the data from a LabSampleNum x Parameter format to just LabSampleNum
generate_egret_sample_from_water_quality_data <- function(input_data = data.frame(), parameters) {  
  # Normalize parameter names so they match the column names in input_data
  normalized_params = list(gsub(" ", "_", unique(parameters)))
  # create our return dict
  egret_normalized = list(normalized_params)
  # Go through each param, and compile/generate sample data 
  for (param in normalized_params){
    egret_normalized[param] = data.frame(
                        Date = as.Date(input_data$CollectDate),
                        ConcLow = input_data[param], 
                        ConcHigh = input_data[normalized_param], 
                        Uncen = (input_data[normalized_param] * 0 + 1),
                        ConcAve = input_data[normalized_param],
                        Julian = (input_data[normalized_param] * 0),
                        Month = strftime(input_data$CollectDate, format="%m"),
                        Day = strftime(input_data$CollectDate, format="%d"),
                        DecYear = decimal_date(input_data$CollectDate),
                        MonthSeq = (input_data[normalized_param] * 0),
                        SinDY = sin(2*pi*decimal_date(input_data$CollectDate)),
                        CosDY = cos(2*pi*decimal_date(input_data$CollectDate))
    )
  }
  
  return(egret_normalized)
}

#
# Define function that fetches King County Water Quality data based on location
# First fetches metadata about locations - https://data.kingcounty.gov/Environment-Waste-Management/WLRD-Sites/wbhs-bbzf
# Then fetches the passed parameters for the locations - https://data.kingcounty.gov/Environment-Waste-Management/Water-Quality/vwmt-pvjw
#
# Caches results, where passing the same locations will use cached data
# Does not detect different parms value.
# To 'clear' cache, just rename or delete the files located in `./data_cache`
#
# Example call:
#   get_socrata_data_func <- function(locns = c('0852'),parms = c('Chlorophyll a','Secchi Transparency','Total Suspended Solids'), SiteType = 'Large Lakes'){
#
get_socrata_data_func <- function(locns = c('0852'),
  parms = default_data_parms,
  SiteType = 'Large Lakes') {
  
  # Start by fetching location data
  # https://data.kingcounty.gov/Environment-Waste-Management/WLRD-Sites/wbhs-bbzf
  loc_url_portal<-'https://data.kingcounty.gov/resource/wbhs-bbzf.csv'
  cache_name = './data_cache/cache_WLRD_location_dataset.csv'
  # Rename cache file to re-fetch data
  if(file.exists(cache_name)) {
    locs <- read_csv(cache_name)
  } else {
    # save df as csv for later
    locs <- read.socrata(loc_url_portal)
    write_csv(locs, cache_name, col_name=TRUE)
  }

  locs <- (locs %>%
    transmute(SiteName=sitename,
              Locator=locator,
              lng=as.numeric(longitude),
              lat=as.numeric(latitude),
              SiteTypeName=site_type,
              Area=area) %>%
    filter(SiteTypeName==SiteType&!is.na(lng)) 
  )
  
  # Limit to central lake locations and two long-term locations (0540 at Montlake Cut and 0804 at north end of Lake Washington)
  # locs <- filter(locs,Locator %in% c("0612","0852","A522","0804","0540"))
  # locs <- filter(locs,Locator %in% c("A522"))
  # locs <- filter(locs,Locator %in% c("0512"))
  locs <- filter(locs,Locator %in% locns)
  # Do each location individually, for ease of caching
  # Base cache name on location
  cache_name = paste0('./data_cache/cache_water_quality-',paste0(locs$Locator,collapse='-'),'-dataset.csv')
  
  if(file.exists(cache_name)) {
    data_out <- read_csv(cache_name)
  } else {
      # Fetching Puget Sound Water Quality data
      # https://data.kingcounty.gov/Environment-Waste-Management/Water-Quality/vwmt-pvjw
    data_url_start<-'https://data.kingcounty.gov/resource/vwmt-pvjw.csv' #entire wq portal
    download_query<-paste0("?$where=",
                          "(",paste0("locator='",locs$Locator,"'",collapse=' OR '),')',
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
                      Text=textvalue
            )

          
    
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
    data_out$Value <- lab_nutrient_correction(
                        Value = data_out$Value,
                        Date=data_out$CollectDate,
                        Parameter=data_out$Parameter)

    data_out$Value <- lab_chlorophyll_correction(
                        Value = data_out$Value,
                        Date=data_out$CollectDate,
                        Parameter=data_out$Parameter)

    # save df as csv for later
    write_csv(data_out, cache_name, col_name=TRUE)
  }

  return(data_out)
}
  
# Everything past here has been moved to a methods files
