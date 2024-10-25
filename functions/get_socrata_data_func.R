## function to extract water quality data from Socrata
# contains additional functions to format the water quality data for different purposes
# Libraries ##################################################################################################
library(plyr)
library(dplyr)
library(mgcv)
library(data.table)
library(tidyverse)
library(RSocrata)
library(lubridate)
library(miscTools)
library(forecast)
library(zoo)
library(readr)
library(readxl)
library(ggplot2)
library(psych)
library(GGally)
library(corrplot)
library(gratia)
library(qgam)

# Initialize some of the common data ###########################################################

# This creates a scatterplot matrix of all the land cover categories in the NLCD
#LandCover <- read_excel("data_cache/SourceData/streams_2019lulc.xlsx", sheet = "LULC - %")
#pairs.panels(LandCover[, c(3:22)], smooth = FALSE, scale = TRUE, lm = TRUE, cex.cor = 3, main = 'Scatterplot Matrix for Landcover Data')
# This plot tells me that the best parameters to test are as follows: 
# Developed, All Intensities
# Developed, Open Space
# Deciduous Forest
# Total Agricultural
# Wetlands, Total
# Open Water
#
# So I will eliminate the other land cover types
CoverVariables <-  read_excel("data_cache/SourceData/streams_2019lulc.xlsx", sheet = "LULC - %")[, c(1:2,5,13,18,21,3)]
#pairs.panels(CoverVariables[, c(3:7)], smooth = FALSE, scale = FALSE, lm = FALSE, cex.cor = 0.5, main = 'Scatterplot Matrix for Landcover Data')


# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]

# Checks for the 'Chlorophyll a' parameter
source("./functions/lab_chlorophyll_correction.R")

# Checks the following parameters:
# Total Phosphorus, Total Nitrogen, Orthophosphate Phosphorus, Nitrite + Nitrate Nitrogen
source("./functions/lab_nutrient_correction.R")


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
# Prepares data for use by the egrets mergeReport and modelEstimation functions
# Pass in the exapanded data frame from normalize_water_quality_data_parameters
# Also specify which (single) parameter you want to generate the egret dataframe for
generate_egret_sample_from_water_quality_data <- function(input_data = data.frame(), param) {  
  # Generate sample dataframe for passed parameter
  normalized_param = gsub(" ", "_", param)
  return(data.frame(
      Date = as.Date(input_data$CollectDate),
      ConcLow = input_data[[normalized_param]], 
      ConcHigh = input_data[[normalized_param]], 
      Uncen = (input_data[[normalized_param]] * 0 + 1),
      ConcAve = input_data[[normalized_param]],
      Julian = (input_data[[normalized_param]] * 0),
      Month = strftime(input_data$CollectDate, format="%m"),
      Day = strftime(input_data$CollectDate, format="%d"),
      DecYear = decimal_date(input_data$CollectDate),
      MonthSeq = (input_data[[normalized_param]] * 0),
      SinDY = sin(2*pi*decimal_date(input_data$CollectDate)),
      CosDY = cos(2*pi*decimal_date(input_data$CollectDate))
  ))
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
  cache_name = './data_cache/Misc/cache_WLRD_location_dataset.csv'
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
  cache_name = paste0('./data_cache/Misc/cache_water_quality-',paste0(locs$Locator,collapse='-'),'-dataset.csv')
  
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
                Locator= locator,
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
  
# Everything past here has been moved to a methods file

summarize_WQ_data <- function(params, timeframe)
{
  bigTable <- fread('./data_cache/SourceData/KC_WQ_Data.csv') %>% 
    mutate(Locator=ifelse(Locator=='FF321','F321',                  #Join past/present locations
                          ifelse(Locator=='A632','0632',
                                 ifelse(Locator=='N484A','N484',
                                        ifelse(Locator %in% c('0456','0456A'),'A456',
                                               ifelse(Locator=='X438', '0438',
                                                      ifelse(Locator=='X630','A630',
                                                             Locator)))))))
  # Returns either the annual median or the monthly arithmetic average for the data, depending on the input
  # Begin by making a vector of all the unique locator codes
  paramconv <- c("Ammonia_Nitrogen", "Organic_Nitrogen", "Nitrite_+_Nitrate_Nitrogen", "Total_Kjeldahl_Nitrogen", "Total_Nitrogen",
                 "Orthophosphate_Phosphorus", "Total_Phosphorus", "Total_Hydrolyzable Phosphorus")
  
  
  locs <- unique(bigTable$Locator)
  locs <- locs[order(locs)]
  
  # Initialize empty frames for use in the for loop
  df1 <- tibble()
  df2 <- tibble()
  df3 <- tibble()
  
  if (timeframe == 'annual'){
  median_out <- as.data.frame(unique(bigTable$Wtr_Year)) # creates a data frame of every year in the data
  names(median_out) <- c('Year')
  median_out <- arrange(median_out, median_out$Year)
 
   # Fill out columns for every location in the data set
  for (loc in locs) {
    df1 <- data.frame(bigTable$Wtr_Year[bigTable$Locator == loc], 
                      bigTable$Month[bigTable$Locator == loc],
                      bigTable[, ..params][bigTable$Locator == loc]) %>%
      drop_na()
    names(df1) <- c('Year','Month','Conc')
    
    
    if (params %in% paramconv) {  # some parameters are reported in mg/L but would typically be reported in ug/L, this makes that conversion
      df1$Conc <- df1$Conc * 1000
    }
    
    df2 <- df1 %>% # bin readings into a single number per month
      group_by(Year, Month) %>%
      summarise(ave = mean(Conc, na.rm = TRUE), .groups = 'drop_last') 
    
    df3 <- df2 %>%  # Count the number of readings per water year, if there are less than 7 months of readings in a year, it will be removed from this record
      select(- all_of('Month')) %>%
      group_by(Year) %>%
      mutate(num = n()) %>%
      group_by(Year) %>% 
      summarise(med = median(ave, na.rm = TRUE), nums = mean(num, na.rm = TRUE)) %>% # bin all readings into an annual median value
      subset(nums > 6) %>%
      select(- all_of('nums')) 
    
    median_out <- full_join(median_out,df3, by = 'Year')
    
   }
  names(median_out) <- c('Year',locs) # rename all columns to match their locations

  #save data frame for later usage
  cache_name = paste0('./data_cache/NutrientData/median_annual_',paste0(params),'.csv')
  write_csv(median_out, cache_name, col_name=TRUE)
  
  }
  
else{  if (timeframe == 'monthly'){
    bigTable$Year_mon <- as.yearmon(bigTable$Year_Mon)
    
    median_out <- as.data.frame(unique(bigTable$Year_mon)) # creates a data frame of every year in the data
    names(median_out) <- c('Year_mon')
    median_out <- arrange(median_out, median_out$Year_mon)
    
    # Fill out columns for every location in the data set
    for (loc in locs) {
      df1 <- data.frame(bigTable$Year_mon[bigTable$Locator == loc], 
                        bigTable[, ..params][bigTable$Locator == loc]) %>%
        drop_na()
      names(df1) <- c('Year_mon','Conc')
      
      if (params %in% paramconv) {
        df1$Conc <- df1$Conc * 1000
      }
      
      df2 <- df1 %>%
        group_by(Year_mon) %>%
        summarise(ave = mean(Conc, na.rm = TRUE)) 
      
      median_out <- full_join(median_out,df2, by = 'Year_mon')
    }
    names(median_out) <- c('Year_mon',locs) # rename all columns to match their locations
    cache_name = paste0('./data_cache/NutrientData/mean_monthly_',paste0(params),'.csv')
    write_csv(median_out, cache_name, col_name=TRUE)
  }
  
  for(column in colnames(median_out)){
    median_out[,column][is.nan(median_out[,column])] <- NA # Replaces all Nan's with NA for the sake of consistency
  }}
  
  return(median_out)
}



demedian <- function(x = data.frame())
{
  cname<- colnames(x)
  rname<- rownames(x)
  out <- data.frame(row.names = rname)
  med <- colMedians(x, na.rm = TRUE)
  for (i in 1:ncol(x))
  {
    out[,i] <- x[,i] - med[i]
  }
  colnames(out) <- cname
  rownames(out) <- rname
  return(out)
}

####
#
#
#
#
####

#LT_Slope_Dist <- function(input.data= tibble(), 
#                          window= integer(length = 4), 
#                          cutoff= integer(length = 2), 
#                          units= character(length = 1),
#                          hydro= FALSE){
#  # This function will filter out sites and extract the long term trends
#  # The trend is based on the window, a series of four numbers, the first 2 are the baseline years, the second 2 are the test or recent years
#  
#  if(hydro == T){input.data <- input.data %>% rename(Year = WtrYear)} # Stream gages will be timed based on water year, so this will rename the year column to work with this function
#  
#  # Determines how many years each site has in the baseline and test groups
#  BaseSelect <- input.data %>%
#    subset((Year >= window[1] & Year <= window[2])) %>%
#    sapply(function(x) sum(!is.na(x)))
#  
#  TestSelect <- input.data %>%
#    subset((Year >= window[3] & Year <= window[4])) %>%
#    sapply(function(x) sum(!is.na(x)))
#  
#  # This removes the sites that do not meet the cutoff 
#  input.data <- as.data.frame(input.data)
#  input.filtered <- as.data.table(input.data[names(BaseSelect[BaseSelect > cutoff[1]])[names(BaseSelect[BaseSelect > cutoff[1]]) %in% names(TestSelect[TestSelect > cutoff[2]])]])
#  
#  Median_diffyr <- numeric()
#  
#  # Use geometric mean if performed on hydrological parameters
#  if(hydro == TRUE){
#    
#    for (site in colnames(input.filtered[-1])) {
#      Loop.frame <- input.filtered[,c('Year',..site)] 
#      Loop.frame <- na.omit(Loop.frame) # Removes all the NA rows, so years without samples are not counted in the central year
#      Loop.frame <- Loop.frame[,'Year']
#      Loop.framediff <- sapply(Loop.frame[Year >= window[3] & Year <= window[4]], function(x) mean(x, na.rm = TRUE)) - sapply(Loop.frame[Year <= window[2] & Year >= window[1]], function(x) mean(x, na.rm = TRUE))
#      Median_diffyr[site] <- Loop.framediff
#      remove(Loop.frame, Loop.framediff)
#    }
#    
#    # Calculates baseline and recent geometric averages, then calculates the difference
#    Median_diff <- sapply(input.filtered[Year >= window[3] & Year <= window[4]], function(x) mean(x, na.rm = TRUE)) - sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) mean(x, na.rm = TRUE))
#    Median_slp <- as.data.frame(Median_diff[-1]/Median_diffyr[-1]) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/yr)
#    colnames(Median_slp) <- c(paste0('Mean Slope (', units,'/wtryear)')) 
#    
#    # add % decline
#    Median_pdiff <- 100*(sapply(input.filtered[Year >= window[3] & Year <= window[4]], function(x) mean(x, na.rm = TRUE)) - sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) mean(x, na.rm = TRUE)))/sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) mean(x, na.rm = TRUE))
#    Median_slp$`% Change Per Water Year` <- Median_pdiff[-1]/Median_diffyr[-1]
#  } else {
#    
#    for (site in colnames(input.filtered[-1])) {
#      Loop.frame <- input.filtered[,c('Year',..site)] 
#      Loop.frame <- na.omit(Loop.frame) # Removes all the NA rows, so years without samples are not counted in the central year
#      Loop.frame <- Loop.frame[,'Year']
#      Loop.framediff <- sapply(Loop.frame[Year >= window[3] & Year <= window[4]], function(x) median(x, na.rm = TRUE)) - sapply(Loop.frame[Year <= window[2] & Year >= window[1]], function(x) median(x, na.rm = TRUE))
#      Median_diffyr[site] <- Loop.framediff
#      remove(Loop.frame, Loop.framediff)
#    }
#    # Calculates the baseline and recent averages, then calculates the difference 
#    Median_diff <- sapply(input.filtered[Year >= window[3] & Year <= window[4]], function(x) median(x, na.rm = TRUE)) - sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) median(x, na.rm = TRUE))
#    Median_slp <- as.data.frame(Median_diff[-1]*10/Median_diffyr[-1]) #This is the average slope. Units are μmicrogram/Liter/10year (μg/L/decade)
#    colnames(Median_slp) <- c(paste0('Median Slope (', units,'/decade)')) 
#    
#    # add % decline
#    Median_pdiff <- 100*(sapply(input.filtered[Year >= window[3] & Year <= window[4]], function(x) median(x, na.rm = TRUE)) - sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) median(x, na.rm = TRUE)))/sapply(input.filtered[Year <= window[2] & Year >= window[1]], function(x) median(x, na.rm = TRUE))
#    Median_slp$`% Change Per Decade` <- Median_pdiff[-1]*10/Median_diffyr[-1]
#  }
#  
#  return(Median_slp)
#}

####
#
#
#
#
#
####

LT_Slope_Dist <- function(input.data= tibble(), 
                          window= c(1979,2012,2013,2022), 
                          cutoff= c(5,5,10), 
                          units= NULL,
                          hydro= FALSE){
  # This function will filter out sites and extract the long term trends
  # The trend is based on the window, a series of four numbers, the first 2 are the baseline years, the second 2 are the test or recent years
  
#  if(hydro == T){input.data <- input.data %>% rename(Year = WtrYear)} # Stream gages will be timed based on water year, so this will rename the year column to work with this function
  
  # Determines how many years each site has in the baseline and test groups
  BaseSelect <- input.data %>%
    subset((Year >= window[1] & Year <= window[2])) %>%
    sapply(function(x) sum(!is.na(x)))
  
  TestSelect <- input.data %>%
    subset((Year >= window[3] & Year <= window[4])) %>%
    sapply(function(x) sum(!is.na(x)))
  
  # This removes the sites that do not meet the cutoff 
  input.data <- as.data.frame(input.data)
  input.filtered <- as.data.table(
    input.data[names(BaseSelect[BaseSelect > cutoff[1]])[
      names(BaseSelect[BaseSelect > cutoff[1]]) %in% names(TestSelect[TestSelect > cutoff[2]])]])
  
  Out.data <- data.frame('Center_Year_Test' = numeric(), 'Center_Year_Base' = numeric(), 
                         'Test_Val'= numeric(), 'Base_Val'= numeric(), 
                         'Mean_Slp'= numeric(), '% Change Per decade' = numeric())
  colnames(Out.data)[5:6] <- c(paste0('Mean Slope (', units,'/decade)'),'% Change Per Decade') 
 
  # This look will take the central values and years, then calulate the slope in terms of absolute units and in terms of percent change from the baseline
    for (site in colnames(input.filtered)[-1]) {
      Loop.frameBase <- input.filtered[,c('Year',..site)][Year <= window[2] & Year >= window[1]] %>% drop_na() # vector of all baseline years
      Loop.frameTest <- input.filtered[,c('Year',..site)][Year <= window[4] & Year >= window[3]] %>% drop_na() # vector of all test years
     
      if (nrow(Loop.frameBase) < cutoff[3]){ # If there are less than 10 years available in the baseline period, this will prevent an error
        stop <- nrow(Loop.frameBase)
      } else {
        stop <- cutoff[3]
      }
      
      Y1 <- mean(Loop.frameTest[,Year])  # Calculates the baseline and recent averages, then calculates the difference
      Y2 <- mean(Loop.frameBase[c(1:stop),Year])
      V1 <- colMeans(Loop.frameTest[,2]) #sapply(Loop.frameTest[,2], function(x) mean(x, na.rm = T)) 
      V2 <- colMeans(Loop.frameBase[c(1:stop),2]) #sapply(Loop.frameBase[c(1:stop),2], function(x) mean(x, na.rm = T))
      
      Out.data[site,] <- c(Y1, #fill in the rows of data, this may not work in the current syntax
                           Y2,
                           V1,
                           V2,
                           10 * (V1-V2)/(Y1-Y2),
                           10 * (100*(V1-V2)/V2)/(Y1-Y2) )
      remove(Loop.frameBase, Loop.frameTest,Y1,Y2,V1,V2,stop)
    }

  
  return(Out.data)
}

####
#
#
#
#
#
####

Land_Cover_Modeling <- function(WQ_Data = tibble(), 
                                LandCover_Data = tibble(), 
                                param = character(), 
                                window = numeric(length = 2),
                                log_space = FALSE){
 
# Removes the more upstream duplicate sites 
 #dupes <- c('C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470')
 #WQ_Data <- WQ_Data %>% select(- all_of(dupes))
 
  # Treats the data in either log or absolute space, depending on the inputs
  # This takes the average of the years in the defined of the water quality data, and removes any sites with half the number of years in the window
  if (log_space == FALSE){
    mod_inputs <- WQ_Data %>%
      subset(Year <= window[2] & Year >= window[1])%>%
      select(- all_of('Year')) %>%
      t() %>% 
      as.data.frame() %>%
      rownames_to_column(var = 'Locator')%>%
      rowwise(Locator) %>%
      summarise(count = sum(! is.na(c_across(c(V1:V7)))), 
             mean_Conc = mean(c_across(c(V1:V7)), na.rm = TRUE)) %>%
      left_join(LandCover_Data, by = 'Locator') %>%
      subset(count > (window[2]-window[1])/2) %>%
      select(- all_of("count"))
  }
  else {
    mod_inputs <- WQ_Data %>%
      subset(Year <= window[2] & Year >= window[1])%>%
      select(- all_of('Year')) %>%
      t() %>% 
      as.data.frame() %>%
      rownames_to_column(var = 'Locator') %>%
      rowwise(Locator) %>%
      summarise(count = sum(! is.na(c_across(V1:V7))), 
                mean_Conc = mean(log(c_across(V1:V7)), na.rm = TRUE)) %>%
      left_join(LandCover_Data, by = 'Locator') %>%
      subset(count > (window[2]-window[1])/2) %>%
      select(- all_of("count"))
  }
  
  # Saves the original Landcover names and replaces the names in the data frame with letters
  OrigNames <- names(mod_inputs)
  names(mod_inputs) <- c('Locator', 'mean_Conc', 'Stream', 'a', 'b', 'c', 'd', 'e')
  
  # Creates all combinations of the original names
  ConstName <- paste(param,'Const.', sep = ' = ')
  SingleName <- OrigNames[4:8]
  DualComb <- combn(SingleName,2)
  TripleComb <- combn(SingleName,3)
  name_formula <- list()
  
  # Create all combinations of the variables for the model
  ConstEq <- paste('mean_Conc',' ~')
  SingleVar <- names(mod_inputs)[4:8]
  DualVar <- combn(SingleVar,2)
  TripleVar <- combn(SingleVar,3)
  model_formula <- list()
  
  # This loop will create a list of all 16 model combination formulas
  for (i in 1:41) {
    if (i >= 1 && i <= 5){
      model_formula[i] <- paste(ConstEq, SingleVar[i])
      name_formula[i] <- paste(ConstName, SingleName[i], sep = ' + ')
    }
    if(i >= 6 && i <= 15){
      model_formula[i] <- paste(paste(ConstEq, DualVar[1,i-5]), DualVar[2,i-5], sep = ' + ')
      name_formula[i] <-  paste(ConstName, DualComb[1,i-5], DualComb[2,i-5], sep = ' + ')
    }
    if(i >=16 && i <= 25){
      model_formula[i] <- paste(paste(ConstEq, TripleVar[1,i-15]), TripleVar[2,i-15], TripleVar[3,i-15], sep = ' + ')
      name_formula[i] <-  paste(ConstName, TripleComb[1,i-15], TripleComb[2,i-15], TripleComb[3,i-15], sep = ' + ')
    }
  }
  
  # Creates an empty table for the model diagnostics
  mod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'RMSE' = numeric(), 'AICc' = numeric(),
                        'AICwt' = numeric(), 'Intercept' = numeric(), 'coef_1' = numeric(), 
                        'coef_2' = numeric(), 'coef_3' = numeric())
  
  # Creates an empty list for the function output, this will store the model results and the diagnostics table
  out_list <- list()
  
  # This will loop through all the defined model formulas, put the results in the mod_results table, add extra diagnostics, then save the actual model in out_list 
  for (i in 1:length(model_formula)) {
    # Fit a general linear model to the selected parameters
    mod <- glm(as.formula(model_formula[[i]]), data = mod_inputs, family = gaussian(link = "identity"))
    
    # Add additional diagnostics
    n <- nrow(mod_inputs)
    r2 <- rSquared(mod$y, mod$residuals) 
    k <- length(mod$coefficients) - 1
    aicc <- mod$aic + (2*k*(k+1))/(n-k-1)
    
    # Add a new row to the results table
    mod_results <- mod_results %>% add_row(Description = name_formula[[i]], R_Squared = r2[1,1], RMSE = sd(mod$residuals),
                                           AICc = mod$aic, Intercept = mod$coefficients[1], 
                                           coef_1 = mod$coefficients[2], coef_2 = mod$coefficients[3], 
                                           coef_3 = mod$coefficients[4])
    
    # Add the model results to the function output and change the name in the list
    out_list[[i+2]] <- mod
    names(out_list)[i+2] <- name_formula[[i]]
  }
  
  # Add model selection diagnostics
  mod_results$relLik <- exp(-0.5 * (mod_results$AICc - min(mod_results$AICc)))
  mod_results$AICwt <- mod_results$relLik/sum(mod_results$relLik)
  
  # Create composite model data in the input data frame
  names(mod_inputs) <- OrigNames
  mod_inputs <- mod_inputs %>%  # Create the predictions and residuals columns
   mutate(combined_Pred = 0, 
          combined_Resid = 0,
          .after = mean_Conc)
  
  # Calculate the predictions
  for (i in 1:nrow(mod_results)) {
    mod_inputs$combined_Pred <- mod_inputs$combined_Pred + (out_list[[i+2]]$linear.predictors*mod_results$AICwt[i])
  }
  
  # Calculate the residuals
  mod_inputs$combined_Resid <- mod_inputs$mean_Conc - mod_inputs$combined_Pred
  
  # Arrange the table by decreasing AICwt
  mod_results <- mod_results %>% 
    arrange(-AICwt) %>%
    column_to_rownames(var = 'Description') %>%
    round(digits = 3) %>%
    rownames_to_column(var = 'Description')
  
  # Move the results table and the input table into the output list
  out_list[[1]] <- mod_results
  out_list[[2]] <- mod_inputs
  names(out_list)[c(1,2)] <- c('Results Table', 'Inputs + Composite Predictions')
  
  
  return(out_list)
}


Seasonal_Analysis <- function(input_data = tibble(), form = "Percent", window = c(1981,2022)){
  # This function takes monthly data and transforms it into a long table for graphing
  
  if(form == 'Percent'){
    long_table <- input_data %>%
      reshape2::melt(id.var='Year_mon') %>% #make the table long
      mutate(Year = year(Year_mon),
             Month = month(Year_mon)) %>% # separate the year_mon into two separate columns, the remove any empty fields
      drop_na() %>%
      rowwise() %>%
      mutate(Year = replace(Year, Month %in% c(10:12), Year+1),
             .before = 1) %>%
      group_by(Year, variable) %>%
      mutate(num = n()) %>% 
      subset(num >= 7) %>% # Counts the number of non-empty months per site and year, then removes all years that have less than half a year of data
      group_by(variable,Year) %>%
      reframe(annual_dev = 100*(value - median(value, na.rm=TRUE))/(median(value, na.rm=TRUE)), # calculates the median concentration for each year, then the monthly deviation from the median
              Month = Month) %>%
      group_by(variable,Month) %>%
      reframe(med_annual_dev = median(annual_dev, na.rm= TRUE)) # merges all of years for each site into one average
  }
  
  if(form == 'Mean-Dev'){
    long_table <- input_data %>%
      reshape2::melt(id.var='Year_mon') %>% #make the table long
      mutate(Year = year(Year_mon),
             Month = month(Year_mon)) %>% # separate the year_mon into two separate columns, the remove any empty fields
      drop_na() %>%
      rowwise() %>%
      mutate(Year = replace(Year, Month %in% c(10:12), Year+1),
             .before = 1) %>%
      group_by(Year, variable) %>%
      mutate(num = n()) %>% 
      subset(num >= 7) %>% # Counts the number of non-empty months per site and year, then removes all years that have less than half a year of data
      group_by(variable,Year) %>%
      reframe(annual_dev = (value - mean(value, na.rm=TRUE)), # calculates the mean concentration for each year, then the monthly deviation from the median
              Month = Month) %>%
      group_by(variable,Month) %>%
      reframe(mean_annual_dev = mean(annual_dev, na.rm= TRUE)) # merges all of years for each site into one average
  }
  if(form == 'Absolute'){
    long_table <- input_data %>%
      reshape2::melt(id.var='Year_mon') %>% #make the table long
      mutate(Year = year(Year_mon),
             Month = month(Year_mon)) %>% # separate the year_mon into two separate columns, the remove any empty fields
      drop_na() %>%
      rowwise() %>%
      mutate(Year = replace(Year, Month %in% c(10:12), Year+1),
             .before = 1) %>%
      group_by(Year, variable) %>%
      mutate(num = n()) %>% 
      subset(num >= 7) %>% # Counts the number of non-empty months per site and year, then removes all years that have less than half a year of data
      group_by(variable,Month) %>%
      reframe(mean_month = mean(value, na.rm= TRUE)) # merges all of years for each site into one average
  }
  if(form == 'Relative'){
    long_table <- input_data %>%
      reshape2::melt(id.var='Year_mon') %>% #make the table long
      mutate(Year = year(Year_mon),
             Month = month(Year_mon)) %>% # separate the year_mon into two separate columns, the remove any empty fields
      drop_na() %>%
      rowwise() %>%
      mutate(Year = replace(Year, Month %in% c(10:12), Year+1),
             .before = 1) %>%
      group_by(Year, variable) %>%
      subset(Year >= window[1] & Year <= window[2]) %>%
      mutate(num = n()) %>% 
      subset(num >= 7) %>% # Counts the number of non-empty months per site and year, then removes all years that have less than half a year of data
      group_by(variable,Year) %>%
      reframe(rel_dev = value / median(value, na.rm=TRUE), # divides the monthly concentration by its annual median
              Month = Month) %>%
      group_by(variable,Month) %>%
      reframe(geo_mean_dev = geometric.mean(rel_dev, na.rm= TRUE))  # merges all of years for each site into one average
  }
  return(long_table) 
}


# Standardized method for extracting, fitting, and plotting nutrient data for monitoring sites. 
# Currently it will automatically fit the following percentiles: 10th, 25th, 50th, 75th, 90th
# Future additions: Q = FALSE, Gage_Code = c(NULL), Season = FALSE
# Optional flow component, seasonal component, specifyable quantiles
# Prompts for the date windows
# Issues with y-scaling. How to remove extreme values? just replot?


QuantileGamRun <- function(SiteCode = c('A315'), Params = c('Nitrite + Nitrate Nitrogen')){

  outlist <- list()
  # This will begin by querying the socrata database for each of the site_codes mentioned for all parameters  
  
  WQ_Params <- get_socrata_data_func(locns = SiteCode, parms = Params,
                                     SiteType = 'Streams and Rivers') %>%
    mutate(Locator=ifelse(Locator=='FF321','F321',                  #Join past/present locations
                          ifelse(Locator=='A632','0632',
                                 ifelse(Locator=='N484A','N484',
                                        ifelse(Locator %in% c('0456','0456A'),'A456',
                                               ifelse(Locator=='X438', '0438',
                                                      ifelse(Locator=='X630','A630',
                                                             Locator))))))) %>%
    mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),  # merge the two DO and conductivity fields
           Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
           Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                   "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L'),
           Censored = if_else(Value <= MDL, TRUE, FALSE, missing = FALSE),
           Date = date(CollectDate),
           Dec_Date = decimal_date(Date) - min(decimal_date(Date))) %>%
    rowwise() %>%
    mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                   "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000)) # Convert nutrient values to micrograms per liter for convenience
  
  SiteCode <- unique(WQ_Params$Locator)
  count = 0
  
  # Fit and plot the models
  for (i in 1:length(SiteCode)) {
    for (j in 1:length(Params)) {
      
      count = count + 1
      title <- paste(SiteCode[i], Params[j], sep = ": ")
      
      dat <- WQ_Params %>% 
        subset(Date > dmy('31-12-1978') & Date < dmy('01-01-2023')) %>%
        subset(Locator == SiteCode[i]) %>%
        subset(Parameter == Params[j])
      
      
      multiquant <- mqgam(Value ~ s(Dec_Date, k = 20, bs = 'ad'), data = dat, qu = c(0.1,0.25,0.5,0.75,0.90))
      
      dat <- as_tibble(dat) %>% 
        mutate(q10 = qdo(multiquant, qu = 0.1, fitted),
               q25 = qdo(multiquant, qu = 0.25, fitted),
               q50 = qdo(multiquant, qu = 0.5, fitted),
               q75 = qdo(multiquant, qu = 0.75, fitted),
               q90 = qdo(multiquant, qu = 0.9, fitted))
      
      print(ggplot(data = dat) +
              geom_point(aes(x = Date, y = Value), col = "darkgrey") +
              geom_line(aes(x = Date, y = q50),
                        col = "black", lwd = 1) +
              geom_line(aes(x = Date, y = q25),
                        col = "black", lwd = 0.5) +
              geom_line(aes(x = Date, y = q75),
                        col = "black", lwd = 0.5) +
              geom_line(aes(x = Date, y = q10),
                        col = "black", lwd = 0.5, linetype = 'twodash') +
              geom_line(aes(x = Date, y = q90),
                        col = "black", lwd = 0.5, linetype = 'twodash') +
              scale_y_continuous(#limits = c(0,3), 
                name = unique(dat$Units)[1]) +
              ggtitle(label = title))
      
      outlist[[count]] <- dat
      names(outlist)[count] <- title
      
    }
  }
  return(outlist)
  }


