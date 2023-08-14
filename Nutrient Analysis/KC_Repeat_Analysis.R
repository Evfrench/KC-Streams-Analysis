library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
library(ggplot2)
source('./functions/get_socrata_data_func.R')
bigTable <- fread('./data_cache/KC_WQ_Data')

NOx_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
PO4_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')
  
# You should a lot of these operations into functions  ##################################

# Option 1 ################################################################################
#
# Baseline: start-2017, 1 yr required
# Current: 2018 - 2022, 1 yr required
#



# Creates vectors containing the number of years with data in baseline window and the 'recent' window for NO2/3
siteSelectNbase <- sapply(NOx_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectNrecent <- sapply(NOx_annual[Year > 2017], function(x) sum(!is.na(x)))

# Does the same for PO4
siteSelectPbase <- sapply(PO4_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectPrecent <- sapply(PO4_annual[Year > 2017], function(x) sum(!is.na(x)))


# This For loop eliminates sites from the data set. There are 2 checks the sites go through, minimum allowable baseline data and minimum allowable recent data
# Recent standard is 4 years, baseline standard is 15 years
for (site in colnames(NOx_annual))
{
  if (siteSelectNbase[site] < 1 | siteSelectNrecent[site] < 1){
    NOx_annual <- NOx_annual %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 1 | siteSelectPrecent[site] < 1){
    PO4_annual <- PO4_annual %>% select(- all_of(site))
  }
}

################################################################################
#
# NO2/3 Slope Distribution
#
################################################################################
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
N_avg_diffyr <- numeric()

for (site in colnames(NOx_annual[,-1])) {
  nLoop <- NOx_annual[,c('Year',..site)] 
  nLoop <- na.omit(nLoop) # Removes all the NA rows, so years without samples are not counted in the central year
  nLoop <- nLoop[,'Year']
  nLoopdiff <- sapply(nLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(nLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  N_avg_diffyr[site] <- nLoopdiff
  remove(nLoop, nLoopdiff)
}

# Calculates the baseline and recent averages, then calculates the difference
NOx_avg_diff <- sapply(NOx_annual[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(NOx_annual[Year <= 2017], function(x) median(x, na.rm = TRUE))
NOx_avg_slp <- as.data.frame(NOx_avg_diff[-1]*10/N_avg_diffyr) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/decade)
colnames(NOx_avg_slp) <- c('Avg Slope (μg/L/decade)')

# Slope distribution Curve 
# ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('NO2/3 Slope Distribution Curve') 
#+  scale_x_continuous(breaks = c(-20:2 *50))

# Slope distribution histogram
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 50) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'red', size = 1) +
  ggtitle('NO2/3 Slope Distribution Histogram, bin-width = 25') 
#+ scale_x_continuous(breaks = c(-20:2 *50))

# Slope distribution Curve, Modified limits
#ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('NO2/3 Slope Distribution Curve, Zoomed-in') +
#  scale_x_continuous(breaks = c(-6:6 *50),limits = c(-300,300))

# Slope distribution histogram, Modified limits
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 25) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'red', size = 1) +
  ggtitle('NO2/3 Slope Distribution Histogram, Zoomed-in, bin-width = 25') +
  scale_x_continuous(breaks = c(-6:6 *50),limits = c(-300,300))

################################################################################
#
# PO4 Slope Distribution
#
################################################################################
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
P_avg_diffyr <- numeric()

for (site in colnames(PO4_annual[,-1])) {
  pLoop <- PO4_annual[,c('Year',..site)] 
  pLoop <- na.omit(pLoop) # removes years with empty nutrient values
  pLoop <- pLoop[,'Year'] # removes the nutrient column and just keeps years
  pLoopdiff <- sapply(pLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(pLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  P_avg_diffyr[site] <- pLoopdiff
  remove(pLoop, pLoopdiff)
}


PO4_avg_diff <- sapply(PO4_annual[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(PO4_annual[Year <= 2017], function(x) median(x, na.rm = TRUE))
PO4_avg_slp <- as.data.frame(PO4_avg_diff[-1]*10/P_avg_diffyr) #This is the average slope. Units are microgram/Liter/year
colnames(PO4_avg_slp) <- c('Avg Slope (μg/L/decade)')


# Slope distribution Curve 
#ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') +
#  ggtitle('PO4 Slope Distribution Curve') +
#  scale_x_continuous(breaks = )

# Slope distribution histogram
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 1.0) +
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'red', size = 1) +
  ggtitle('PO4 Slope Distribution Histogram, bin-width = 1') +
  scale_x_continuous(breaks = )

# Slope distribution Curve, Modified limits
#ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('PO4 Slope Distribution Curve, Zoomed-in') +
#  scale_x_continuous(breaks = c(-5:5 *2),limits = c(-10,10))

# Slope distribution histogram, Modified limits
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'red', size = 1) +
  ggtitle('PO4 Slope Distribution Histogram, Zoomed-in, bin-width = 1') +
  scale_x_continuous(breaks = c(-5:5 *2),limits = c(-10,10))


################################################################################
#
# Test GAM and monthly trends
#
################################################################################

NOx_monthly <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('monthly'))
PO4_monthly <- summarize_WQ_data(bigTable, c('Orthophosphate_Phosphorus'), c('monthly'))

NOx_monthly <- subset(NOx_monthly, Year_mon < 'Jan 2023')
PO4_monthly <- subset(PO4_monthly, Year_mon < 'Jan 2023')



# this For loop does two things, it determines if there is at least 1 year of data in the baseline and recent years,
# then it fits the data to a GAM, takes the standard deviation, and stores it in a vector
# it then finds the central year/month in the baseline and recent sets and takes the difference between them in years
PO4_sd <- numeric()
P_month_diffyr <- numeric()

for (loc in colnames(PO4_monthly[,-1])){
  if (siteSelectPbase[loc] < 2 | siteSelectPrecent[loc] < 2){
    
    PO4_monthly <- PO4_monthly %>% select(- all_of(loc))
    next
    
  }
  
  else {
  
    dat <- PO4_monthly %>%
    subset(Year_mon <= 'Dec 2017') %>% 
    select(all_of('Year_mon') | all_of(loc))
    
  pmod <- gam(dat[,2] ~ s(decimal_date(as.Date(dat[,1])))) # # Creates a GAM, need to create a for loop that creates a model for each locator and extracts the st dev into a vector or array
  PO4_sd[loc] <- sd(pmod$residuals) # takes the detrended data (residuals) and calculates the standard deviation
                                    # these values are much larger than any differences calculated between baseline and recent. This may mean that the response variable needs to be log-transformed

  pLoop <- PO4_monthly[,c('Year_mon',loc)] 
  pLoop <- na.omit(pLoop) # removes years with empty nutrient values
  pLoop <- pLoop[,'Year_mon'] # removes the nutrient column and just keeps years
  pLoopdiff <- decimal_date(as.Date(median(pLoop[pLoop > 'Dec 2017']))) - decimal_date(as.Date(median(pLoop[pLoop <= 'Dec 2017'])))
  P_month_diffyr[loc] <- pLoopdiff
  
  remove(pLoop, pLoopdiff, dat, pmod)
  }
}

# Divides the data into recent and baseline groups, then takes the median of each
PO4_month_recent <- PO4_monthly %>% 
  subset(Year_mon > 'Dec 2017') %>%
  select(- all_of('Year_mon'))

PO4_month_base <- PO4_monthly %>% 
  subset(Year_mon <= 'Dec 2017') %>%
  select(- all_of('Year_mon'))

# Stores the difference between the recent and long-term medians. Perhaps these should be averages or geometric averages.. The medians ignore the spikes in nutrient concentration which may be important for telling the story? Or not, its hard to say
PO4_month_diff <- sapply(PO4_month_recent, function(x) median(x, na.rm = TRUE)) - sapply(PO4_month_base, function(x) median(x, na.rm = TRUE))

# Stores the difference if concentration, time, and the st.dev in one data frame, adds a slope and signifigance determination
PO4_month_change <- data.frame(PO4_month_diff,P_month_diffyr,PO4_sd, row.names = colnames(PO4_monthly[-1]))
colnames(PO4_month_change) <- c('Conc_diff','Time_diff','detrend_sd')
PO4_month_change$`Avg Slope (μg/L/decade)` <- PO4_month_diff * 10 / P_month_diffyr
# What element on the GAM object represents the model st dev? Did Kurtis use the model predictions as his slope basis?

#ggplot(PO4_month_change, aes(x = `Avg Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') +
#  ggtitle('PO4 Slope Distribution Curve') +
#  scale_x_continuous( breaks = )

#PO4_plot <- PO4_monthly %>%
#  remove_rownames() %>%
#  column_to_rownames(var = 'Year_mon') %>% # avoids taking the median Year_mon
#  demedian() %>%
#  rownames_to_column(var = 'Year_mon') %>%
#  reshape2::melt(id.var="Year_mon")
#PO4_plot$Year_mon <- as.yearmon(PO4_plot$Year_mon)

#ggplot(PO4_plot, aes(Year_mon, value)) + 
#  facet_wrap(. ~ variable, shrink = FALSE) + 
#  geom_point() +
#  geom_line() +
#  ggtitle("Monthly average Orthophosphate, Median-Centered") + 
#  scale_y_continuous(name = "PO4, mg/L", limits = c(-50,100))








timeframe <- 'annual'
loc <- 'A319'
params <- c('Nitrite_+_Nitrate_Nitrogen')
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
    median_out <- as.data.frame(unique(bigTable$Year)) # creates a data frame of every year in the data
    names(median_out) <- c('Year')
    median_out <- arrange(median_out, median_out$Year)
    
    
    # Fill out columns for every location in the data set
    for (loc in locs) {
      df1 <- data.frame(bigTable$Year[bigTable$Locator == loc], 
                        bigTable$Month[bigTable$Locator == loc],
                        bigTable[, ..params][bigTable$Locator == loc])
      names(df1) <- c('Year','Month','Conc')
      
      if (params %in% paramconv) {
        df1$Conc <- df1$Conc * 1000
      }
      
      df2 <- df1 %>%
        group_by(Year, Month) %>%
        summarise(ave = mean(Conc, na.rm = TRUE), .groups = 'drop_last')
      
      df3 <- df2 %>%
        select(- all_of('Month')) %>%
        group_by(Year) %>%
        mutate(num = n()) %>%
        group_by(Year) %>% 
        summarise(med = median(ave, na.rm = TRUE), nums = mean(num, na.rm = TRUE)) %>%
        subset(nums > 5) %>%
        select(- all_of('nums')) 
      
      median_out <- full_join(median_out,df3, by = 'Year')
    }
    names(median_out) <- c('Year',locs) # rename all columns to match their locations
    
    #save data frame for later usage
    cache_name = paste0('./data_cache/median_annual_',paste0(params),'.csv')
    write_csv(median_out, cache_name, col_name=TRUE)
    
  }
  
  if (timeframe == 'monthly'){
    bigTable$Year_mon <- as.yearmon(bigTable$Decimal_year)
    
    median_out <- as.data.frame(unique(bigTable$Year_mon)) # creates a data frame of every year in the data
    names(median_out) <- c('Year_mon')
    median_out <- arrange(median_out, median_out$Year_mon)
    
    # Fill out columns for every location in the data set
    for (loc in locs) {
      df1 <- data.frame(bigTable$Year_mon[bigTable$Locator == loc], 
                        bigTable[, ..params][bigTable$Locator == loc])
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
    
  }
  
  for(column in colnames(median_out)){
    median_out[,column][is.nan(median_out[,column])] <- NA # Replaces all Nan's with NA for the sake of consistency
  }
  