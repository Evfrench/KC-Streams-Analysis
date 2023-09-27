# You should make a lot of these operations into functions  ##################################
library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
library(ggplot2)
source('./functions/long_term_trend_func.R')
source('./functions/get_socrata_data_func.R')
bigTable <- fread('./data_cache/KC_WQ_Data')

N_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
P_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')

median_slopes <- matrix(0, nrow = 3, ncol = 2 )
slopeSD <- matrix(0, nrow = 3, ncol = 2 )
mean_slopes <- matrix(0, nrow = 3, ncol = 2 )
slopeIQR <- list(0, nrow = 3, ncol = 2 )

# Treatment 1 ################################################################################
#
# Baseline: start-2017, 1 yr required
# Current: 2018 - 2022, 1 yr required
# Results: 72 sites, central slope NO2/3: -76.6 μg/L/decade, P: -1.58 μg/L/decade
# Outliers: NO2/3: , P: 

# Creates vectors containing the number of years with data in baseline window and the 'recent' window for NO2/3
siteSelectNbase <- sapply(N_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectNrecent <- sapply(N_annual[Year > 2017], function(x) sum(!is.na(x)))

# Does the same for P
siteSelectPbase <- sapply(P_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectPrecent <- sapply(P_annual[Year > 2017], function(x) sum(!is.na(x)))

# Creates a dataframe for selection Treatment 1
N_1 <- N_annual
P_1 <- P_annual

# This For loop eliminates sites from the data set. There are 2 checks the sites go through, minimum allowable baseline data and minimum allowable recent data
# Recent standard is 4 years, baseline standard is 15 years
for (site in colnames(N_annual))
{
  if (siteSelectNbase[site] < 1 | siteSelectNrecent[site] < 1){
    N_1 <- N_1 %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 1 | siteSelectPrecent[site] < 1){
    P_1 <- P_1 %>% select(- all_of(site))
  }
}

## NO2/3 Slope Distribution ###############################################################################
#
# 
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
N_Median_diffyr <- numeric()

for (site in colnames(N_1[,-1])) {
  nLoop <- N_1[,c('Year',..site)] 
  nLoop <- na.omit(nLoop) # Removes all the NA rows, so years without samples are not counted in the central year
  nLoop <- nLoop[,'Year']
  nLoopdiff <- sapply(nLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(nLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  N_Median_diffyr[site] <- nLoopdiff
  remove(nLoop, nLoopdiff)
}

# Calculates the baseline and recent averages, then calculates the difference
N_Median_diff <- sapply(N_1[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(N_1[Year <= 2017], function(x) median(x, na.rm = TRUE))
N_Median_slp1 <- as.data.frame(N_Median_diff[-1]*10/N_Median_diffyr) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/decade)
colnames(N_Median_slp1) <- c('Median Slope (μg/L/decade)')

# Slope distribution Curve 
# ggplot(N_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('NO2/3 Slope Distribution Curve') 
#+  scale_x_continuous(breaks = c(-20:2 *50))

# Slope distribution histogram
ggplot(N_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 35) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
  ggtitle('NO2/3 Slope Distribution Histogram Treatment 1, bin-width = 35') 
#+ scale_x_continuous(breaks = c(-20:2 *50))

# Slope distribution Curve, Modified limits
#ggplot(N_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('NO2/3 Slope Distribution Curve, Zoomed-in') +
#  scale_x_continuous(breaks = c(-6:6 *50),limits = c(-300,300))

# Slope distribution histogram, Modified limits
#ggplot(N_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(binwidth = 25) + 
#  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
#  ggtitle('NO2/3 Slope Distribution Histogram, Zoomed-in, bin-width = 25') +
#  scale_x_continuous(breaks = c(-6:6 *50),limits = c(-300,300))

## P Slope Distribution ###############################################################################
#
# 
#
#
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
P_Median_diffyr <- numeric()

for (site in colnames(P_1[,-1])) {
  pLoop <- P_1[,c('Year',..site)] 
  pLoop <- na.omit(pLoop) # removes years with empty nutrient values
  pLoop <- pLoop[,'Year'] # removes the nutrient column and just keeps years
  pLoopdiff <- sapply(pLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(pLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  P_Median_diffyr[site] <- pLoopdiff
  remove(pLoop, pLoopdiff)
}


P_Median_diff <- sapply(P_1[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(P_1[Year <= 2017], function(x) median(x, na.rm = TRUE))
P_Median_slp1 <- as.data.frame(P_Median_diff[-1]*10/P_Median_diffyr) #This is the average slope. Units are microgram/Liter/year
colnames(P_Median_slp1) <- c('Median Slope (μg/L/decade)')


# Slope distribution Curve 
#ggplot(P_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') +
#  ggtitle('P Slope Distribution Curve') +
#  scale_x_continuous(breaks = )

# Slope distribution histogram
ggplot(P_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 0.9) +
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'gray', size = 1) +
  ggtitle('P Slope Distribution Histogram Treatment 1, bin-width = 0.9') +
  scale_x_continuous(breaks = )

# Slope distribution Curve, Modified limits
#ggplot(P_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') + 
#  ggtitle('P Slope Distribution Curve, Zoomed-in') +
#  scale_x_continuous(breaks = c(-5:5 *2),limits = c(-10,10))

# Slope distribution histogram, Modified limits
#ggplot(P_Median_slp1, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(binwidth = 1) + 
#  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
#  ggtitle('P Slope Distribution Histogram, Zoomed-in, bin-width = 1') +
#  scale_x_continuous(breaks = c(-5:5 *2),limits = c(-10,10))

median_slopes[1,] <- c(median(N_Median_slp1[,1]), median(P_Median_slp1[,1]))
slopeSD[1,] <- c(sd(N_Median_slp1[,1]),sd(P_Median_slp1[,1]))
mean_slopes[1,] <- c(mean(N_Median_slp1[,1]), mean(P_Median_slp1[,1]))



# Treatment 2 ################################################################################
#
# Baseline: start-2017, 10 yrs required
# Current: 2018 - 2022, 3 yrs required
# Results: 51 sites, central slope NO2/3: -71.9 μg/L/decade, P: -1.70 μg/L/decade
# Outliers: NO2/3: , P: 

# Creates vectors containing the number of years with data in baseline window and the 'recent' window for NO2/3
siteSelectNbase <- sapply(N_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectNrecent <- sapply(N_annual[Year > 2017], function(x) sum(!is.na(x)))

# Does the same for P
siteSelectPbase <- sapply(P_annual[Year <= 2017], function(x) sum(!is.na(x)))
siteSelectPrecent <- sapply(P_annual[Year > 2017], function(x) sum(!is.na(x)))

# Creates a dataframe for selection Treatment 2
N_2 <- N_annual
P_2 <- P_annual

# This For loop eliminates sites from the data set. There are 2 checks the sites go through, minimum allowable baseline data and minimum allowable recent data
# Recent standard is 4 years, baseline standard is 15 years
for (site in colnames(N_annual))
{
  if (siteSelectNbase[site] < 10 | siteSelectNrecent[site] < 3){
    N_2 <- N_2 %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 10 | siteSelectPrecent[site] < 3){
    P_2 <- P_2 %>% select(- all_of(site))
  }
}

## NO2/3 Slope Distribution ###############################################################################
#
# 
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
N_Median_diffyr <- numeric()

for (site in colnames(N_2[,-1])) 
{
  nLoop <- N_2[,c('Year',..site)] 
  nLoop <- na.omit(nLoop) # Removes all the NA rows, so years without samples are not counted in the central year
  nLoop <- nLoop[,'Year']
  nLoopdiff <- sapply(nLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(nLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  N_Median_diffyr[site] <- nLoopdiff
  remove(nLoop, nLoopdiff)
}

# Calculates the baseline and recent averages, then calculates the difference
N_Median_diff <- sapply(N_2[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(N_2[Year <= 2017], function(x) median(x, na.rm = TRUE))
N_Median_slp2 <- as.data.frame(N_Median_diff[-1]*10/N_Median_diffyr) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/decade)
colnames(N_Median_slp2) <- c('Median Slope (μg/L/decade)')

# Slope distribution histogram
ggplot(N_Median_slp2, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 35) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
  ggtitle('NO2/3 Slope Distribution Histogram Treatment 2, bin-width = 35') 
#+ scale_x_continuous(breaks = c(-20:2 *50))

## P Slope Distribution ###############################################################################
#
# 
#
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
P_Median_diffyr <- numeric()

for (site in colnames(P_2[,-1])) {
  pLoop <- P_2[,c('Year',..site)] 
  pLoop <- na.omit(pLoop) # removes years with empty nutrient values
  pLoop <- pLoop[,'Year'] # removes the nutrient column and just keeps years
  pLoopdiff <- sapply(pLoop[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(pLoop[Year <= 2017], function(x) median(x, na.rm = TRUE))
  P_Median_diffyr[site] <- pLoopdiff
  remove(pLoop, pLoopdiff)
}


P_Median_diff <- sapply(P_2[Year > 2017], function(x) median(x, na.rm = TRUE)) - sapply(P_2[Year <= 2017], function(x) median(x, na.rm = TRUE))
P_Median_slp2 <- as.data.frame(P_Median_diff[-1]*10/P_Median_diffyr) #This is the average slope. Units are microgram/Liter/year
colnames(P_Median_slp2) <- c('Median Slope (μg/L/decade)')

# Slope distribution histogram
ggplot(P_Median_slp2, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 0.9) +
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
  ggtitle('P Slope Distribution Histogram Treatment 2, bin-width = 0.9') +
  scale_x_continuous(breaks = )


median_slopes[2,] <- c(median(N_Median_slp2[,1]), median(P_Median_slp2[,1]))
slopeSD[2,] <- c(sd(N_Median_slp2[,1]),sd(P_Median_slp2[,1]))
mean_slopes[2,] <- c(mean(N_Median_slp2[,1]), mean(P_Median_slp2[,1]))


# Treatment 3 ################################################################################
#
# Baseline: 1979 - 2009, 5 yrs required
# Current: 2013 - 2022, 5 yrs required
# Results: 47 sites, central slope NO2/3: -46.0 μg/L/decade, P: -1.90 μg/L/decade
# Outliers: NO2/3: , P: 

# Creates vectors containing the number of years with data in baseline window and the 'recent' window for NO2/3
siteSelectNbase <- sapply(N_annual[Year <= 2009 & Year >= 1979], function(x) sum(!is.na(x)))
siteSelectNrecent <- sapply(N_annual[Year > 2012], function(x) sum(!is.na(x)))

# Does the same for P
siteSelectPbase <- sapply(P_annual[Year <= 2009 & Year >= 1979], function(x) sum(!is.na(x)))
siteSelectPrecent <- sapply(P_annual[Year > 2012], function(x) sum(!is.na(x)))

# Creates a dataframe for selection Treatment 3
N_3 <- N_annual
P_3 <- P_annual

# This For loop eliminates sites from the data set. There are 2 checks the sites go through, minimum allowable baseline data and minimum allowable recent data
# Recent standard is 4 years, baseline standard is 15 years
for (site in colnames(N_annual))
{
  if (siteSelectNbase[site] < 5 | siteSelectNrecent[site] < 5){
    N_3 <- N_3 %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 5 | siteSelectPrecent[site] < 5){
    P_3 <- P_3 %>% select(- all_of(site))
  }
}

## NO2/3 Slope Distribution ###############################################################################
#
# 
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
N_Median_diffyr <- numeric()

for (site in colnames(N_3[,-1])) {
  nLoop <- N_3[,c('Year',..site)] 
  nLoop <- na.omit(nLoop) # Removes all the NA rows, so years without samples are not counted in the central year
  nLoop <- nLoop[,'Year']
  nLoopdiff <- sapply(nLoop[Year > 2012], function(x) median(x, na.rm = TRUE)) - sapply(nLoop[Year <= 2009 & Year >= 1979], function(x) median(x, na.rm = TRUE))
  N_Median_diffyr[site] <- nLoopdiff
  remove(nLoop, nLoopdiff)
}

# Calculates the baseline and recent averages, then calculates the difference
N_Median_diff <- sapply(N_3[Year > 2012], function(x) median(x, na.rm = TRUE)) - sapply(N_3[Year <= 2009 & Year >= 1979], function(x) median(x, na.rm = TRUE))
N_Median_slp3 <- as.data.frame(N_Median_diff[-1]*10/N_Median_diffyr) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/decade)
colnames(N_Median_slp3) <- c('Median Slope (μg/L/decade)')

# Slope distribution histogram
ggplot(N_Median_slp3, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 30) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
  geom_vline(xintercept = c(-84.5, -22.3), linetype = 'dashed', color = 'black', size = 0.5) +
  geom_vline(xintercept = -44.2, linetype = 'solid', color = 'black', size = 0.5) +
  ggtitle('NO2/3 Slope Distribution Histogram, bin-width = 30') 
#+ scale_x_continuous(breaks = c(-20:2 *50))

## P Slope Distribution ###############################################################################
#
# 
#
# Another For loop, separates the frame by different locators, then takes the centroid year for the recent and baseline group and stores it in a vector
P_Median_diffyr <- numeric()

for (site in colnames(P_3[,-1])) {
  pLoop <- P_3[,c('Year',..site)] 
  pLoop <- na.omit(pLoop) # removes years with empty nutrient values
  pLoop <- pLoop[,'Year'] # removes the nutrient column and just keeps years
  pLoopdiff <- sapply(pLoop[Year > 2012], function(x) median(x, na.rm = TRUE)) - sapply(pLoop[Year <= 2009 & Year >= 1979], function(x) median(x, na.rm = TRUE))
  P_Median_diffyr[site] <- pLoopdiff
  remove(pLoop, pLoopdiff)
}


P_Median_diff <- sapply(P_3[Year > 2012], function(x) median(x, na.rm = TRUE)) - sapply(P_3[Year <= 2009 & Year >= 1979], function(x) median(x, na.rm = TRUE))
P_Median_slp3 <- as.data.frame(P_Median_diff[-1]*10/P_Median_diffyr) #This is the average slope. Units are microgram/Liter/year
colnames(P_Median_slp3) <- c('Median Slope (μg/L/decade)')

# Slope distribution histogram
ggplot(P_Median_slp3, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 0.8) +
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', size = 1) +
  geom_vline(xintercept = c(-2.96, -0.99), linetype = 'dashed', color = 'black', size = 0.5) +
  geom_vline(xintercept = -1.92, linetype = 'solid', color = 'black', size = 0.5) +
  ggtitle('P Slope Distribution Histogram, bin-width = 0.8') 
#  scale_x_continuous(breaks = )



median_slopes[3,] <- c(median(N_Median_slp3[,1]), median(P_Median_slp3[,1]))
slopeSD[3,] <- c(sd(N_Median_slp3[,1]),sd(P_Median_slp3[,1]))
mean_slopes[3,] <- c(mean(N_Median_slp3[,1]), mean(P_Median_slp3[,1]))
slopeIQR <- list(N1 = quantile(N_Median_slp1[,1], type = 8), N2 = quantile(N_Median_slp2[,1], type = 8), N3 = quantile(N_Median_slp3[,1], type = 8),
                 P1 = quantile(P_Median_slp1[,1], type = 8), P2 = quantile(P_Median_slp2[,1], type = 8), P3 = quantile(P_Median_slp3[,1], type = 8))





# Test GAM and monthly trends ###############################################################################
#
# 
N_monthly <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('monthly'))
P_monthly <- summarize_WQ_data(bigTable, c('Orthophosphate_Phosphorus'), c('monthly'))

N_monthly <- subset(N_monthly, Year_mon < 'Jan 2023')
P_monthly <- subset(P_monthly, Year_mon < 'Jan 2023')



# this For loop does two things, it determines if there is at least 1 year of data in the baseline and recent years,
# then it fits the data to a GAM, takes the standard deviation, and stores it in a vector
# it then finds the central year/month in the baseline and recent sets and takes the difference between them in years
P_sd <- numeric()
P_month_diffyr <- numeric()

for (loc in colnames(P_monthly[,-1])){
  if (siteSelectPbase[loc] < 2 | siteSelectPrecent[loc] < 2){
    
    P_monthly <- P_monthly %>% select(- all_of(loc))
    next
    
  }
  
  else {
    
    dat <- P_monthly %>%
      subset(Year_mon <= 'Dec 2017') %>% 
      select(all_of('Year_mon') | all_of(loc))
    
    pmod <- gam(dat[,2] ~ s(decimal_date(as.Date(dat[,1])))) # # Creates a GAM, need to create a for loop that creates a model for each locator and extracts the st dev into a vector or array
    P_sd[loc] <- sd(pmod$residuals) # takes the detrended data (residuals) and calculates the standard deviation
    # these values are much larger than any differences calculated between baseline and recent. This may mean that the response variable needs to be log-transformed
    
    pLoop <- P_monthly[,c('Year_mon',loc)] 
    pLoop <- na.omit(pLoop) # removes years with empty nutrient values
    pLoop <- pLoop[,'Year_mon'] # removes the nutrient column and just keeps years
    pLoopdiff <- decimal_date(as.Date(median(pLoop[pLoop > 'Dec 2017']))) - decimal_date(as.Date(median(pLoop[pLoop <= 'Dec 2017'])))
    P_month_diffyr[loc] <- pLoopdiff
    
    remove(pLoop, pLoopdiff, dat, pmod)
  }
}

# Divides the data into recent and baseline groups, then takes the median of each
P_month_recent <- P_monthly %>% 
  subset(Year_mon > 'Dec 2017') %>%
  select(- all_of('Year_mon'))

P_month_base <- P_monthly %>% 
  subset(Year_mon <= 'Dec 2017') %>%
  select(- all_of('Year_mon'))

# Stores the difference between the recent and long-term medians. Perhaps these should be averages or geometric averages.. The medians ignore the spikes in nutrient concentration which may be important for telling the story? Or not, its hard to say
P_month_diff <- sapply(P_month_recent, function(x) median(x, na.rm = TRUE)) - sapply(P_month_base, function(x) median(x, na.rm = TRUE))

# Stores the difference if concentration, time, and the st.dev in one data frame, adds a slope and signifigance determination
P_month_change <- data.frame(P_month_diff,P_month_diffyr,P_sd, row.names = colnames(P_monthly[-1]))
colnames(P_month_change) <- c('Conc_diff','Time_diff','detrend_sd')
P_month_change$`Median Slope (μg/L/decade)` <- P_month_diff * 10 / P_month_diffyr
# What element on the GAM object represents the model st dev? Did Kurtis use the model predictions as his slope basis?


#ggplot(P_month_change, aes(x = `Median Slope (μg/L/decade)`)) +
#  geom_histogram(stat = 'density') +
#  ggtitle('P Slope Distribution Curve') +
#  scale_x_continuous( breaks = )

#P_plot <- P_monthly %>%
#  remove_rownames() %>%
#  column_to_rownames(var = 'Year_mon') %>% # avoids taking the median Year_mon
#  demedian() %>%
#  rownames_to_column(var = 'Year_mon') %>%
#  reshape2::melt(id.var="Year_mon")
#P_plot$Year_mon <- as.yearmon(P_plot$Year_mon)

#ggplot(P_plot, aes(Year_mon, value)) + 
#  facet_wrap(. ~ variable, shrink = FALSE) + 
#  geom_point() +
#  geom_line() +
#  ggtitle("Monthly average Orthophosphate, Median-Centered") + 
#  scale_y_continuous(name = "P, mg/L", limits = c(-50,100))




# Exploring Outliers from Treatment 1 & 2 ######################################

# Starting with the NO2/3 outliers: 
# B499: -1241, Yarrow Creek
# VA45A: -830, Mileta Creek
# A670: -440, Laughing Jacobs Creek
# VA41A: -361, Fisher Creek
# LSIN9: -333, Rock Creek
# 0632: 126, Issaquah Creek

N_outliers <- N_1 %>% 
  select(all_of(c('Year', 'B499','VA45A','A670','VA41A','LSIN9','0632'))) %>%
  reshape2::melt(id.var="Year") %>%
  subset(Year > 1980)

ggplot(N_outliers, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median NO2-3, Outlier Locations") +
  geom_vline(xintercept = 2017.5, linetype = 'solid', color = 'navy', size = 0.5) +
  scale_y_continuous(name = "NO2/NO3, μg/L")



# Now the PO4 outliers:
#AMES_1: -26.6, Ames Creek
#0632: -22.2, Issaquah Creek
#B499: -13.3, Yarrow Creek
#0456A: 132.5, Forbes Creek
#VA45A: 4.39, Mileta Creek
#A315: -9.61, Mill creek

P_outliers <- P_1 %>% 
  select(all_of(c('Year', 'AMES_1','0632','B499','0456A','VA45A', 'A315'))) %>%
  reshape2::melt(id.var="Year") %>%
  subset(Year > 1980)

ggplot(P_outliers, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median PO4, Outlier Locations") +
  geom_vline(xintercept = 2017.5, linetype = 'solid', color = 'navy', size = 0.5) +
  scale_y_continuous(name = "PO4, μg/L")
