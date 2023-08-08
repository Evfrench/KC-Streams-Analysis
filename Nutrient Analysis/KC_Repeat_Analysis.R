library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
library(ggplot2)
source('./functions/get_socrata_data_func.R')
bigTable <- fread('./data_cache/KC_WQ_Data')

################################################################################
#
# Data Wrangling
#
################################################################################

NOx_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
PO4_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')

NOx_monthly <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('monthly'))

#Removes the year 2023 since it is an incomplete year
NOx_annual <- subset(NOx_annual, Year < 2023)
PO4_annual <- subset(PO4_annual, Year < 2023)


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
  if (siteSelectNbase[site] < 2 | siteSelectNrecent[site] < 2){
    NOx_annual <- NOx_annual %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 2 | siteSelectPrecent[site] < 2){
    PO4_annual <- PO4_annual %>% select(- all_of(site))
  }
}

################################################################################
#
# Test GAM
#
################################################################################
# Creates a test GAM, need to create a for loop that creates a model for each locator and extracts the st dev into a vector or array
test_mod <- gam(A319 ~ s(Year), data = PO4_annual)
summary(test_mod)
plot.gam(test_mod, residuals = TRUE)

# What element on the GAM object represents the model st dev? Did Kurtis use the model predictions as his slope basis?

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
NOx_avg_slp <- as.data.frame(NOx_avg_diff[-1]/N_avg_diffyr) #This is the average slope. Units are μmicrogram/Liter/year (μg/L/yr)
colnames(NOx_avg_slp) <- c('Avg Slope (μg/L/yr)')

# Slope distribution Curve 
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(stat = 'density') + 
  ggtitle('NO2/3 Slope Distribution Curve') +
  scale_x_continuous(breaks = c(-20:2 *5))

# Slope distribution histogram
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(binwidth = 2.5) + 
  ggtitle('NO2/3 Slope Distribution Histogram, bin-width = 2.5') +
  scale_x_continuous(breaks = c(-20:2 *5))

# Slope distribution Curve, Modified limits
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(stat = 'density') + 
  ggtitle('NO2/3 Slope Distribution Curve, Zoomed-in') +
  scale_x_continuous(breaks = c(-6:6 *5),limits = c(-30,30))

# Slope distribution histogram, Modified limits
ggplot(NOx_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(binwidth = 2.5) + 
  ggtitle('NO2/3 Slope Distribution Histogram, Zoomed-in, bin-width = 2.5') +
  scale_x_continuous(breaks = c(-6:6 *5),limits = c(-30,30))

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
PO4_avg_slp <- as.data.frame(PO4_avg_diff[-1]/P_avg_diffyr) #This is the average slope. Units are microgram/Liter/year
colnames(PO4_avg_slp) <- c('Avg Slope (μg/L/yr)')


# Slope distribution Curve 
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(stat = 'density') +
  ggtitle('PO4 Slope Distribution Curve') +
  scale_x_continuous(breaks = )

# Slope distribution histogram
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(binwidth = 0.1) + 
  ggtitle('PO4 Slope Distribution Histogram, bin-width = 0.1') +
  scale_x_continuous(breaks = )

# Slope distribution Curve, Modified limits
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(stat = 'density') + 
  ggtitle('PO4 Slope Distribution Curve, Zoomed-in') +
  scale_x_continuous(breaks = c(-5:5 *0.2),limits = c(-1,1))

# Slope distribution histogram, Modified limits
ggplot(PO4_avg_slp, aes(x = `Avg Slope (μg/L/yr)`)) +
  geom_histogram(binwidth = 0.1) + 
  ggtitle('PO4 Slope Distribution Histogram, Zoomed-in, bin-width = 0.1') +
  scale_x_continuous(breaks = c(-5:5 *0.2),limits = c(-1,1))


