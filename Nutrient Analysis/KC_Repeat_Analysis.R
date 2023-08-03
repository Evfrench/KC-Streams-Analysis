library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
source('./functions/get_socrata_data_func.R')  
bigTable <- fread('./data_cache/KC_WQ_Data')

NOx_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
PO4_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')

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
  if (siteSelectNbase[site] < 15 | siteSelectNrecent[site] < 4){
    NOx_annual <- NOx_annual %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 15 | siteSelectPrecent[site] < 4){
    PO4_annual <- PO4_annual %>% select(- all_of(site))
  }
}


test_mod <- gam(A620 ~ s(Year), data = NOx_annual)
summary(test_mod)

# Calculates the baseline and recent averages, then calculates the difference
# NOTE: Should the central year be calculated for each site individually? Jasper says yes :'(
NOx_avg_b <- sapply(NOx_annual[Year <= 2017], function(x) mean(x, na.rm = TRUE))
NOx_avg_r <- sapply(NOx_annual[Year > 2017], function(x) mean(x, na.rm = TRUE))
NOx_avg_diff <- NOx_avg_r - NOx_avg_b
NOx_avg_slp <- NOx_avg_diff[-1] / NOx_avg_diff[1]

PO4_avg_b <- sapply(PO4_annual[Year <= 2017], function(x) mean(x, na.rm = TRUE))
PO4_avg_r <- sapply(PO4_annual[Year > 2017], function(x) mean(x, na.rm = TRUE))
PO4_avg_diff <- PO4_avg_r - PO4_avg_b
PO4_avg_slp <- PO4_avg_diff[-1] / PO4_avg_diff[1]

