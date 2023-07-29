library(plyr)
library(dplyr)
library(data.table)
source('./functions/get_socrata_data_func.R')  
bigTable <- fread('./data_cache/KC_WQ_Data')

###############################################################################
#
# Annual Median Summary of Nutrients
#
###############################################################################

AnnualNO3_NO2 <- get_annual_median(bigTable, 'Nitrite_+_Nitrate_Nitrogen')
Annual_OrthoP <- get_annual_median(bigTable, 'Orthophosphate_Phosphorus')

datSelect <- tibble(AnnualNO3_NO2[,"Year"], rowSums(!is.na(AnnualNO3_NO2[,-1])),rowSums(!is.na(Annual_OrthoP[,-1])))
names(datSelect) <- c('Year','NOx_Entries','PO4_Entries')

###############################################################################
# Data Cleaning

# This creates a plot with the number sites with non-empty readings every year
ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = NOx_Entries, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))

# NOTE: Highest frequency is from 2014 to present, large drop-off at 2008
# NOTE: The a large cluster of data is between 1979-2008, this may be a good window to start with
NOx_filtered <- subset(AnnualNO3_NO2, Year > 1978 & Year < 2009)
PO4_filtered <- subset(Annual_OrthoP, Year > 1978 & Year < 2009)

# NOTE: This will tell the number of data points available for each site, the maximum is 30
siteSelectN <- sapply(NOx_filtered, function(x) sum(!is.na(x)))
siteSelectP <- sapply(PO4_filtered, function(x) sum(!is.na(x)))

# The median number of years with any readings is 31 for N and P

# This loop will filter out all the sites that have less than 2/3 of the total window filled out
# We end up with 46 of the 92 sites, not bad
#NOx_filtered <- tibble(.rows = 54)
#PO4_filtered <- tibble(.rows = 54)
for (site in colnames(Annual_OrthoP))
{
  if (siteSelectN[site] < 20){
    NOx_filtered <- NOx_filtered %>% select(- all_of(site))
  }
  if (siteSelectP[site] < 20){
    PO4_filtered <- PO4_filtered %>% select(- all_of(site))
  }
}

# Orders the columns alphabetically, this will be helpful for later data QC
siteorder <- order(colnames(NOx_filtered))
NOx_filtered <- NOx_filtered[,siteorder]
PO4_filtered <- PO4_filtered[,siteorder]

# Saves the tables into csv files
write.csv(NOx_filtered, './data_cache/filtered_Nitrite_+_Nitrate_Nitrogen.csv', col.names = TRUE)
write.csv(PO4_filtered, './data_cache/filtered_Orthophosphate_Phosphorus.csv', col.names = TRUE)

# This creates a plot with the number sites with non-empty readings every year of the filtered data
# Note: The number of entries each year never drops below 40/46, so every year should be well-represented, except for 1979 in the orthophosphate data
# NOTE: In the future, determine the following site characteristics: Land use category,land cover category, seasonal distribution of data collected, geographic distribution
datSelect2 <- tibble(NOx_filtered[,"Year"], rowSums(!is.na(NOx_filtered[,-1])),rowSums(!is.na(PO4_filtered[,-1])))
names(datSelect2) <- c('Year','NOx_Entries','PO4_Entries')

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = NOx_Entries, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))


###############################################################################
# Plotting time series

# This center the data around each site's median, and convert it into a long table for plotting
NOx_filtered2 <- NOx_filtered %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  demedian() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var="Year") 
NOx_filtered2$Year <- as.integer(NOx_filtered2$Year)

# Creates a long frame of absolute values
NOx_filtered3 <- NOx_filtered %>% reshape2::melt(id.var="Year") 

# Each site is plotted by itself and presented in a grid of time series
ggplot(NOx_filtered2, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median NO2-3, Median-Centered") +
  scale_y_continuous(name = "NO2/NO3, mg/L")

ggplot(NOx_filtered3, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median NO2-3, Absolute Values") +
  scale_y_continuous(name = "NO2/NO3, mg/L")

# Does the same for orthophosphate
PO4_filtered2 <- PO4_filtered %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>% # avoids taking the median year
  demedian() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var="Year")
PO4_filtered2$Year <- as.integer(PO4_filtered2$Year)

PO4_filtered3 <- PO4_filtered %>% reshape2::melt(id.var="Year")

# Each site is plotted by itself and presented in a grid of time series
ggplot(PO4_filtered2, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Orthophosphate, Median-Centered and Zoomed In") +
  scale_y_continuous(name = "PO4, mg/L", limits = c(-0.05,0.05))

ggplot(PO4_filtered3, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Orthophosphate, Absolute Values") +
  scale_y_continuous(name = "PO4, mg/L")



# Saves list of sites for use in other scripts and to add information, run again if the sites are changed
#locs <- as_data_frame(unique(NOx_filtered2$variable))
#colnames(locs) <- c('Locator')
#write.csv(locs, './data_cache/Selected_Monitoring_Stations.csv')
