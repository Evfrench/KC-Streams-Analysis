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

AnnualNO3_NO2 <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('annual'))
Annual_OrthoP <- summarize_WQ_data(bigTable, c('Orthophosphate_Phosphorus'), c('annual'))

datSelect <- tibble(AnnualNO3_NO2[,"Year"], rowSums(!is.na(AnnualNO3_NO2[,-1])),rowSums(!is.na(Annual_OrthoP[,-1])))
names(datSelect) <- c('Year','NO2/3_Entries','PO4_Entries')
datSelect <- subset(datSelect, (Year > 1978 & Year < 2009) | (Year > 2012 & Year < 2023))
###############################################################################
# Data Cleaning

# This creates a plot with the number sites with non-empty readings every year
ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = `NO2/3_Entries`, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))

# NOTE: Highest frequency is from 2013 to present, large drop-off at 2008
# NOTE: The a large cluster of data is between 1979-2008, this may be a good window to start with
# UPDATE: This is has been changed to include both the of the clusters of data.
N_filtered <- subset(AnnualNO3_NO2, (Year > 1978 & Year < 2009) | (Year > 2012 & Year < 2023))
P_filtered <- subset(Annual_OrthoP, (Year > 1978 & Year < 2009) | (Year > 2012 & Year < 2023))

# NOTE: This will tell the number of data points available for each site, the maximum is 30 in the baseline and 10 in the test group
siteSelectNbase <- N_filtered %>% 
  subset(Year > 1978 & Year < 2009) %>% 
  sapply(function(x) sum(!is.na(x)))

siteSelectNtest <- N_filtered %>% 
  subset(Year > 2012 & Year < 2023) %>% 
  sapply(function(x) sum(!is.na(x)))

siteSelectPbase <- P_filtered %>% 
  subset(Year > 1978 & Year < 2009) %>% 
  sapply(function(x) sum(!is.na(x)))

siteSelectPtest <- P_filtered %>% 
  subset(Year > 2012 & Year < 2023) %>% 
  sapply(function(x) sum(!is.na(x)))

# The median number of years with any readings is 31 for N and P

# This loop will filter out all the sites that have less than 2/3 of the total window filled out
# We end up with 46 of the 92 sites, not bad
# UPDATE: The new loop includes 2 separate intervals. Each site must have half of the years in the test group, and at least 5 in the baseline
#N_filtered <- tibble(.rows = 54)
#P_filtered <- tibble(.rows = 54)
for (site in colnames(Annual_OrthoP))
{
  if (siteSelectNbase[site] < 5 | siteSelectNtest[site] < 5){
    N_filtered <- N_filtered %>% select(- all_of(site))
  }
  if (siteSelectPbase[site] < 5 | siteSelectPtest[site] < 5){
    P_filtered <- P_filtered %>% select(- all_of(site))
  }
}

# Orders the columns alphabetically, this will be helpful for later data QC
siteorder <- order(colnames(N_filtered))
N_filtered <- N_filtered[,siteorder]
P_filtered <- P_filtered[,siteorder]

# Saves the tables into csv files
write.csv(N_filtered, './data_cache/filtered_Nitrite_+_Nitrate_Nitrogen.csv', col.names = TRUE)
write.csv(P_filtered, './data_cache/filtered_Orthophosphate_Phosphorus.csv', col.names = TRUE)

# This creates a plot with the number sites with non-empty readings every year of the filtered data
# Note: The number of entries each year never drops below 40/46, so every year should be well-represented, except for 1979 in the orthophosphate data
# NOTE: In the future, determine the following site characteristics: Land use category,land cover category, seasonal distribution of data collected, geographic distribution
datSelect2 <- tibble(N_filtered[,"Year"], rowSums(!is.na(N_filtered[,-1])),rowSums(!is.na(P_filtered[,-1])))
names(datSelect2) <- c('Year','NO2/3_Entries','PO4_Entries')

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = `NO2/3_Entries`, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))


###############################################################################
# Plotting time series

# This center the data around each site's median, and convert it into a long table for plotting
N_filtered2 <- N_filtered %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  demedian() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var="Year") 
N_filtered2$Year <- as.integer(N_filtered2$Year)

# Creates a long frame of absolute values
N_filtered3 <- N_filtered %>% reshape2::melt(id.var="Year") 

# Each site is plotted by itself and presented in a grid of time series
ggplot(N_filtered2, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median NO2-3, Median-Centered") +
  scale_y_continuous(name = "NO2/NO3, μg/L")

ggplot(N_filtered3, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median NO2-3, Absolute Values") +
  scale_y_continuous(name = "NO2/NO3, μg/L")

# Does the same for orthophosphate
P_filtered2 <- P_filtered %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>% # avoids taking the median year
  demedian() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var="Year")
P_filtered2$Year <- as.integer(P_filtered2$Year)

P_filtered3 <- P_filtered %>% reshape2::melt(id.var="Year")

# Each site is plotted by itself and presented in a grid of time series
ggplot(P_filtered2, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Orthophosphate, Median-Centered and Zoomed In") +
  scale_y_continuous(name = "PO4, μg/L", limits = c(-0.05,0.05))

ggplot(P_filtered3, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Orthophosphate, Absolute Values") +
  scale_y_continuous(name = "PO4, μg/L")



# Saves list of sites for use in other scripts and to add information, run again if the sites are changed
#locs <- as_data_frame(unique(N_filtered2$variable))
#colnames(locs) <- c('Locator')
#write.csv(locs, './data_cache/Selected_Monitoring_Stations.csv')
