library(data.table)
library(plyr)
library(dplyr)
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

# This creates a plot with the number sites with non-empty readings every year
ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = NOx_Entries, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))

# NOTE: Highest frequency is from 2014 to present, large drop-off at 2008

# NOTE: This will tell the number of data points available for each site
siteSelectN <- sapply(AnnualNO3_NO2, function(x) sum(!is.na(x)))
siteSelectP <- sapply(AnnualNO3_NO2, function(x) sum(!is.na(x)))

# The median number of years with any readings is 31 for N and P

# This loop will filter out all the sites that have less than 31 years of recorded data
NOx_filtered <- tibble(.rows = 54)
PO4_filtered <- tibble(.rows = 54)
for (site in colnames(Annual_OrthoP))
{
  if (siteSelectN[site] >= 31){
    NOx_filtered[,site] <- AnnualNO3_NO2[, site]
  }
  if (siteSelectP[site] >= 31){
    PO4_filtered[,site] <- Annual_OrthoP[, site]
  }
}

datSelect2 <- tibble(NOx_filtered[,"Year"], rowSums(!is.na(NOx_filtered[,-1])),rowSums(!is.na(PO4_filtered[,-1])))
names(datSelect2) <- c('Year','NOx_Entries','PO4_Entries')

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = NOx_Entries, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect2, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))
# This creates a plot with the number sites with non-empty readings every year of the filtered data
# NOTE: The largest cluster of data is between 1979-2008

# NOTE: In the future, determine the following site characteristics: Land use category, 
# land cover category, seasonal distribution of data collected, geographic distribution


# This will set the window where there is the most data, then center the data around each site's median, and convert it into a long table
NOx_filtered <- NOx_filtered %>%
  subset(Year > 1978 & Year < 2009) %>%
  column_to_rownames(var = 'Year') %>%
  demedian() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var="Year")
NOx_filtered$Year <- as.integer(NOx_filtered$Year)
 
# Future notes: add a chart title and change the y-axis labels to be more accurate
ggplot(NOx_filtered, aes(Year, value)) + 
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() 

