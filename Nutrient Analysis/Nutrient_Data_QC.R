################################################################################
#
# This file is meant to analyze the quality of the data in the Nutrient Summaries file
#
################################################################################
library(plyr)
library(dplyr)
library(data.table)
bigTable <- fread('./data_cache/KC_WQ_Data')
sites <- readr::read_csv("data_cache/Selected_Monitoring_Stations.csv")

# Subset the data to the established years of 1979 to 2008
timeWindow <- subset(bigTable, Year > 1978 & Year < 2009)

################################################################################
#
# NO2/NO3
#
# Remove empty cells and keep monitoring sites that were selected in the nutrient summaries file
Nit <- timeWindow[,c('SampleID', 'CollectDate','Year','Month','Locator',
              #  'Orthophosphate_Phosphorus')]
              'Nitrite_+_Nitrate_Nitrogen')]
Nit <- Nit[complete.cases(Nit[,6]),]
Nit <- Nit %>% subset(Locator %in% sites$Locator) 

ggplot(Nit, aes(x = Month)) + 
  ggtitle("NO2-3, Monthly Readings, 1979-2008") +
  geom_histogram(stat = 'count') + 
  scale_x_discrete(limits = c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec'))

ggplot(Nit[order(Nit$Locator)], aes(x = Month)) + 
  ggtitle("NO2-3, Monthly Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()

timeWindow <- subset(bigTable, Year > 1978 & Year < 2009)

################################################################################
#
# Orthophosphate
#
# Remove empty cells and keep monitoring sites that were selected in the nutrient summaries file
Orth <- timeWindow[,c('SampleID', 'CollectDate','Year','Month','Locator',
                       'Orthophosphate_Phosphorus')]
                    # 'Nitrite_+_Nitrate_Nitrogen')]
Orth <- Orth[complete.cases(Orth[,6]),]
Orth <- Orth %>% subset(Locator %in% sites$Locator) 

ggplot(Orth, aes(x = Month)) + 
  ggtitle("PO4, Monthly Readings, 1979-2008") +
  geom_histogram(stat = 'count') + 
  scale_x_discrete(limits = c('Jan','Feb','Mar','Apr','May','Jun','July','Aug','Sep','Oct','Nov','Dec'))

ggplot(Orth[order(Orth$Locator)], aes(x = Month)) + 
  ggtitle("PO4, Monthly Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()