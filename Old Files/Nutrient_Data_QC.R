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

ggplot(Nit, aes(x = Month)) + 
  ggtitle("NO2-3, Monthly Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()


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

ggplot(Orth, aes(x = Month)) + 
  ggtitle("PO4, Monthly Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()

################################################################################
#
# 2 sites stood out here for having a seasonal bias: 0311 and 3106 (both N and P)
#
################################################################################

ggplot(Nit, aes(x = Year)) + 
  ggtitle("NO2-3, Annual Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()

ggplot(Orth, aes(x = Year)) + 
  ggtitle("PO4, Annual Readings by Locator, 1979-2008") +
  facet_wrap(. ~ Locator) +
  geom_histogram(stat = 'count') + 
  scale_x_discrete()

site0311N <- subset(Nit, Locator == '0311')
site0311P <- subset(Orth, Locator == '0311')

site3106N <- subset(Nit, Locator == '3106')
site3106P <- subset(Orth, Locator == '3106')

ggplot() +
  ggtitle('0311 and 3106 NO2-3 Samples, 1979-2008') +
  geom_point(data = site3106N, aes(CollectDate, `Nitrite_+_Nitrate_Nitrogen`, colour = Locator)) +
  geom_point(data = site0311N, aes(CollectDate, `Nitrite_+_Nitrate_Nitrogen`, colour = Locator))
 
ggplot() +
  ggtitle('0311 and 3106 PO4 Samples, 1979-2008') +
  geom_point(data = site3106P, aes(CollectDate, Orthophosphate_Phosphorus, colour = Locator)) +
  geom_point(data = site0311P, aes(CollectDate, Orthophosphate_Phosphorus, colour = Locator))


# NOTES:  Between 1982-1988 All samples are collected in triplicate for 3106
#         From June-November, samples are collected more frequently (weekly for 3106, 5-10 per month for 0311)
#         From December-May, samples are collected once a month, sometimes twice
################################################################################

# Plots of all points, just to have
ggplot(Nit, aes(x = CollectDate, y = `Nitrite_+_Nitrate_Nitrogen`)) +
  ggtitle('NO2-3, All Sites, 1979-2008') +
  facet_wrap(. ~ Locator) + 
  geom_point()

ggplot(Orth, aes(x = CollectDate, y = Orthophosphate_Phosphorus)) +
  ggtitle('PO4, All Sites, 1979-2008') +
  facet_wrap(. ~ Locator) + 
  geom_point()