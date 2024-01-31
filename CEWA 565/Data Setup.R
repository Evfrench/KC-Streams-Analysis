## Call Libraries & Load Data ##################################################
library(plyr)
library(miscTools)
library(readxl)
library(mgcv)
source('./functions/get_socrata_data_func.R')

# You need TSS and Fecal data, parcel dev records, and that's it. Oh and Current Land Cover

AnnualFec <- as.data.frame(fread('~/KC-Streams-Analysis/data_cache/median_annual_Fecal_Coliform.csv'))

AnnualTSS <- as.data.frame(fread('~/KC-Streams-Analysis/data_cache/median_annual_Total_Suspended_Solids.csv'))

AnnualDev <- readRDS('~/KC-Streams-Analysis/data_cache/watershed_build_years.RDS') %>%  
  reshape2::dcast(YRBUILT ~ Locator, value.var = 'ParcelsBuiltPer100Acres') 

LandCover <- read_excel("data_cache/streams_2019lulc.xlsx", sheet = "LULC - %")
## Check the Frequency of the data ##############################################

colnames(AnnualDev) <- c('Year', colnames(AnnualDev[,-1]))
Dev_Entries <- tibble(AnnualDev['Year'],rowSums(!is.na(AnnualDev[,-1])))
names(Dev_Entries) <- c('Year','Entries')

Fec_Entries <- tibble(AnnualFec['Year'], rowSums(!is.na(AnnualFec[,-1])))
names(Fec_Entries) <- c('Year', 'Entries')

TSS_Entries <- tibble(AnnualTSS['Year'], rowSums(!is.na(AnnualTSS[,-1])))
names(TSS_Entries) <- c('Year', 'Entries')

ggplot(Dev_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Site Development Entries per Year')

ggplot(Fec_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Fecal Coliform Entries per Year')

ggplot(TSS_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('TSS Development Entries per Year')


## Clean The Data ######################################################

# Counts the measured years for each site
# Narrow down to years 1980-2020, and filter out all sites with data in less than half of the years
SelectDev <- AnnualDev %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  sapply(function(x) sum(!is.na(x)))
SelectFec <- AnnualFec %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  sapply(function(x) sum(!is.na(x)))
SelectTSS <- AnnualTSS %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  sapply(function(x) sum(!is.na(x)))

rivers <- c('0305','0307','0309','0311')
# I am removing the following historical sites (0305, 0307, 0309, 0311) 
# these are all in the Duwamish system. The remaining node is 3106 with the most data
# Filters out the data frames, the first time this was run
FilterFec <- AnnualFec[names(SelectFec[SelectFec > 20])] %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  select(- all_of(rivers))

FilterTSS <- AnnualTSS[names(SelectTSS[SelectTSS > 20])] %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  select(- all_of(rivers))

# for ease of use, I want the sites in these two sets to be the same
# It looks like Fecal coliform has one extra site so I will eliminate it and check if they are now equal
FilterFec <- FilterFec[, !names(FilterFec) %in% c('KTHA01')]
identical(colnames(FilterFec),colnames(FilterTSS))

# Success!

# Now to filter down the Site development Time Series
FilterDev <- AnnualDev[colnames(FilterTSS)] %>%
  subset((Year >= 1980 & Year <= 2020))

## Plot The Results ##################################################################

PlotDev <- FilterDev %>% 
  reshape2::melt(id.var = "Year")

PlotFec <- FilterFec %>% 
  reshape2::melt(id.var = "Year")

PlotTSS <- FilterTSS %>% 
  reshape2::melt(id.var = "Year")

# I'm going to use this set of plots to sort the monitoring sites into different groups
ggplot(PlotDev, aes(Year, value)) +
facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Parcels Built per 100 Acres per Year") +
  geom_hline(yintercept = 3, color = 'black', linetype = 'dashed') +
  scale_y_continuous(name = "Parcels/100 Acres")

ggplot(PlotFec, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Fecal Coliform, 1980-2020") +
  scale_y_continuous(name = "CFU", limits = c(0,3000))

ggplot(PlotTSS, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Total Suspended Solids, 1980-2020") +
  scale_y_continuous(name = "mg/L")

## Sort the Data into groups ##################################################

# Make a data frame of just the low development rivers
LowDev <- FilterDev %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  select_if(~ !any(. > 3))
# Note: get rid of the multiple Duwamish sites please.

LowDevRivers <- LandCover %>%
  select(Locator:Stream, `Urban, Total`) %>%
  subset(Locator %in% colnames(LowDev)) %>%
  mutate(Dev_Peak = 'Low Dev')

# Make a data frame of the other sites
OtherDev <- FilterDev %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  select(- all_of(colnames(LowDev)))


# this will return a vector with the column number(year) with the maximum normalized development
max_val <- OtherDev %>% 
  replace(is.na(.), 0) %>%
  t() %>%
  max.col(ties.method = 'first')

# how do I sort this into separate data frames?
# 80's: 1-10
# 90's: 11-20
# 00's: 21-30
# 10's: 31-41

high_dev_names <- colnames(OtherDev)
#Dev_80s <- tibble(FilterDev$Year)
name80 <- character()
#Dev_90s <- tibble(FilterDev$Year)
name90 <- character()
#Dev_00s <- tibble(FilterDev$Year)
name00 <- character()

for (i in 1:length(max_val)){
  if (max_val[i] >= 1 & max_val[i] <= 10){
    #Dev_80s <- Dev_80s %>% add_column(OtherDev[,i])
    name80 <- name80 %>% append(high_dev_names[i])
  }
  if (max_val[i] >= 11 & max_val[i] <= 20){
    #Dev_90s <- Dev_90s %>% add_column(OtherDev[,i])
    name90 <- name90 %>% append(high_dev_names[i])
  }
  if (max_val[i] >= 21 & max_val[i] <= 41){
    #Dev_00s <- Dev_00s %>% add_column(OtherDev[,i])
    name00 <- name00 %>% append(high_dev_names[i])
  }
}

# This will create a list of rivers for each classification, along with a covariate of modern % developed land cover
River80s <- LandCover %>%
  select(Locator:Stream, `Urban, Total`) %>%
  subset(Locator %in% name80) %>%
  mutate(Dev_Peak = '1980s')

River90s <- LandCover %>%
  select(Locator:Stream, `Urban, Total`) %>%
  subset(Locator %in% name90) %>%
  mutate(Dev_Peak = '1990s')

River00s <- LandCover %>%
  select(Locator:Stream, `Urban, Total`) %>%
  subset(Locator %in% name00) %>%
  mutate(Dev_Peak = '2000s')

# Export the time series for site development, TSS, and Fecal Coliform
#Site_Classifications <- rbind(LowDevRivers,River80s,River90s,River00s)
#write.csv(Site_Classifications, file = './data_cache/Site_Dev_Classes.csv')
#write.csv(FilterDev, file = './data_cache/CEWA565_Dev_ts.csv')
#write.csv(FilterTSS, file = './data_cache/CEWA565_TSS_ts.csv')
#write.csv(FilterFec, file = './data_cache/CEWA565_Fec_ts.csv')

## Grouped TSS Plots ###########################################################
# Make the grouped data frames based on peak development
# Then fit a linear model to each group of data and plot that as well

# low dev Group
LowDevTSS <- FilterTSS[c('Year', colnames(LowDev))] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

RegLowDev <- glm(`value` ~ `Year`, data = LowDevTSS)
TrendLowDev <- tibble(Year = c(1980,2020), 
                      TSS = c(RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*1980,
                              RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*2020))

LowDevTSS2 <- LowDevTSS %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

GamLowDev <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = LowDevTSS2, 
                 family = gaussian())

# 1980s Group
`80sTSS` <- FilterTSS[c('Year', name80)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg80s <- glm(`value` ~ `Year`, data = `80sTSS`)
Trend80s <- tibble(Year = c(1980,2020), 
                      TSS = c(Reg80s$coefficients[1] + Reg80s$coefficients[2]*1980,
                              Reg80s$coefficients[1] + Reg80s$coefficients[2]*2020))

`80sTSS2` <- `80sTSS` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam80s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `80sTSS2`, family = gaussian())

# 1990s Group
`90sTSS` <- FilterTSS[c('Year', name90)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg90s <- glm(`value` ~ `Year`, data = `90sTSS`)
Trend90s <- tibble(Year = c(1980,2020), 
                   TSS = c(Reg90s$coefficients[1] + Reg90s$coefficients[2]*1980,
                           Reg90s$coefficients[1] + Reg90s$coefficients[2]*2020))

`90sTSS2` <- `90sTSS` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam90s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `90sTSS2`, family = gaussian())

# 2000s Group
`00sTSS` <- FilterTSS[c('Year', name00)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg00s <- glm(`value` ~ `Year`, data = `00sTSS`)
Trend00s <- tibble(Year = c(1980,2020), 
                   TSS = c(Reg00s$coefficients[1] + Reg00s$coefficients[2]*1980,
                           Reg00s$coefficients[1] + Reg00s$coefficients[2]*2020))

`00sTSS2` <- `00sTSS` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam00s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `00sTSS2`, family = gaussian())

# plot with linear trends
ggplot() +
  geom_point(data= LowDevTSS, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sTSS`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sTSS`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sTSS`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("TSS (mg/L)") + 
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                    name="Peak Development\nPeriod",
                    breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(data= TrendLowDev, aes(Year, TSS), color= 'darkred', size = 1.5) +
  geom_line(data= Trend80s, aes(Year, TSS), color= 'forestgreen', size = 1.5) +
  geom_line(data= Trend90s, aes(Year, TSS), color= 'blue', size = 1.5) +
  geom_line(data= Trend00s, aes(Year, TSS), color= 'orange', size = 1.5) +
  ggtitle('TSS Plots of Each Development Period')
  
# Plot with the smoothed GAMS

ggplot() +
  geom_point(data= LowDevTSS, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sTSS`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sTSS`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sTSS`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("TSS (mg/L)") + 
  #scale_y_continuous(limits = c(,)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevTSS2$Year, GamLowDev$linear.predictors), color= 'darkred', size = 1.5) +
  geom_line(aes(`80sTSS2`$Year, Gam80s$linear.predictors), color= 'forestgreen', size = 1.5) +
  geom_line(aes(`90sTSS2`$Year, Gam90s$linear.predictors), color= 'blue', size = 1.5) +
  geom_line(aes(`00sTSS2`$Year, Gam00s$linear.predictors), color= 'orange', size = 1.5) +
  ggtitle('TSS Plots of Each Development Period')  

# Plot with the linear regression and smoothed GAMs

ggplot() +
  geom_line(data= TrendLowDev, aes(x= Year, y= TSS, color = 'Low Dev')) +
  geom_line(data= Trend80s, aes(x= Year, y= TSS, color = '1980s Peak')) +
  geom_line(data= Trend90s, aes(x= Year, y= TSS, color = '1990s Peak')) +
  geom_line(data= Trend00s, aes(x= Year, y= TSS, color = '2000s Peak')) +
  ylab("TSS (mg/L)") + 
  scale_y_continuous(limits = c(0,8)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevTSS2$Year, GamLowDev$linear.predictors), color= 'darkred', linewidth = 1.5) +
  geom_line(aes(`80sTSS2`$Year, Gam80s$linear.predictors), color= 'forestgreen', linewidth = 1.5) +
  geom_line(aes(`90sTSS2`$Year, Gam90s$linear.predictors), color= 'blue', linewidth = 1.5) +
  geom_line(aes(`00sTSS2`$Year, Gam00s$linear.predictors), color= 'orange', linewidth = 1.5) +
  ggtitle('TSS Plots of Each Development Period') 

 
## Grouped Fecal Coliform Plots ###########################################################

# low dev Group
LowDevFec <- FilterFec[c('Year', colnames(LowDev))] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

RegLowDev <- glm(`value` ~ `Year`, data = LowDevFec)
TrendLowDev <- tibble(Year = c(1980,2020), 
                      Fec = c(RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*1980,
                              RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*2020))

LowDevFec2 <- LowDevFec %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

GamLowDev <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = LowDevFec2, 
                 family = gaussian())

# 1980s Group
`80sFec` <- FilterFec[c('Year', name80)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg80s <- glm(`value` ~ `Year`, data = `80sFec`)
Trend80s <- tibble(Year = c(1980,2020), 
                   Fec = c(Reg80s$coefficients[1] + Reg80s$coefficients[2]*1980,
                           Reg80s$coefficients[1] + Reg80s$coefficients[2]*2020))

`80sFec2` <- `80sFec` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam80s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `80sFec2`, family = gaussian())

# 1990s group
`90sFec` <- FilterFec[c('Year', name90)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg90s <- glm(`value` ~ `Year`, data = `90sFec`)
Trend90s <- tibble(Year = c(1980,2020), 
                   Fec = c(Reg90s$coefficients[1] + Reg90s$coefficients[2]*1980,
                           Reg90s$coefficients[1] + Reg90s$coefficients[2]*2020))

`90sFec2` <- `90sFec` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam90s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `90sFec2`, family = gaussian())

# 2000s Group
`00sFec` <- FilterFec[c('Year', name00)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg00s <- glm(`value` ~ `Year`, data = `00sFec`)
Trend00s <- tibble(Year = c(1980,2020), 
                   Fec = c(Reg00s$coefficients[1] + Reg00s$coefficients[2]*1980,
                           Reg00s$coefficients[1] + Reg00s$coefficients[2]*2020))
`00sFec2` <- `00sFec` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam00s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `00sFec2`, family = gaussian())

# plot With Linear Trends
ggplot() +
  geom_point(data= LowDevFec, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sFec`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sFec`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sFec`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Fecal Coliform (CFU)") + 
  scale_y_continuous(limits = c(-60,3000)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(data= TrendLowDev, aes(Year, Fec), color= 'darkred', size = 1.5) +
  geom_line(data= Trend80s, aes(Year, Fec), color= 'forestgreen', size = 1.5) +
  geom_line(data= Trend90s, aes(Year, Fec), color= 'blue', size = 1.5) +
  geom_line(data= Trend00s, aes(Year, Fec), color= 'orange', size = 1.5) +
  ggtitle('Fecal coliform Plots of Each Development Period') 

# Plot with the smoothed GAMS

ggplot() +
  geom_point(data= LowDevFec, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sFec`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sFec`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sFec`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Fecal Coliform (CFU)") + 
  scale_y_continuous(limits = c(0,3000)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevFec2$Year, GamLowDev$linear.predictors), color= 'darkred', size = 1.5) +
  geom_line(aes(`80sFec2`$Year, Gam80s$linear.predictors), color= 'forestgreen', size = 1.5) +
  geom_line(aes(`90sFec2`$Year, Gam90s$linear.predictors), color= 'blue', size = 1.5) +
  geom_line(aes(`00sFec2`$Year, Gam00s$linear.predictors), color= 'orange', size = 1.5) +
  ggtitle('Fecal Coliform Plots of Each Development Period')  

# Plot with the linear regression and smoothed GAMs

ggplot() +
  geom_line(data= TrendLowDev, aes(x= Year, y= Fec, color = 'Low Dev')) +
  geom_line(data= Trend80s, aes(x= Year, y= Fec, color = '1980s Peak')) +
  geom_line(data= Trend90s, aes(x= Year, y= Fec, color = '1990s Peak')) +
  geom_line(data= Trend00s, aes(x= Year, y= Fec, color = '2000s Peak')) +
  ylab("Fecal Coliform (CFU)") +  
  #scale_y_continuous(limits = c(0,1300)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevFec2$Year, GamLowDev$linear.predictors), color= 'darkred', size = 1.5) +
  geom_line(aes(`80sFec2`$Year, Gam80s$linear.predictors), color= 'forestgreen', size = 1.5) +
  geom_line(aes(`90sFec2`$Year, Gam90s$linear.predictors), color= 'blue', size = 1.5) +
  geom_line(aes(`00sFec2`$Year, Gam00s$linear.predictors), color= 'orange', size = 1.5) +
  ggtitle('Fecal Coliform Plots of Each Development Period')  


   ## Grouped Dissolved Nitrite/Nitrate Analysis ###########################################

AnnualNit <- as.data.frame(fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv'))

FilterNit <- AnnualNit[colnames(FilterTSS)] %>%
  subset((Year >= 1980 & Year <= 2020))

# Low Development Group
LowDevNit <- FilterNit[c('Year', colnames(LowDev))] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

RegLowDev <- glm(`value` ~ `Year`, data = LowDevNit)
TrendLowDev <- tibble(Year = c(1980,2020), 
                      Nit = c(RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*1980,
                              RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*2020))

GamLowDev <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = LowDevNit %>%
                   group_by(Year) %>%
                   summarise(Value = mean(value, na.rm = T)), 
                 family = gaussian())

LowDevNit2 <- LowDevNit %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

# 80's Peak Group
`80sNit` <- FilterNit[c('Year', name80)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg80s <- glm(`value` ~ `Year`, data = `80sNit`)
Trend80s <- tibble(Year = c(1980,2020), 
                   Nit = c(Reg80s$coefficients[1] + Reg80s$coefficients[2]*1980,
                           Reg80s$coefficients[1] + Reg80s$coefficients[2]*2020))


Gam80s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `80sNit` %>%
                group_by(Year) %>%
                summarise(Value = mean(value, na.rm = T)), family = gaussian())
`80sNit2` <- `80sNit` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

# 90's Peak Group
`90sNit` <- FilterNit[c('Year', name90)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg90s <- glm(`value` ~ `Year`, data = `90sNit`)
Trend90s <- tibble(Year = c(1980,2020), 
                   Nit = c(Reg90s$coefficients[1] + Reg90s$coefficients[2]*1980,
                           Reg90s$coefficients[1] + Reg90s$coefficients[2]*2020))


Gam90s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `90sNit` %>%
                group_by(Year) %>%
                summarise(Value = mean(value, na.rm = T)), family = gaussian())

`90sNit2` <- `90sNit` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T)) 

# 2000's Peak Group
`00sNit` <- FilterNit[c('Year', name00)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg00s <- glm(`value` ~ `Year`, data = `00sNit`)
Trend00s <- tibble(Year = c(1980,2020), 
                   Nit = c(Reg00s$coefficients[1] + Reg00s$coefficients[2]*1980,
                           Reg00s$coefficients[1] + Reg00s$coefficients[2]*2020))


Gam00s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `00sNit` %>%
                group_by(Year) %>%
                summarise(Value = mean(value, na.rm = T)), family = gaussian())

`00sNit2` <- `00sNit` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

# Plot with the regression Lines
ggplot() +
  geom_point(data= LowDevNit, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sNit`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sNit`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sNit`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Nitrite/Nitrate (μg/L)") + 
  #scale_y_continuous(limits = c(,)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(data= TrendLowDev, aes(Year, Nit), color= 'darkred', size = 1.5) +
  geom_line(data= Trend80s, aes(Year, Nit), color= 'forestgreen', size = 1.5) +
  geom_line(data= Trend90s, aes(Year, Nit), color= 'blue', size = 1.5) +
  geom_line(data= Trend00s, aes(Year, Nit), color= 'orange', size = 1.5) +
  ggtitle('Nitrite+Nitrate Plots of Each Development Period') 

# Plot with the smoothed GAMS

ggplot() +
  geom_point(data= LowDevNit, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sNit`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sNit`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sNit`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Nitrite/Nitrate (μg/L)") + 
  #scale_y_continuous(limits = c(,)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevNit2$Year, GamLowDev$linear.predictors), color= 'darkred', size = 1.5) +
  geom_line(aes(`80sNit2`$Year, Gam80s$linear.predictors), color= 'forestgreen', size = 1.5) +
  geom_line(aes(`90sNit2`$Year, Gam90s$linear.predictors), color= 'blue', size = 1.5) +
  geom_line(aes(`00sNit2`$Year, Gam00s$linear.predictors), color= 'orange', size = 1.5) +
  ggtitle('Nitrite+Nitrate Plots of Each Development Period') 

# Plot with the linear regression and smoothed GAMs

ggplot() +
  geom_line(data= TrendLowDev, aes(x= Year, y= Nit, color = 'Low Dev')) +
  geom_line(data= Trend80s, aes(x= Year, y= Nit, color = '1980s Peak')) +
  geom_line(data= Trend90s, aes(x= Year, y= Nit, color = '1990s Peak')) +
  geom_line(data= Trend00s, aes(x= Year, y= Nit, color = '2000s Peak')) +
  ylab("Nitrite/Nitrate (μg/L)") + 
  scale_y_continuous(limits = c(0,1300)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(LowDevNit2$Year, GamLowDev$linear.predictors), color= 'darkred', linewidth = 1.5) +
  geom_line(aes(`80sNit2`$Year, Gam80s$linear.predictors), color= 'forestgreen', linewidth = 1.5) +
  geom_line(aes(`90sNit2`$Year, Gam90s$linear.predictors), color= 'blue', linewidth = 1.5) +
  geom_line(aes(`00sNit2`$Year, Gam00s$linear.predictors), color= 'orange', linewidth = 1.5) +
  ggtitle('Nitrite+Nitrate Plots of Each Development Period') 

## Grouped Dissolved Phosphorus Analysis ##################################################

AnnualPh <- as.data.frame(fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv'))

FilterPh <- AnnualPh[colnames(FilterTSS)] %>%
  subset((Year >= 1980 & Year <= 2020))

# Low Dev Group

LowDevPh <- FilterPh[c('Year', colnames(LowDev))] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

RegLowDev <- glm(`value` ~ `Year`, data = LowDevPh)
TrendLowDev <- tibble(Year = c(1980,2020), 
                      Ph = c(RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*1980,
                              RegLowDev$coefficients[1] + RegLowDev$coefficients[2]*2020))

LowDevPh2 <- LowDevPh %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

GamLowDev <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = LowDevPh2, family = gaussian())


# 80's peak group

`80sPh` <- FilterPh[c('Year', name80)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg80s <- glm(`value` ~ `Year`, data = `80sPh`)
Trend80s <- tibble(Year = c(1980,2020), 
                   Ph = c(Reg80s$coefficients[1] + Reg80s$coefficients[2]*1980,
                           Reg80s$coefficients[1] + Reg80s$coefficients[2]*2020))

`80sPh2` <- `80sPh` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam80s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `80sPh2`, family = gaussian())


# 90's Peak Group

`90sPh` <- FilterPh[c('Year', name90)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg90s <- glm(`value` ~ `Year`, data = `90sPh`)
Trend90s <- tibble(Year = c(1980,2020), 
                   Ph = c(Reg90s$coefficients[1] + Reg90s$coefficients[2]*1980,
                           Reg90s$coefficients[1] + Reg90s$coefficients[2]*2020))

`90sPh2` <- `90sPh` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam90s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `90sPh2`, family = gaussian())

# 2000s Peak Group

`00sPh` <- FilterPh[c('Year', name00)] %>%
  remove_rownames() %>%
  reshape2::melt(id.var = 'Year')

Reg00s <- glm(`value` ~ `Year`, data = `00sPh`)
Trend00s <- tibble(Year = c(1980,2020), 
                   Ph = c(Reg00s$coefficients[1] + Reg00s$coefficients[2]*1980,
                           Reg00s$coefficients[1] + Reg00s$coefficients[2]*2020))

`00sPh2` <- `00sPh` %>%
  group_by(Year) %>%
  summarise(Value = mean(value, na.rm = T))

Gam00s <- gam(`Value` ~ s(`Year`, k = 10, bs = 'tp'), data = `00sPh2`, family = gaussian())

ggplot() +
  geom_point(data= LowDevPh, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sPh`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sPh`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sPh`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Phosphate (μg/L)") + 
  #scale_y_continuous(limits = c(0,250)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(data= TrendLowDev, aes(Year, Ph), color= 'darkred', size = 1.5) +
  geom_line(data= Trend80s, aes(Year, Ph), color= 'forestgreen', size = 1.5) +
  geom_line(data= Trend90s, aes(Year, Ph), color= 'blue', size = 1.5) +
  geom_line(data= Trend00s, aes(Year, Ph), color= 'orange', size = 1.5) +
  ggtitle('Orthophosphate Phosphorus Plots of Each Development Period') 


# Plot with the smoothed GAMS

ggplot() +
  geom_point(data= LowDevPh, aes(x= Year, y= value, color = 'Low Dev')) +
  geom_point(data= `80sPh`, aes(x= Year, y= value, color = '1980s Peak')) +
  geom_point(data= `90sPh`, aes(x= Year, y= value, color = '1990s Peak')) +
  geom_point(data= `00sPh`, aes(x= Year, y= value, color = '2000s Peak')) +
  ylab("Phosphate (μg/L)") + 
  #scale_y_continuous(limits = c(0,300)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(x =GamLowDev$Model$Year, y= GamLowDev$linear.predictors), color= 'darkred', size = 1.5) +
  geom_line(aes(x= Gam80s$Model$Year, y= Gam80s$linear.predictors), color= 'forestgreen', size = 1.5) +
  geom_line(aes(x= Gam90s$Model$Year, y= Gam90s$linear.predictors), color= 'blue', size = 1.5) +
  geom_line(aes(x= Gam00s$Model$Year, y= Gam00s$linear.predictors), color= 'orange', size = 1.5) +
  ggtitle('Orthophosphate Phosphorus Plots of Each Development Period') 

# Plot with the linear regression and smoothed GAMs

ggplot() +
  geom_line(data= TrendLowDev, aes(x= Year, y= Ph, color = 'Low Dev')) +
  geom_line(data= Trend80s, aes(x= Year, y= Ph, color = '1980s Peak')) +
  geom_line(data= Trend90s, aes(x= Year, y= Ph, color = '1990s Peak')) +
  geom_line(data= Trend00s, aes(x= Year, y= Ph, color = '2000s Peak')) +
  ylab("Phosphate (μg/L)") + 
  #scale_y_continuous(limits = c(0,75)) +
  scale_color_manual(values=c("darkred", "forestgreen", "blue","orange"), 
                     name="Peak Development\nPeriod",
                     breaks=c('Low Dev', '1980s Peak', '1990s Peak','2000s Peak')) +
  geom_line(aes(x= GamLowDev$Model$Year, y= GamLowDev$linear.predictors), color= 'darkred', linewidth = 1.5) +
  geom_line(aes(x= Gam80s$Model$Year, y= Gam80s$linear.predictors), color= 'forestgreen', linewidth = 1.5) +
  geom_line(aes(x= Gam90s$Model$Year, y= Gam90s$linear.predictors), color= 'blue', linewidth = 1.5) +
  geom_line(aes(x= Gam00s$Model$Year, y= Gam00s$linear.predictors), color= 'orange', linewidth = 1.5) +
  ggtitle('Orthophosphate Phosphorus Plots of Each Development Period') 


# The following streams have some abnormally high values in the 80's
# 3106, Green River
# 0317, Springbrook
# A315, Mill Creek
# 0322, Nuwaukum Creek


### Plotting the Outlier Streams ##########################################

ggplot(data = LowDevPh %>% subset(variable %in% c('3106','0317','A315','0322')),
       aes(Year,value)) +
  facet_wrap(. ~ variable) +
  geom_point() +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  ggtitle('Sites with notably high Orthophosphate Concentrations') +
  ylab('Phosphate (μg/L)')

# Whats going on with these? Check their current land use, maybe there are farms? Green River makes sense

# Separating the Low Dev group ######################################
# Start by importing the data frame I made in python
site_summary <-  as.data.frame(fread('~/KC-Streams-Analysis/data_cache/ANOVA_sites.csv'))

site_summary['Dev_Peak'][site_summary['Dev_Peak'] == 'Low Dev' & site_summary['% Basin Developed'] >= 50] <- 'Dev Before 1980'

ggplot() +
  geom_point(data= site_summary, aes(x= `% Basin Developed`, y= TSS_mean, color = Dev_Peak), size = 3) +
  scale_color_manual(values=c( "forestgreen", "blue","orange","darkred","purple")) +
  ggtitle('Mean Site TSS Concentrations vs % Developed Land, \nSeparated by Peak Dev Group') +
  ylab('Mean TSS (mg/L)')

ggplot() +
  geom_point(data= site_summary, aes(x= `% Basin Developed`, y= TSS_logmean, color = Dev_Peak), size = 3) +
  scale_color_manual(values=c( "forestgreen", "blue","orange","darkred","purple")) +
  ggtitle('Log Mean Site TSS Concentrations vs % Developed Land, \nSeparated by Peak Dev Group') +
  ylab('Log Mean TSS')


ggplot() +
  geom_point(data= site_summary, aes(x= `% Basin Developed`, y= Fecal_mean, color = Dev_Peak), size = 3) +
  scale_color_manual(values=c( "forestgreen", "blue","orange","darkred","purple")) +
  ggtitle('Mean Site Fecal Coliform Concentrations vs % Developed Land, \nSeparated by Peak Dev Group') +
  ylab('Mean Fecal Coliform (CFU)')

ggplot() +
  geom_point(data= site_summary, aes(x= `% Basin Developed`, y= Fecal_logmean, color = Dev_Peak), size = 3) +
  scale_color_manual(values=c( "forestgreen", "blue","orange","darkred","purple")) +
  ggtitle('Log Mean Site Fecal Coliform Concentrations vs % Developed Land, \nSeparated by Peak Dev Group') +
  ylab('Log Mean Fecal Coliform')
