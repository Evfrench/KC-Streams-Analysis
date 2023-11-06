## Call Libraries & Load Data ##################################################
library(plyr)
library(miscTools)
library(readxl)
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
SelectFec <- AnnualTSS %>%
  subset((Year >= 1980 & Year <= 2020)) %>%
  sapply(function(x) sum(!is.na(x)))

# Filters out the data frames, the first time this was run
FilterFec <- AnnualFec[names(SelectFec[SelectFec > 20])] %>%
  subset((Year >= 1980 & Year <= 2020))
FilterTSS <- AnnualTSS[names(SelectTSS[SelectTSS > 20])] %>%
  subset((Year >= 1980 & Year <= 2020))

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
  scale_y_continuous(name = "Parcel/100 Acres")

ggplot(PlotFec, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Fecal Coliform, 1980-2020") +
  scale_y_continuous(name = "CCU", limits = c(0,2000))

ggplot(PlotTSS, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Total Suspended Solids, 1980-2020") +
  scale_y_continuous(name = "mg/L")

# Make a data frame of just the low development rivers
LowDev <- FilterDev %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  select_if(~ !any(. > 3))

LowDevRivers <- LandCover %>%
  select(Locator:Stream) %>%
  subset(Locator %in% colnames(LowDev))
LowDevRivers # Note: get rid of the multiple Duwamish sites please.

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


Dev_80s <- tibble(FilterDev$Year)
name80 <- character()
Dev_90s <- tibble(FilterDev$Year)
name90 <- character()
Dev_00s <- tibble(FilterDev$Year)
name00 <- character()
Dev_10s <- tibble(FilterDev$Year)
name10 <- character()

for (i in 1:length(max_val)){
  if (max_val[i] >= 1 & max_val[i] <= 10){
    Dev_80s <- Dev_80s %>% add_column(OtherDev[,i])
    name80 <- append
  }
  if (max_val[i] >= 11 & max_val[i] <= 20){
    Dev_90s <- Dev_90s %>% add_column(OtherDev[,i])
  }
  if (max_val[i] >= 21 & max_val[i] <= 30){
    Dev_00s <- Dev_00s %>% add_column(OtherDev[,i])
  }
  if (max_val[i] >= 31 & max_val[i] <= 41){
    Dev_10s <- Dev_10s %>% add_column(OtherDev[,i])
  }
}

