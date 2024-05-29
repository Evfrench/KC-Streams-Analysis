# https://pakillo.github.io/R-GIS-tutorial/
# This is the study for Newaukum Creek (0322), a rural basin that strongly follows dimension 2 and weakly along 4
# Stream Gage 40D, 1995-Present
source('./functions/get_socrata_data_func.R')
library(qgam)
library(gratia)

WQ_Params <- get_socrata_data_func(locns = '0322', parms = default_data_parms,
                               SiteType = 'Streams and Rivers') %>%
  mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),
         Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
         Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                 "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L'),
         Censored = if_else(Value <= MDL, TRUE, FALSE, missing = FALSE),
         Dec_Date = decimal_date(CollectDate)) %>%
  rowwise() %>%
  mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                         "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000))


Stream_Flow <- readRDS(file = '~/KC-Streams-Analysis/data_cache/Hydrological/DailyAveFlow_allgages.RDS') %>%
  drop_na() %>%
  mutate(Year_mon = as.yearmon(Date)) %>%
  subset(SITE_CODE == '12108500')
