source('./functions/get_socrata_data_func.R')
library(factoextra)
  # Put together the sites for PCA using long term trends

Nit <- read.csv('./data_cache/LongTermTrends/Nitrate_Slopes.csv')
TN <- read.csv('./data_cache/LongTermTrends/TN_Slopes.csv')
Ph <- read.csv('./data_cache/LongTermTrends/Phosphate_Slopes.csv')
TP <- read.csv('./data_cache/LongTermTrends/TP_Slopes.csv')
TSS <- read.csv('./data_cache/LongTermTrends/TSS_Slopes.csv')
FC <- read.csv('./data_cache/LongTermTrends/fecal_Slopes.csv')
Alk <- read.csv('./data_cache/LongTermTrends/Alkalinity_Slopes.csv')
Cond <- read.csv('./data_cache/LongTermTrends/Conductivity_Slopes.csv')
DO <- read.csv('./data_cache/LongTermTrends/Dissolved_Oxygen_Slopes.csv')
Temp <- read.csv('./data_cache/LongTermTrends/Temperature_Slopes.csv')

Trends <- Alk %>% full_join(Cond, by = 'X') %>%
  full_join(DO, by = 'X') %>%
  full_join(FC, by = 'X') %>%
  full_join(Nit, by = 'X') %>%
  full_join(Ph, by = 'X') %>%
  full_join(Temp, by = 'X') %>%
  full_join(TN, by = 'X') %>%
  full_join(TP, by = 'X') %>%
  full_join(TSS, by = 'X')
Trends <- Trends[,- c(3,5,7,9,11,13,15,17,19,21)]
colnames(Trends) <- c('Site','Alk','Cond','DO','Fecal Coliform','Nitrate','Phosphate','Temperature','TN','TP','TSS')
Trends <- Trends[ c(1:43),] %>%
  column_to_rownames(var = 'Site')

Decomp <- prcomp(Trends, scale. = TRUE)

fviz_eig(Decomp)

fviz_pca_ind(Decomp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(Decomp, axes = c(3,4),
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(Decomp,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(Decomp, axes = c(3,4),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(Decomp, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(Decomp, repel = TRUE, axes = c(3,4),
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

biplot(Decomp, choices = c(1,3))

biplot(Decomp, choices = c(2,3))

# I want to choose 0321 rural, 0322 agr, 0446 Urban (urban strong trends), 0484 suburban, A432 (urban weak trends), A499 Cochran Springs

# 0317, 0322, 0434, 0474, 0478, 0631, A315, A320
# 0430, 0444, 0446, 0470, 0484, A631, A656, B434, Cherry_1, C484, N484
# Note the number of samples for each site and time series, Ask KC about summer PO4 between 2005-2010, 57 point above 30 times the median. 



# Let's re-examine the nitrogen seasonal analysis, look at individual site level now #########################
remove_sites <- c('0305','0307','0308','0309','3106')
Nit_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Nitrite_+_Nitrate_Nitrogen.csv') %>% select(- all_of(remove_sites))
Nit_Monthly$Year_mon <- as.yearmon(Nit_Monthly$Year_mon)


Nit_Month <- Nit_Monthly %>%
  reshape2::melt(id.vars = 'Year_mon') %>%
  mutate(Year = year(Year_mon), 
         Month = month(Year_mon, label = T, abbr = F),
         .before = 1) %>%
  subset(Year_mon >= as.yearmon('Oct 1977')) %>%
  group_by(variable) %>%
  reframe(normalized_val = value/mean(value, na.rm = T), # calculates the median concentration for each year, then the monthly deviation from the median
          Year = Year,
          Month = Month) %>%
  rowwise() %>%
  mutate(Wtr_Year = replace(Year, Month %in% c('October', 'November', 'December'), Year+1))  %>%
  select(- all_of('Year'))



  long_table <- Nit_Monthly %>%
    reshape2::melt(id.var='Year_mon') %>% #make the table long
    mutate(Year = year(Year_mon), 
           Month = month(Year_mon, label = T, abbr = F),
           .before = 1) %>% # separate the year_mon into two separate columns, the remove any empty fields
    drop_na() %>%
    group_by(Year, variable) %>%
    mutate(num = n()) %>% 
    subset(num >= 7) %>% # Counts the number of non-empty months per site and year, then removes all years that have less than half a year of data
    group_by(variable,Year) %>%
    reframe(annual_dev = 100*(value - median(value, na.rm=TRUE))/(median(value, na.rm=TRUE)), # calculates the median concentration for each year, then the monthly deviation from the median
            Month = Month) %>%
    group_by(variable,Month) %>%
    reframe(med_annual_dev = median(annual_dev, na.rm= TRUE)) # merges all of years for each site into one average