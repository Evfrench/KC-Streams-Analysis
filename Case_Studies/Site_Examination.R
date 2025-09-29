# Source Function and Libraries #####################################
source('./functions/get_socrata_data_func.R')
library(factoextra)
  # Put together the sites for PCA using long term trends

Nit <- fread('./data_cache/LongTermTrends/Nitrate_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
TN <- fread('./data_cache/LongTermTrends/TN_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Phos <- fread('./data_cache/LongTermTrends/Phosphate_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
TP <- fread('./data_cache/LongTermTrends/TP_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
TSS <- fread('./data_cache/LongTermTrends/TSS_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
FC <- fread('./data_cache/LongTermTrends/fecal_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Alk <- fread('./data_cache/LongTermTrends/Alkalinity_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Cond <- fread('./data_cache/LongTermTrends/Conductivity_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
DO <- fread('./data_cache/LongTermTrends/Dissolved_Oxygen_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Temp <- fread('./data_cache/LongTermTrends/Temperature_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Am <- fread('./data_cache/LongTermTrends/Ammonia_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
pH <- fread('./data_cache/LongTermTrends/pH_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))
Turb <- fread('./data_cache/LongTermTrends/Turbidity_Slopes.csv') %>%
  select(all_of(c('V1','% Change Per Decade')))



#Trends <- FC %>% 
#  full_join(TN, by = 'V1') %>%
#  full_join(Am, by = 'V1') %>%
#  full_join(TSS, by = 'V1') %>%
#  full_join(Phos, by = 'V1') %>%
#  full_join(Nit, by = 'V1') %>%
#  full_join(DO, by = 'V1') %>%
#  full_join(pH, by = 'V1') %>%
#  full_join(TP, by = 'V1') %>%
#  full_join(Turb, by = 'V1') %>%
#  full_join(Alk, by = 'V1') %>%
#  full_join(Temp, by = 'V1') %>%
#  full_join(Cond, by = 'V1') %>%
  #column_to_rownames(var = 'V1') %>%
#  drop_na()
#colnames(Trends) <- c('F. Coliform','Total-N','NH4-N','TSS','PO4-P','NO3-N','DO','pH','Total-P',' Turb.','Alk.','Temp.','Cond.')
#colnames(Trends) <- c('Site','FCB','TN','NH4-N','TSS','PO4-P','NO3-N','DO','pH','TP',' Turb.','Alk.','Temp.','Cond.')


Trends <- Cond %>%
  full_join(Temp, by = 'V1') %>%
  full_join(Alk, by = 'V1') %>%
  full_join(Turb, by = 'V1') %>%
  full_join(pH, by = 'V1') %>%
  full_join(TP, by = 'V1') %>%
  full_join(DO, by = 'V1') %>%
  full_join(Nit, by = 'V1') %>%
  full_join(Phos, by = 'V1') %>%
  full_join(TSS, by = 'V1') %>%
  full_join(Am, by = 'V1') %>%
  full_join(TN, by = 'V1') %>%
  full_join(FC, by = 'V1') %>%
  #column_to_rownames(var = 'V1') %>%
  drop_na()

colnames(Trends) <- c('Site','Cond.','Temp.','Alk.','Turb.','pH','TP','DO','NO3-N','PO4-P','TSS','NH4-N','TN','FCB')
Trends <- Trends %>%
  reshape2::melt(id.vars = 'Site', value.name = '% Change per Decade', variable.name = "Constituent")

ggplot(Trends, aes(x= Constituent, y= `% Change per Decade`)) +
  geom_boxplot(aes(group= Constituent), outliers = FALSE) +
  scale_x_discrete(breaks = unique(Trends$Constituent)) +
  xlab(NULL) +
  scale_y_continuous(limits = c(-40,40), n.breaks = 9) +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) + 
  geom_vline(xintercept = c(3.5,6.5), linetype = 'solid', color = 'black', linewidth = 0.75) +
  annotate('text', x= 2, y= 30, label= 'Positive Trend', size = rel(7.0)) +
  annotate('text', x= 5, y= 30, label= 'No Trend', size = rel(7.0)) +
  annotate('text', x= 10, y= 30, label= 'Negative Trend', size = rel(7.0)) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text = element_text(size = 17),
        title = element_text(size = 13)) +
  coord_flip() +
  ggtitle("Comparison of Long-Term Trends")
  
#pairs.panels(Trends, smooth = FALSE, scale = TRUE, lm = TRUE, cex.cor = 2)

Decomp <- prcomp(Trends, scale. = TRUE)
loads <- as.data.frame(Decomp$rotation)[, 1:3] %>%
  rownames_to_column(var = "Constituent")

ggplot(data = loads, aes(y = PC1, x = Constituent)) +
  geom_col() +
  scale_x_discrete(limits = (loads %>% arrange(PC1))$Constituent)

ggplot(data = loads, aes(y = PC2, x = Constituent)) +
  geom_col() +
  scale_x_discrete(limits = (loads %>% arrange(PC2))$Constituent)

ggplot(data = loads, aes(y = PC3, x = Constituent)) +
  geom_col() +
  scale_x_discrete(limits = (loads %>% arrange(PC3))$Constituent)

fviz_eig(Decomp)

fviz_pca_contrib(Decomp, choice = "var", axes = 1)
fviz_pca_contrib(Decomp, choice = "var", axes = 2)
fviz_pca_contrib(Decomp, choice = "var", axes = 3)
fviz_pca_contrib(Decomp, choice = "var", axes = 4)

# 1st PC: Turb, PO4-P, TSS, FCB, Alk, Temp | Total-P, Total-N, Cond, NO3-N, NH4-N, DO, pH
# 2nd PC: pH, NH4-N, total-P, NO3-N, DO, total-N | Cond, Alk, Temp, FCB, TSS, Turb, PO4-P
# 3rd PC: NO3-N, DO, Cond, Temp, NH4-N, FCB | PO4-P, Total-N, Alk, Total-P, pH, Turb, TSS

fviz_pca_contrib(Decomp, choice = "ind", axes = 1)
fviz_pca_contrib(Decomp, choice = "ind", axes = 2)
fviz_pca_contrib(Decomp, choice = "ind", axes = 3)
fviz_pca_contrib(Decomp, choice = "ind", axes = 4)

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
  
  
# PCA of recent concentrations. How do they stack up? what groups are there? ###############################################
  
  
  
  Nit <- Nitrate_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  TN <- TN_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Phos <- Phosphate_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  TP <- TP_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  TSS <- TSS_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  FC <- Fecal_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Alk <- Alk_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Cond <- cond_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  DO <- DO_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Temp <- Temp_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Am <- Ammonia_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  pH <- pH_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  Turb <- Turb_LC_inputs %>%
    select(all_of(c('Locator','mean_Conc')))
  
  
  Concentrations <- Cond %>% full_join(Temp, by = 'Locator') %>%
    full_join(Alk, by = 'Locator') %>%
    full_join(Turb, by = 'Locator') %>%
    full_join(TP, by = 'Locator') %>%
    full_join(pH, by = 'Locator') %>%
    full_join(DO, by = 'Locator') %>%
    full_join(Nit, by = 'Locator') %>%
    full_join(Phos, by = 'Locator') %>%
    full_join(TSS, by = 'Locator') %>%
    full_join(Am, by = 'Locator') %>%
    full_join(TN, by = 'Locator') %>%
    full_join(FC, by = 'Locator') %>%
    drop_na()
  colnames(Concentrations) <- c('Locator','Cond.','Temp.','Alk.',' Turb.','Total-P','pH','DO','NO3-N','PO4-P','TSS','NH4-N','Total-N','F. Coliform')
  write_csv(Concentrations, file = "./data_cache/Misc/Recent_Concentration.csv")
  
  Concentrations <- fread(file = "./data_cache/Misc/Recent_Concentration.csv") %>% column_to_rownames(var = 'Locator')
  
  pairs.panels(Concentrations, smooth = FALSE, scale = TRUE, lm = TRUE, cex.cor = 3, jiggle = TRUE, main = 'Comtemporary Constituent Concentrations')
  
  
  Decomp <- prcomp(Concentrations, scale. = TRUE)

  loads <- as.data.frame(Decomp$rotation)[, 1:3] %>%
    rownames_to_column(var = "Constituent")
  
  ggplot(data = loads, aes(y = PC1, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC1))$Constituent) +
    ggtitle(label = 'PC 1 Loading')
  
  ggplot(data = loads, aes(y = PC2, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC2))$Constituent) +
    ggtitle(label = 'PC 2 Loading')
  
  ggplot(data = loads, aes(y = PC3, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC3))$Constituent) +
    ggtitle(label = 'PC 3 Loading')
  
  fviz_eig(Decomp)
  # NOTES- 1st PC: 44%, 2nd PC: 23%, 3rd PC: 11%
  # Sum of 4 eigenvalues: 84% of variance
  
fviz_pca_contrib(Decomp, choice = "var", axes = 1)
fviz_pca_contrib(Decomp, choice = "var", axes = 2)
fviz_pca_contrib(Decomp, choice = "var", axes = 3)
fviz_pca_contrib(Decomp, choice = "var", axes = 4)

  #NOTES- Params contributions (in order)  above expected | below expected
  # 1st PC: Total P, PO4-P, Cond, Alk, Turb, FCB | DO, NH4-N, Temp, TSS, Total-N, NO3-N, pH
  # 2nd PC: pH, NO3-N, Total-N, DO, NH4-N | Turb, Cond, Total-P, Alk, PO4-P, FCB, Temp, TSS
  # 3rd PC: TSS, Alk, Cond, Total-N, NO3-N, Temp, Turb | pH. NH4-N, DO, Total-P, PO4-P, FCB
  # 4th PC: FCB, TSS, Temp, DO | PO4-P, Total-N, pH, NO3-N, NH4-N, Alk, Turb, Total-P, Cond

fviz_pca_contrib(Decomp, choice = "ind", axes = 1)
fviz_pca_contrib(Decomp, choice = "ind", axes = 2)
fviz_pca_contrib(Decomp, choice = "ind", axes = 3)
  
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
  
  biplot(Decomp, choices = c(1,2))
  
  biplot(Decomp, choices = c(2,3))
  
  ## Top and bottom 20% of developed, top and bottom 20% of forested? Seasonal stuff?? #########
  
  Concentrations2 <- Concentrations %>% inner_join(LandCover %>% select(all_of(c('Locator','Developed, Total','Forest, Total','Deciduous Forest','Stream'))), by = 'Locator')
  
  ConcentrationsDev <- Concentrations2 %>% 
    subset(`Developed, Total` >= quantile(Concentrations2$`Developed, Total`, probs = 0.8))
  
  ConcentrationsFor <- Concentrations2 %>% 
    subset(`Forest, Total` >= quantile(Concentrations2$`Forest, Total`, probs = 0.8))
  
  
  # Another Trends Examination #######################
  
  Nit <- fread('./data_cache/LongTermTrends/Nitrate_Slopes.csv')[,c(1,6)]
  TN <- fread('./data_cache/LongTermTrends/TN_Slopes.csv')[,c(1,6)]
  Phos <- fread('./data_cache/LongTermTrends/Phosphate_Slopes.csv')[,c(1,6)]
  TP <- fread('./data_cache/LongTermTrends/TP_Slopes.csv')[,c(1,6)]
  TSS <- fread('./data_cache/LongTermTrends/TSS_Slopes.csv')[,c(1,6)]
  FC <- fread('./data_cache/LongTermTrends/fecal_Slopes.csv')[,c(1,6)]
  Alk <- fread('./data_cache/LongTermTrends/Alkalinity_Slopes.csv')[,c(1,6)]
  Cond <- fread('./data_cache/LongTermTrends/Conductivity_Slopes.csv')[,c(1,6)] 
  DO <- fread('./data_cache/LongTermTrends/Dissolved_Oxygen_Slopes.csv')[,c(1,6)]
  Temp <- fread('./data_cache/LongTermTrends/Temperature_Slopes.csv')[,c(1,6)]
  Am <- fread('./data_cache/LongTermTrends/Ammonia_Slopes.csv')[,c(1,6)]
  pH <- fread('./data_cache/LongTermTrends/pH_Slopes.csv')[,c(1,6)]
  Turb <- fread('./data_cache/LongTermTrends/Turbidity_Slopes.csv')[,c(1,6)]
  #DOsat <- fread('./data_cache/LongTermTrends/Dissolved_Oxygen_Saturation_Slopes.csv')[,c(1,6)]
  #colnames(CoverVariables)[1] <- "V1"
  
  Trends2 <- FC %>% 
    full_join(TN, by = 'V1') %>%
    full_join(Am, by = 'V1') %>%
    full_join(TSS, by = 'V1') %>%
    full_join(Phos, by = 'V1') %>%
    full_join(Nit, by = 'V1') %>%
    full_join(DO, by = 'V1') %>%
    full_join(pH, by = 'V1') %>%
    full_join(TP, by = 'V1') %>%
    full_join(Turb, by = 'V1') %>%
    full_join(Alk, by = 'V1') %>%
    full_join(Temp, by = 'V1') %>%
    full_join(Cond, by = 'V1') %>%
    #left_join(CoverVariables[,c(1,3)], by = 'V1') %>%
    #full_join(DOsat, by = 'V1') %>%
    column_to_rownames(var = 'V1') %>%
    drop_na()
  colnames(Trends2) <- c('FCB','Total-N','NH4-N','TSS','PO4-P','NO3-N','DO','pH','Total-P',' Turb.','Alk.','Temp.','Cond.')
  # Slopes of all sites in concentration units 
  pairs.panels(Trends2, smooth = FALSE, scale = T, lm = TRUE, cex.cor = 3, stars = FALSE, jiggle = TRUE, main = "Stream Water Quality Trends (In Concentration Units)")
  
  # Slopes of NH4, TSS, PO4, TP, Turb in concentration units, minus 0317 
  pairs.panels(Trends2[-c(2,36),c(3,4,5,9,10)], smooth = FALSE, scale = F, lm = TRUE, cex.cor = 3)
  

  
  Decomp <- prcomp(Trends2[-2,], scale. = TRUE)
  loads <- as.data.frame(Decomp$rotation)[, 1:3] %>%
    rownames_to_column(var = "Constituent")
  
  ggplot(data = loads, aes(y = PC1, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC1))$Constituent)
  
  ggplot(data = loads, aes(y = PC2, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC2))$Constituent)
  
  ggplot(data = loads, aes(y = PC3, x = Constituent)) +
    geom_col() +
    scale_x_discrete(limits = (loads %>% arrange(PC3))$Constituent)
  
  fviz_eig(Decomp)
  

  # Examining individual sites ################
  Alk_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Alkalinity.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc') %>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Am_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Ammonia_Nitrogen.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc') %>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Cond_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Conductivity.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  DO_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Dissolved_Oxygen.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Fec_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Fecal_Coliform.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Nit_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Nitrite_+_Nitrate_Nitrogen.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  pH_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_pH,_Field.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Srp_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Orthophosphate_Phosphorus.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Temp_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Temperature.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  TN_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Nitrogen.csv')%>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  TP_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Phosphorus.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  TSS_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Suspended_Solids.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  Turb_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Turbidity.csv') %>% pivot_longer(!Year, names_to = 'Site', values_to = 'Conc')%>% drop_na() %>% subset(Year <= 2022 & Year >= 1979)
  
  
  # Uuuuuuh all the data for springbrook is gone except for nitrate nitrogen
  
  ## Springbrook and Mill Creek #############################################
  
  ggplot(data = Srp_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Orthophosphate, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'ug/L')
  
  ggplot(data = TP_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Total Phosphorus, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'ug/L')
  
  ggplot(data = Am_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Ammonium Nitrogen, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'ug/L')
  
  ggplot(data = TSS_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Total Suspended Solids, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'mg/L')
  
  ggplot(data = Turb_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Turbidity, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'NTU')
  
  ggplot(data = Fec_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Fecal Coliform, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'CFU/100mL')
  
  ggplot(data = Alk_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Total Alkalinity, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'mgCaCO3/L')
  
  ggplot(data = Cond_Annual %>% subset(Site == '0317' | Site == 'A315'), aes(x = Year, y = Conc)) +
    facet_wrap(.~ Site, ncol = 1) +
    geom_point() +
    ggtitle('Conductivity, Springbrook and Mill Creek') +
    scale_x_continuous(name = 'Year') +
    scale_y_log10(name = 'umhos/cm')
  
  ## Springbrook Creek 0317 ###################
  
  QuantileGamRun(SiteCode = c('0317','A315'), Params = c('Orthophosphate Phosphorus'))
  
  QuantileGamRun(SiteCode = c('0317','A315'), Params = c('Total Phosphorus'))
  
  QuantileGamRun(SiteCode = '0317', Params = c('Ammonia Nitrogen'))
  
  QuantileGamRun(SiteCode = c('0317','A315'), Params = c('Total Suspended Solids'))
  
  QuantileGamRun(SiteCode = c('0317','A315'), Params = c('Turbidity'))
  
  QuantileGamRun(SiteCode = '0317', Params = c('Total Alkalinity'))
  
  QuantileGamRun(SiteCode = c('0317','A315'), Params = c('Conductivity, Field','Conductivity'))
  
  QuantileGamRun(SiteCode = '0317', Params = c('Orthophosphate Phosphorus','Total Phosphorus','Ammonia Nitrogen','Total Suspended Solids','Turbidity','Total Alkalinity','Conductivity, Field','Conductivity','Fecal Coliform'))
  
  QuantileGamRun(SiteCode = '0317', Params = c('Fecal Coliform'))
  
  
  QuantileGamRun(SiteCode = 'SKYKOMISH', Params = c('Nitrite + Nitrate Nitrogen'))
  
  ## Mill Creek A315 ########################
  
  QuantileGamRun(SiteCode = 'VA45A', Params = c('Total Nitrogen', 'Nitrite + Nitrate Nitrogen','Ammonia Nitrogen'))
  
  QuantileGamRun(SiteCode = 'A499', Params = c('Total Nitrogen', 'Nitrite + Nitrate Nitrogen','Ammonia Nitrogen'))
  
  QuantileGamRun(SiteCode = 'A315', Params = c('Total Nitrogen', 'Nitrite + Nitrate Nitrogen','Ammonia Nitrogen'))

  QuantileGamRun(SiteCode = 'C320', Params = c('Total Nitrogen', 'Nitrite + Nitrate Nitrogen','Ammonia Nitrogen'))
  
  QuantileGamRun(SiteCode = 'VA42A', Params = 'Fecal Coliform')
  
  QuantileGamRun(SiteCode = 'A315', Params = 'Fecal Coliform')

  Mill <- QuantileGamRun(SiteCode = 'A315', Params = 'Fecal Coliform')
  
  QuantileGamRun(SiteCode = c('0456', 'A456','0456A'), Params = c('Orthophosphate Phosphorus','Total Phosphorus',"Fecal Coliform",'Nitrite + Nitrate Nitrogen'))
  
  Test <- QuantileGamRun(SiteCode = 'A456', Params = 'Total Phosphorus')

Test2 <-  get_socrata_data_func(locns = '0456', parms = 'Total Phosphorus', SiteType = 'Streams and Rivers')
QuantileGamRun(SiteCode = 'VA42A', Params = 'Fecal Coliform')


# Average Monthly nitrate between Jan 2005 and Dec 2006 ###################

remove_sites <- c('0305','0307','0308','0309','3106', 
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

Nit_Hood_Canal_Comparison <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Nitrite_+_Nitrate_Nitrogen.csv') %>% select(- all_of(remove_sites)) %>%
  mutate(Year_mon = as.yearmon(Year_mon), .before = 1) %>%
  #subset(Year_mon >= 'Jan 2005' & Year_mon <= 'Dec 2006') %>%
  subset(Year_mon >= 'Jan 2016' & Year_mon <= 'Dec 2022') %>%
  pivot_longer(!Year_mon, names_to = 'Locator', values_to = 'NO2/NO3', values_drop_na = T) %>%
  group_by(Locator) %>%
  summarise(AvgNO3 = mean(`NO2/NO3`)) %>% 
  left_join(CoverVariables, by = 'Locator') %>% arrange(by = `Developed, Total`) 

  