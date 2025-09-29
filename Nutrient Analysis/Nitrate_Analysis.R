# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106', 
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# If already run once, these will load the frames from the data cache
Nit_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Nitrite_+_Nitrate_Nitrogen.csv') %>% select(- all_of(remove_sites))
Nit_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Nitrite_+_Nitrate_Nitrogen.csv') %>% select(- all_of(remove_sites))
Nit_Monthly$Year_mon <- as.yearmon(Nit_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
Nit_Entries <- tibble(as.data.frame(Nit_Annual)['Year'], rowSums(!is.na(Nit_Annual[,-1])))
names(Nit_Entries) <- c('Year', 'Entries')
ggplot(Nit_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Nitrate Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
Nitrate_slopes <- LT_Slope_Dist(Nit_Annual, units = c('μg/L'))
write.csv(Nitrate_slopes[[1]],'./data_cache/LongTermTrends/Nitrate_Slopes.csv')
write.csv(Nitrate_slopes[[2]],'./data_cache/ZscoreTrends/Nitrate_Scores.csv')
write.csv(Nitrate_slopes[[4]],'./data_cache/ZscoreTrends/Nitrate_Values.csv')


# Get the IQR of the distribution and percent change distribution
Nit_quant <- quantile(Nitrate_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Nit_pquant <- quantile(Nitrate_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

if (quantile(Nitrate_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(Nitrate_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(Nitrate_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'greater')
}


# Make histograms of the resulting distributions
ggplot(Nitrate_slopes[[1]], aes(x = `Mean Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 50) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Nit_quant[2], Nit_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Nit_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Nitrate Slope Distribution') 

ggplot(Nitrate_slopes[[1]], aes(x = `% Change Per Decade`)) +
  geom_histogram(binwidth = 3) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Nit_pquant[2], Nit_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Nit_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Nitrate Slope Distribution, Percent Change') 


## Comparing these long term trends as z-scores #######################

Nitrate_slopes[[3]]+
  ggtitle("Nitrate Annual Z-score Distribution") 

# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Fits all of the models I originally looped through myself automatically
Nit_lc_mods <- Land_Cover_Modeling(Nit_Annual, CoverVariables, param = "Nitrate", window = c(2016, 2022), log_space = FALSE)
Nitrate_LC_results <- Nit_lc_mods[[1]]
Nitrate_LC_inputs <- Nit_lc_mods[[2]]
# Saves the results table in a CSV
write.csv(Nit_lc_mods[[1]],'./data_cache/LandCover/Nitrate_LandCover_Models.csv')

pairs.panels(Nitrate_LC_inputs[, c(6:10)], smooth = FALSE, scale = FALSE, lm = FALSE, cex.cor = 0.5, main = 'Scatterplot Matrix for Landcover Data')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values

# I looked through the results table and picked the "best" model
top_model <- Nit_lc_mods[["Nitrate = Const. + Developed, All Intensities + Deciduous Forest + Open Water"]]

# quantiles
qqnorm(top_model$residuals)
qqline(top_model$residuals)

# By Predicted Values
ggplot() +
  geom_point(aes(top_model$fitted.values, top_model$residuals)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(top_model$y, top_model$residuals)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')

# By Exogenous Variables
# Developed, all intensities
ggplot() +
  geom_point(aes(top_model$model$a , top_model$residuals)) +
  xlab('% Developed, all intensities') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Deciduous Forest
ggplot() +
  geom_point(aes(top_model$model$c , top_model$residuals)) +
  xlab('% Decidous Forest') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Open Water
ggplot() +
  geom_point(aes(top_model$model$f , top_model$residuals)) +
  xlab('% Open Water') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')


## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.318
#
# rSquared(Nitrate_LC_inputs$mean_Conc, Nitrate_LC_inputs$combined_Resid)

# quantiles
qqnorm(Nitrate_LC_inputs$combined_Resid)
qqline(Nitrate_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(Nitrate_LC_inputs$combined_Pred, Nitrate_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(Nitrate_LC_inputs$mean_Conc, Nitrate_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Nit_Seasonal <- Seasonal_Analysis(Nit_Monthly, form = 'Relative')

ggplot(Nit_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100, 250), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Nitrate % Monthly Deviations from Annual Median")


ggplot(Nit_Seasonal, aes(x= Month, y= geo_mean_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-100, 250), n.breaks = 10) +
  scale_y_log10(limits = c(0.3,3)) +
  ylab('Deviation from Median') +
  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Nitrate Monthly Deviations from Annual Median")

Nit_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(Nit_Seasonal, Month == i)$geo_mean_dev, mu = 1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(Nit_Seasonal, Month == i)$geo_mean_dev, mu = 1, alternative = 'greater')
  }
  
  Nit_Q_Months <- Nit_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(Nit_Seasonal, Month == i)$geo_mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(Nit_Q_Months, file = './data_cache/SeasonalityResults/Nitrate_Monthly_Dist.csv')

# Making a matrix of normalized concentrations to match Mike's work ################

Nit_Month <- Nit_Monthly %>%
  reshape2::melt(id.vars = 'Year_mon') %>%
  mutate(Year = year(Year_mon), 
         Month = month(Year_mon, label = T, abbr = F),
         .before = 1) %>%
  subset(Year_mon >= as.yearmon('Oct 1977')) %>%
  group_by(variable) %>%
  rowwise() %>%
  mutate(Wtr_Year = replace(Year, Month %in% c('October', 'November', 'December'), Year+1))  %>%
  select(- all_of('Year'))
 
write.csv(Nit_Month, file = '~/KC-Streams-Analysis/data_cache/Misc/NO3_AllSites.csv')

# Unused Code
#   group_by(Year, variable) %>%
#   mutate(num = sum(!is.na(value))) %>% 
#   subset(num == 12) %>%

# Extra Stuff

NO3_Table <- inner_join(rownames_to_column(Nitrate_slopes, var = 'Locator'),Nitrate_LC_inputs, by = 'Locator') %>%
  subset(Locator %notin% dupes)
write.csv(NO3_Table,'./data_cache/Misc/Nitrate_Combined.csv')

library(factoextra)

LCs <- LandCover[,- 2] %>%
  column_to_rownames(var = "Locator")

Decomp <- prcomp(LCs, scale. = TRUE)

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


# Giving Mike some extra nitrate data ###########################################

NO3_0309 <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Nitrite_+_Nitrate_Nitrogen.csv') %>% 
  mutate(Year_mon = as.yearmon(Year_mon), .before = 1) %>%
  select(all_of(c('Year_mon', '0309', '3106')))

# all sites with 16.89% or less
# all sites with 88.93% or more



WQ_Params <- get_socrata_data_func(locns = '0317', parms = "Nitrite + Nitrate Nitrogen",
                                   SiteType = 'Streams and Rivers') %>%
  mutate(Locator=ifelse(Locator=='FF321','F321',                  #Join past/present locations
                        ifelse(Locator=='A632','0632',
                               ifelse(Locator=='N484A','N484',
                                      ifelse(Locator %in% c('0456','0456A'),'A456',
                                             ifelse(Locator=='X438', '0438',
                                                    ifelse(Locator=='X630','A630',
                                                           Locator))))))) %>%
  mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),  # merge the two DO and conductivity fields
         Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
         Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                 "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L'),
         Censored = if_else(Value <= MDL, TRUE, FALSE, missing = FALSE),
         Date = date(CollectDate),
         Dec_Date = decimal_date(Date) - min(decimal_date(Date))) %>%
  rowwise() %>%
  mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                 "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000)) %>% # Convert nutrient values to micrograms per liter for convenience
  subset(Year >= 1979 & Year <= 2022)

ggplot(data = WQ_Params) +
  scale_y_continuous(name = 'Nitrate ug/L', limits = c(0,2500)) +
  xlab('Date') +
  ggtitle('Nitrate: Raw Example') +
  geom_point(aes(x = CollectDate, y = Value))
  
ggplot(data = Nit_Monthly) +
  scale_y_continuous(name = 'Nitrate ug/L', limits = c(0,2500)) +
  scale_x_continuous(limits = c(1979,2023),name = 'Date') +
  ggtitle('Nitrate: Monthly Example') +
  geom_point(aes(x = Year_mon, y = `0317`))
  
ggplot(data = Nit_Annual) +
  scale_y_continuous(name = 'Nitrate ug/L', limits = c(0,2500)) +
  scale_x_continuous(limits = c(1979,2022),name = 'Date') +
  ggtitle('Nitrate: Annual Example') +
  geom_point(aes(x = Year, y = `0317`))
