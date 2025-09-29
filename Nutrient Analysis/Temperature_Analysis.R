# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# Load the Lab Samples
Temp_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Temperature.csv') %>% select(- all_of(remove_sites))  #%>%
#  column_to_rownames(var = 'Year') + 273.15 
#Temp_Annual <- Temp_Annual %>%
#  column_to_rownames(var = 'Year') + 273.15 
#Temp_Annual <- Temp_Annual %>%
#  rownames_to_column(var = 'Year') %>%
#  mutate(Year = as.numeric(Year))

#write_csv(Temp_Annual, file = './data_cache/NutrientData/median_annual_Temperature.csv', col_names = TRUE)
#write_csv(Temp_Monthly, file = './data_cache/NutrientData/mean_monthly_Temperature.csv', col_names = TRUE)


Temp_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Temperature.csv') %>% select(- all_of(remove_sites)) %>%
#  column_to_rownames(var = 'Year_mon') + 273.15
#Temp_Monthly <- Temp_Monthly %>%
#  rownames_to_column(var = 'Year_mon') %>%
  mutate(Year_mon = as.yearmon(Year_mon), .before = 1)


# Plot the number of entries per year with the fixed code
Temp_Entries <- tibble(as.data.frame(Temp_Annual)['Year'], rowSums(!is.na(Temp_Annual[,-1])))
names(Temp_Entries) <- c('Year', 'Entries')
ggplot(Temp_Entries, aes(x = Year, y = Entries)) +.
  geom_col() +
  ggtitle('Temperature Entries per Year')

# Consider using Temperature to calculate DO saturation, and compare to DO conc ################



# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note descrepancy in the code)
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
Temp_slopes <- LT_Slope_Dist(Temp_Annual, units = c('deg. C'))
write.csv(Temp_slopes[[1]],'./data_cache/LongTermTrends/Temperature_Slopes.csv')
write.csv(Temp_slopes[[2]],'./data_cache/ZscoreTrends/Temperature_Scores.csv')
write.csv(Temp_slopes[[4]],'./data_cache/ZscoreTrends/Temperature_Values.csv')

if (quantile(Temp_slopes[[1]]$`Mean Slope (deg. C/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(Temp_slopes[[1]]$`Mean Slope (deg. C/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(Temp_slopes[[1]]$`Mean Slope (deg. C/decade)`, alternative = 'greater')
}

# Get the IQR of the distribution and percent change distribution
Temp_quant <- quantile(Temp_slopes[[1]]$`Mean Slope (deg. C/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Temp_pquant <- quantile(Temp_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(Temp_slopes[[1]], aes(x = `Mean Slope (deg. C/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Temp_quant[2], Temp_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Temp_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Temperature Slope Distribution') 

ggplot(Temp_slopes[[1]], aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Temp_pquant[2], Temp_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Temp_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Temperature Slope Distribution, Percent Change') 

## Comparing these long term trends as z-scores #######################

Temp_slopes[[3]]+
  ggtitle("Temperature Annual Z-score Distribution") 

# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
Temp_lc_mods <- Land_Cover_Modeling(Temp_Annual, CoverVariables, param = "Temperature", window = c(2016, 2022), log_space = FALSE)
Temp_LC_results <- Temp_lc_mods[[1]]
Temp_LC_inputs <- Temp_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(Temp_lc_mods[[1]],'./data_cache/LandCover/Temperature_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- Temp_lc_mods[["Temperature = Const. + Developed, All Intensities + Deciduous Forest + Open Water"]]

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
  xlab('% Deciduous Forest') +
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
# R-squared = 0.459
#
# rSquared(Temp_LC_inputs$mean_Conc, Temp_LC_inputs$combined_Resid)

# quantiles
qqnorm(Temp_LC_inputs$combined_Resid)
qqline(Temp_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(Temp_LC_inputs$combined_Pred, Temp_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(Temp_LC_inputs$mean_Conc, Temp_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Temp_Seasonal <- Seasonal_Analysis(Temp_Monthly, form = 'Mean-Dev')

ggplot(Temp_Seasonal, aes(x= Month, y= mean_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-7, 7), n.breaks = 11) +
  #scale_y_log10() +
  ylab('Deviation from Mean (deg. C)') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Temperature Monthly Deviations from Annual Mean")

Temp_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.5) < 0) {
    test <- wilcox.test(subset(Temp_Seasonal, Month == i)$mean_annual_dev, mu =0, alternative = 'less')
  } else {
    test <- wilcox.test(subset(Temp_Seasonal, Month == i)$mean_annual_dev, mu =0, alternative = 'greater')
  }
  
  Temp_Q_Months <- Temp_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.1),
                                           `25th` = quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.25),
                                           `50th` = quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.5),
                                           `75th` = quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.75),
                                           `90th` = quantile(subset(Temp_Seasonal, Month == i)$mean_annual_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write_csv(Temp_Q_Months, file = './data_cache/SeasonalityResults/Temperature_Monthly_Dist.csv')

# Extra Stuff

Temp_Table <- inner_join(rownames_to_column(Temp_slopes, var = 'Locator'),Temp_LC_inputs, by = 'Locator')
write_csv(Temp_Table,'./data_cache/Misc/Temperature_Combined.csv')

# Separate Time Series into Hot and Cold
Hot_months <- Temp_Monthly %>%
  pivot_longer(!Year_mon, values_to = 'Temp', names_to = 'Site') %>%
  mutate(Site=ifelse(Site=='0450CC','0450', 
                     ifelse(Site=='0484A','0484', Site))) %>%
  mutate(Month = month(Year_mon), 
         Year = year(Year_mon),
         .before = 1) %>%
  subset(Month %in% c(7:8)) %>%
  group_by(Site, Year) %>%
  summarise(Temp = mean(Temp, na.rm = T))

Cold_months <- Temp_Monthly %>%
  pivot_longer(!Year_mon, values_to = 'Temp', names_to = 'Site') %>%
  mutate(Site=ifelse(Site=='0450CC','0450', 
                     ifelse(Site=='0484A','0484', Site))) %>%
  mutate(Month = month(Year_mon), 
         Year = year(Year_mon),
         .before = 1) %>%
  subset(Month %in% c(1:3,12)) %>%
  group_by(Site, Year) %>%
  summarise(Temp = mean(Temp, na.rm = T))

Hot_Cold_Temp <- Hot_months %>% 
  full_join(Cold_months, by = c('Year', 'Site')) %>%
  rename(Temp_H = Temp.x, Temp_C = Temp.y) %>%
  subset(Year >= 1979 & Year <= 2022)


ggplot(data = Hot_Cold_Temp) +
  facet_wrap(.~ Site) +
  geom_point(aes(x = Year, y= Temp_H, color = 'Summer (Jun-Sep)')) +
  geom_point(aes(x = Year, y= Temp_C, color = 'Winter (Dec-Mar)')) +
  xlab('Year') +
  ylab('Temperature')

Seasonal_temp_mod <- lm(Temp_H ~ Temp_C, data = Hot_Cold_Temp)

summary(Seasonal_temp_mod)

# Fitting the warm months to land cover data ################################################

Warm_Annual_Temp <- Hot_months %>%
  mutate(Temp = ifelse(is.nan(Temp) , NA, Temp)) %>%
  pivot_wider(id_cols = Year, values_from = Temp, names_from = Site)

Cold_Annual_Temp <- Cold_months %>%
  mutate(Temp = ifelse(is.nan(Temp) , NA, Temp)) %>%
  pivot_wider(id_cols = Year, values_from = Temp, names_from = Site)

Summer_lc_mods <- Land_Cover_Modeling(Warm_Annual_Temp, CoverVariables, param = "Temperature", window = c(2016, 2022), log_space = FALSE)
Summer_LC_results <- Summer_lc_mods[[1]]
Summer_LC_inputs <- Summer_lc_mods[[2]]

Winter_lc_mods <- Land_Cover_Modeling(Cold_Annual_Temp, CoverVariables, param = "Temperature", window = c(2016, 2022), log_space = FALSE)
Winter_LC_results <- Winter_lc_mods[[1]]
Winter_LC_inputs <- Winter_lc_mods[[2]]

write.csv(Summer_LC_results,'./data_cache/Misc/Jun_Sep_Temp_LandCover_Models.csv')
write.csv(Winter_LC_results,'./data_cache/Misc/Dec_Mar_Temp_LandCover_Models.csv')
