#========================================================================================
#
# This parameter is especially dependent on discharge, prime candidate for WRTDS model
#
#========================================================================================

# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# If already run once, these will load the frames from the data cache
TSS_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Suspended_Solids.csv') %>% select(- all_of(remove_sites))
TSS_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Total_Suspended_Solids.csv') %>% select(- all_of(remove_sites))
TSS_Monthly$Year_mon <- as.yearmon(TSS_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
TSS_Entries <- tibble(as.data.frame(TSS_Annual)['Year'], rowSums(!is.na(TSS_Annual[,-1])))
names(TSS_Entries) <- c('Year', 'Entries')
ggplot(TSS_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('TSS Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note discrepancy in the code)
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
TSS_slopes <- LT_Slope_Dist(TSS_Annual, units = c('mg/L'))
write.csv(TSS_slopes,'./data_cache/LongTermTrends/TSS_Slopes.csv')

if (quantile(TSS_slopes$`Mean Slope (mg/L/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(TSS_slopes$`Mean Slope (mg/L/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(TSS_slopes$`Mean Slope (mg/L/decade)`, alternative = 'greater')
}

# Get the IQR of the distribution and percent change distribution
TSS_quant <- quantile(TSS_slopes$`Mean Slope (mg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
TSS_pquant <- quantile(TSS_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(TSS_slopes, aes(x = `Mean Slope (mg/L/decade)`)) +
  geom_histogram(binwidth = 0.25) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TSS_quant[2], TSS_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TSS_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('TSS Slope Distribution') 

ggplot(TSS_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TSS_pquant[2], TSS_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TSS_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('TSS Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Fits all of the models I originally looped through myself automatically
TSS_lc_mods <- Land_Cover_Modeling(TSS_Annual, CoverVariables, param = "TSS", window = c(2016, 2022), log_space = FALSE)
TSS_LC_results <- TSS_lc_mods[[1]]
TSS_LC_inputs <- TSS_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(TSS_lc_mods[[1]],'./data_cache/LandCover/TSS_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- TSS_lc_mods[["TSS = Const. + Developed, All Intensities + Deciduous Forest + Agriculture, Total"]]

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

# Agriculture, Total
ggplot() +
  geom_point(aes(top_model$model$d , top_model$residuals)) +
  xlab('% Agriculture') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.203
#
# rSquared(TSS_LC_inputs$mean_Conc, TSS_LC_inputs$combined_Resid)

# quantiles
qqnorm(TSS_LC_inputs$combined_Resid)
qqline(TSS_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(TSS_LC_inputs$combined_Pred, TSS_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(TSS_LC_inputs$mean_Conc, TSS_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')


# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

TSS_Seasonal <- Seasonal_Analysis(TSS_Monthly, form = 'Relative')

ggplot(TSS_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100, 400), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("TSS % Monthly Deviations from Annual Median")

TSS_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(TSS_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(TSS_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'greater')
  }
  
  TSS_Q_Months <- TSS_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(TSS_Seasonal, Month == i)$geo_mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(TSS_Q_Months, file = './data_cache/SeasonalityResults/TSS_Monthly_Dist.csv')

#
TSS_Table <- inner_join(rownames_to_column(TSS_slopes, var = 'Locator'),TSS_LC_inputs, by = 'Locator')
write.csv(TSS_Table,'./data_cache/Misc/TSS_Combined.csv')

