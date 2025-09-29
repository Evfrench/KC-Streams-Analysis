# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# If already run once, these will load the frames from the data cache
TP_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Phosphorus.csv') %>% select(- all_of(remove_sites))
TP_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Total_Phosphorus.csv') %>% select(- all_of(remove_sites))
TP_Monthly$Year_mon <- as.yearmon(TP_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
TP_Entries <- tibble(as.data.frame(TP_Annual)['Year'], rowSums(!is.na(TP_Annual[,-1])))
names(TP_Entries) <- c('Year', 'Entries')
ggplot(TP_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('TP Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note descrepancy in the code)
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
TP_slopes <- LT_Slope_Dist(TP_Annual, units = c('μg/L'))
write.csv(TP_slopes[[1]],'./data_cache/LongTermTrends/TP_Slopes.csv')
write.csv(TP_slopes[[2]],'./data_cache/ZscoreTrends/TP_Scores.csv')
write.csv(TP_slopes[[4]],'./data_cache/ZscoreTrends/TP_Values.csv')


if (quantile(TP_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(TP_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(TP_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'greater')
}
# Get the IQR of the distribution and percent change distribution
TP_quant <- quantile(TP_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
TP_pquant <- quantile(TP_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(TP_slopes[[1]], aes(x = `Mean Slope (μg/L/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TP_quant[2], TP_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TP_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Total Phosphorus Slope Distribution') 

ggplot(TP_slopes[[1]], aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TP_pquant[2], TP_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TP_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Total Phosphorus Slope Distribution, Percent Change') 

## Comparing these long term trends as z-scores #######################
TP_slopes[[3]]+
  ggtitle("TP Annual Z-score Distribution")


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Fits allll of the models I originally looped through myself automatically
TP_lc_mods <- Land_Cover_Modeling(TP_Annual, CoverVariables, param = "Total Phosphorus", window = c(2016, 2022), log_space = FALSE)
TP_LC_results <- TP_lc_mods[[1]]
TP_LC_inputs <- TP_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(TP_lc_mods[[1]],'./data_cache/LandCover/TP_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- TP_lc_mods[["Total Phosphorus = Const. + Developed, All Intensities + Deciduous Forest + Agriculture, Total"]]

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
  xlab('% Agricultural Land') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.492
#
# rSquared(TP_LC_inputs$mean_Conc, TP_LC_inputs$combined_Resid)

# quantiles
qqnorm(TP_LC_inputs$combined_Resid)
qqline(TP_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(TP_LC_inputs$combined_Pred, TP_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(TP_LC_inputs$mean_Conc, TP_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')


# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

TP_Seasonal <- Seasonal_Analysis(TP_Monthly, form = 'Relative')

ggplot(TP_Seasonal, aes(x= Month, y= geo_mean_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-100, 200), n.breaks = 10) +
  scale_y_log10(limits = c(0.5,2)) +
  ylab('Deviation from Median') +
  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Total Phosphorus Monthly Deviations from Annual Median")

TP_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(TP_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(TP_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'greater')
  }
  
  TP_Q_Months <- TP_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(TP_Seasonal, Month == i)$geo_mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(TP_Q_Months, file = './data_cache/SeasonalityResults/TotalP_Monthly_Dist.csv')

# Extra Stuff

TP_Table <- inner_join(rownames_to_column(TP_slopes, var = 'Locator'),TP_LC_inputs, by = 'Locator')
write.csv(TP_Table,'./data_cache/Misc/TotalPhosphorus_Combined.csv')

