# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# Load the Lab Samples
pH_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_pH,_Field.csv') %>% select(- all_of(remove_sites))
pH_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_pH,_Field.csv') %>% select(- all_of(remove_sites))
pH_Monthly$Year_mon <- as.yearmon(pH_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
pH_Entries <- tibble(as.data.frame(pH_Annual)['Year'], rowSums(!is.na(pH_Annual[,-1])))
names(pH_Entries) <- c('Year', 'Entries')
ggplot(pH_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('pH Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note descrepancy in the code)
# Results: x sites, 
# This function will calculate the long term slopes as defined by the function inputs stated above
pH_slopes <- LT_Slope_Dist(pH_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('pH'))
write.csv(pH_slopes,'./data_cache/LongTermTrends/pH_Slopes.csv')

# Get the IQR of the distribution and percent change distribution
pH_quant <- quantile(pH_slopes$`Median Slope (pH/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
pH_pquant <- quantile(pH_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(pH_slopes, aes(x = `Median Slope (pH/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(pH_quant[2], pH_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = pH_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('pH Slope Distribution') 

ggplot(pH_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(pH_pquant[2], pH_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = pH_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('pH Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
pH_lc_mods <- Land_Cover_Modeling(pH_Annual, CoverVariables, param = "pH", window = c(2016, 2022), log_space = FALSE)
pH_LC_results <- pH_lc_mods[[1]]
pH_LC_inputs <- pH_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(pH_lc_mods[[1]],'./data_cache/LandCover/pH_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- pH_lc_mods[["pH = Const. + Developed, All Intensities"]]

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

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.206
#
# rSquared(pH_LC_inputs$mean_Conc, pH_LC_inputs$combined_Resid)

# quantiles
qqnorm(pH_LC_inputs$combined_Resid)
qqline(pH_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(pH_LC_inputs$combined_Pred, pH_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(pH_LC_inputs$mean_Conc, pH_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')


# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

pH_Seasonal <- Seasonal_Analysis(pH_Monthly)

ggplot(pH_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
#  scale_y_continuous(limits = c(-100, 100), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("pH % Monthly Deviations from Annual Median")
