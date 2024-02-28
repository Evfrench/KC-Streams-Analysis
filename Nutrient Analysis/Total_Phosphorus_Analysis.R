# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

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
TP_slopes <- LT_Slope_Dist(TP_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('μg/L'))

# Get the IQR of the distribution and percent change distribution
TP_quant <- quantile(TP_slopes$`Median Slope (μg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
TP_pquant <- quantile(TP_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(TP_slopes, aes(x = `Median Slope (μg/L/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TP_quant[2], TP_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TP_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Total Phosphorus Slope Distribution') 

ggplot(TP_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(TP_pquant[2], TP_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = TP_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Total Phosphorus Slope Distribution, Percent Change') 


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
  scale_y_continuous(limits = c(-3,3)) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Deciduous Forest
ggplot() +
  geom_point(aes(top_model$model$c , top_model$residuals)) +
  xlab('% Decidous Forest') +
  scale_y_continuous(limits = c(-3,3)) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Agriculture, Total
ggplot() +
  geom_point(aes(top_model$model$d , top_model$residuals)) +
  xlab('% Agricultural Land') +
  scale_y_continuous(limits = c(-3,3)) +
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

TP_Seasonal <- Seasonal_Analysis(TP_Monthly)

ggplot(TP_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100, 200), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Total Phosphorus % Monthly Deviations from Annual Median")

