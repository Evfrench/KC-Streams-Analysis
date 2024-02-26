# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# Load the Lab Samples
Temp_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Temperature.csv') %>% select(- all_of(remove_sites))
Temp_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Temperature.csv') %>% select(- all_of(remove_sites))
Temp_Monthly$Year_mon <- as.yearmon(Temp_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
Temp_Entries <- tibble(as.data.frame(Temp_Annual)['Year'], rowSums(!is.na(Temp_Annual[,-1])))
names(Temp_Entries) <- c('Year', 'Entries')
ggplot(Temp_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Temperature Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note descrepancy in the code)
# Results: x sites, 
#### You need site names tied to row names in this function #######
# This function will calculate the long term slopes as defined by the function inputs stated above
Temp_slopes <- LT_Slope_Dist(Temp_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('deg.C'))

# Get the IQR of the distribution and percent change distribution
Temp_quant <- quantile(Temp_slopes$`Median Slope (deg.C/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Temp_pquant <- quantile(Temp_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(Temp_slopes, aes(x = `Median Slope (deg.C/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Temp_quant[2], Temp_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Temp_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Temperature Slope Distribution') 

ggplot(Temp_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Temp_pquant[2], Temp_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Temp_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Temperature Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
Temp_lc_mods <- Land_Cover_Modeling(Temp_Annual, CoverVariables, param = "Temperature", window = c(2016, 2022), log_space = FALSE)
Temp_LC_results <- Temp_lc_mods[[1]]
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
  xlab('% Deciduous Forest') +
  scale_y_continuous(limits = c(-3,3)) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Open Water
ggplot() +
  geom_point(aes(top_model$model$f , top_model$residuals)) +
  xlab('% Open Water') +
  scale_y_continuous(limits = c(-3,3)) +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Temp_Seasonal <- Seasonal_Analysis(Temp_Monthly)

ggplot(Temp_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-75, 50), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Temperature % Monthly Deviations from Annual Median")
