
# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# Load the Lab Samples
Cond_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Conductivity.csv') %>% select(- all_of(remove_sites))
Cond_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Combined_Conductivity.csv') %>% select(- all_of(remove_sites))
Cond_Monthly$Year_mon <- as.yearmon(Cond_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
Cond_Entries <- tibble(as.data.frame(Cond_Annual)['Year'], rowSums(!is.na(Cond_Annual[,-1])))
names(Cond_Entries) <- c('Year', 'Entries')
ggplot(Cond_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Conductivity Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note descrepancy in the code)
# Results: x sites, 
#### You need site names tied to row names in this function #######
# This function will calculate the long term slopes as defined by the function inputs stated above
cond_slopes <- LT_Slope_Dist(Cond_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('umhos/cm'))

# Get the IQR of the distribution and percent change distribution
Cond_quant <- quantile(cond_slopes$`Median Slope (umhos/cm/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Cond_pquant <- quantile(cond_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(cond_slopes, aes(x = `Median Slope (umhos/cm/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Cond_quant[2], Cond_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Cond_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Conductivity Slope Distribution') 

ggplot(cond_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Cond_pquant[2], Cond_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Cond_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Conductivity Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
Cond_lc_mods <- Land_Cover_Modeling(Cond_Annual, CoverVariables, param = "Conductivity", window = c(2016, 2022), log_space = FALSE)
cond_LC_results <- Cond_lc_mods[[1]]
# Saves the results table in a CSV
write.csv(Cond_lc_mods[[1]],'./data_cache/LandCover/Conductivity_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- Cond_lc_mods[["Conductivity = Const. + Developed, All Intensities + Deciduous Forest"]]

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

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Cond_Seasonal <- Seasonal_Analysis(Cond_Monthly)

ggplot(Cond_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100, 100), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Conductivity % Monthly Deviations from Annual Median")
