# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# Load the Lab Samples
DO_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Dissolved_Oxygen.csv') %>% select(- all_of(remove_sites))
DO_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Combined_Dissolved_Oxygen.csv') %>% select(- all_of(remove_sites))
DO_Monthly$Year_mon <- as.yearmon(DO_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
DO_Entries <- tibble(as.data.frame(DO_Annual)['Year'], rowSums(!is.na(DO_Annual[,-1])))
names(DO_Entries) <- c('Year', 'Entries')
ggplot(DO_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Dissolved Oxygen Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
DO_slopes <- LT_Slope_Dist(DO_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('mg/L'))
write.csv(DO_slopes,'./data_cache/LongTermTrends/Dissolved_Oxygen_Slopes.csv')

# Get the IQR of the distribution and percent change distribution
DO_quant <- quantile(DO_slopes$`Median Slope (mg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
DO_pquant <- quantile(DO_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(DO_slopes, aes(x = `Median Slope (mg/L/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DO_quant[2], DO_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DO_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Dissolved Oxygen Slope Distribution') 

ggplot(DO_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DO_pquant[2], DO_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DO_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Dissolved Oxygen Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
DO_lc_mods <- Land_Cover_Modeling(DO_Annual, CoverVariables, param = "Dissolved Oxygen", window = c(2016, 2022), log_space = FALSE)
DO_LC_results <- DO_lc_mods[[1]]
DO_LC_inputs <- DO_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(DO_lc_mods[[1]],'./data_cache/LandCover/Dissolved_Oxygen_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- DO_lc_mods[["Dissolved Oxygen = Const. + Developed, All Intensities + Deciduous Forest + Wetlands, Total"]]

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
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

# Deciduous Forest
ggplot() +
  geom_point(aes(top_model$model$c , top_model$residuals)) +
  xlab('% Deciduous Forest') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')
  
# Wetlands, Total
ggplot() +
  geom_point(aes(top_model$model$e , top_model$residuals)) +
  xlab('% Wetlands') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.245
#
# rSquared(DO_LC_inputs$mean_Conc, DO_LC_inputs$combined_Resid)

# quantiles
qqnorm(DO_LC_inputs$combined_Resid)
qqline(DO_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(DO_LC_inputs$combined_Pred, DO_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

DO_Seasonal <- Seasonal_Analysis(DO_Monthly)

ggplot(DO_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-75, 50), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Dissolved Oxygen % Monthly Deviations from Annual Median")



# Measuring DO vs Theoretical Saturation ###################################################
#
#
# This analysis will create a time series of deviation from the estimated saturation point
# I think the first analysis should be the long term trend distribution
#
#

# Initialize the temp data, convert to Kelvin
TempK_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Temperature.csv') %>% 
  select(- all_of(remove_sites)) %>%
  column_to_rownames(var = 'Year') + 273.15 

TempK_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Temperature.csv') %>% 
  select(- all_of(remove_sites)) %>% 
  column_to_rownames(var = 'Year_mon') + 273.15

# Create a data frame of Estimated Henry's Constants, provided by D. Tromans (2000)
#
#
# Henry's Constant Estimations
DOSat_Annual <- exp((0.046*(TempK_Annual^2) + 203.35*TempK_Annual*log(TempK_Annual/298) - (299.378 + 0.092*TempK_Annual)*(TempK_Annual - 298) - 20.591*10^3)/(8.3144*TempK_Annual))
DOSat_Monthly <- exp((0.046*(TempK_Monthly^2) + 203.35*TempK_Monthly*log(TempK_Monthly/298) - (299.378 + 0.092*TempK_Monthly)*(TempK_Monthly - 298) - 20.591*10^3)/(8.3144*TempK_Monthly))
#
#
#
# Convert to DO Saturation
DOSat_Annual <- DOSat_Annual * 0.21 * 32000 * 0.99975
DOSat_Monthly <- DOSat_Monthly * 0.21 * 32000 * 0.99975
#
#
#
# Create Df of DO as a percent of saturation
DOSatP_Annual <- column_to_rownames(DO_Annual, var = 'Year') / DOSat_Annual * 100
DOSatP_Annual <- DOSatP_Annual %>%
  rownames_to_column(var = "Year")
DOSatP_Annual$Year <- as.integer(DOSatP_Annual$Year)

DOSatP_Monthly <- column_to_rownames(DO_Monthly, var = 'Year_mon') / DOSat_Monthly * 100
DOSatP_Monthly <- DOSatP_Monthly %>%
  rownames_to_column(var = "Year_mon")
DOSatP_Monthly$Year_mon <- as.yearmon(DOSatP_Monthly$Year_mon)
## Long Term Trends ####
#
#
#
#
DOSatP_slopes <- LT_Slope_Dist(DOSatP_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('%'))
write.csv(DOSatP_slopes,'./data_cache/LongTermTrends/Dissolved_Oxygen_Saturation_Slopes.csv')

# Get the IQR of the distribution and percent change distribution
DOSatP_quant <- quantile(DOSatP_slopes$`Median Slope (%/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
DOSatP_pquant <- quantile(DOSatP_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(DOSatP_slopes, aes(x = `Median Slope (%/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DOSatP_quant[2], DOSatP_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DOSatP_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('DO Saturation Slopes') 

ggplot(DOSatP_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DOSatP_pquant[2], DOSatP_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DOSatP_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Dissolved Oxygen Slope Distribution, Percent Change') 


## Seasonal Analysis ####

DOSatP_Seasonal <- Seasonal_Analysis(DOSatP_Monthly, form = 'Absolute')

ggplot(DOSatP_Seasonal, aes(x= Month, y= mean_month)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-75, 50), n.breaks = 10) +
  ylab('Deviation from Median') +
  geom_hline(yintercept = median(DOSatP_Seasonal$mean_month), linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Dissolved Oxygen Saturation, Avg Monthly")



