# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# If already run once, these will load the frames from the data cache
Turb_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Turbidity.csv') %>% select(- all_of(remove_sites))
Turb_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Turbidity.csv') %>% select(- all_of(remove_sites))
Turb_Monthly$Year_mon <- as.yearmon(Turb_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
Turb_Entries <- tibble(as.data.frame(Turb_Annual)['Year'], rowSums(!is.na(Turb_Annual[,-1])))
names(Turb_Entries) <- c('Year', 'Entries')
ggplot(Turb_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Turbity Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note discrepancy in the code)
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
Turb_slopes <- LT_Slope_Dist(Turb_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('NTU'))
write.csv(Turb_slopes,'./data_cache/LongTermTrends/Turbidity_Slopes.csv')

# Get the IQR of the distribution and percent change distribution
Turb_quant <- quantile(Turb_slopes$`Median Slope (NTU/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Turb_pquant <- quantile(Turb_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(Turb_slopes, aes(x = `Median Slope (NTU/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Turb_quant[2], Turb_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Turb_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Turbidity Slope Distribution') 

ggplot(Turb_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Turb_pquant[2], Turb_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Turb_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Turbidity Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Fits all of the models I originally looped through myself automatically
Turb_lc_mods <- Land_Cover_Modeling(Turb_Annual, CoverVariables, param = "Turbidity", window = c(2016, 2022), log_space = FALSE)
Turb_LC_results <- Turb_lc_mods[[1]]
Turb_LC_inputs <- Turb_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(Turb_lc_mods[[1]],'./data_cache/LandCover/Turbidity_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- Turb_lc_mods[["Turbidity = Const. + Developed, All Intensities + Deciduous Forest + Agriculture, Total"]]

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
# R-squared = 0.222
#
# rSquared(Turb_LC_inputs$mean_Conc, Turb_LC_inputs$combined_Resid)

# quantiles
qqnorm(Turb_LC_inputs$combined_Resid)
qqline(Turb_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(Turb_LC_inputs$combined_Pred, Turb_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(Turb_LC_inputs$mean_Conc, Turb_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')


# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Turb_Seasonal <- Seasonal_Analysis(Turb_Monthly)

ggplot(Turb_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100, 400), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Turbidity % Monthly Deviations from Annual Median")

