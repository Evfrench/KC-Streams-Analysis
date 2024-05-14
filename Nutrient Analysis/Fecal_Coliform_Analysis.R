#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
# This script used both normal and log space data, I need to automate this one a little better
#
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106')

# If already run once, these will load the frames from the data cache
Fec_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Fecal_Coliform.csv') %>% select(- all_of(remove_sites))
Fec_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Fecal_Coliform.csv') %>% select(- all_of(remove_sites))
Fec_Monthly$Year_mon <- as.yearmon(Fec_Monthly$Year_mon)

# This set will call the log-space data
Fec_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Fecal_Coliform_log.csv') %>% select(- all_of(remove_sites))
Fec_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Fecal_Coliform_log.csv') %>% select(- all_of(remove_sites))
Fec_Monthly$Year_mon <- as.yearmon(Fec_Monthly$Year_mon)

# This will call E. coli data
Ecoli_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_E._coli.csv') %>% select(- all_of(remove_sites))
Ecoli_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_E._coli.csv') %>% select(- all_of(remove_sites))
Ecoli_Monthly$Year_mon <- as.yearmon(Ecoli_Monthly$Year_mon)

# This set will call the log-space data
#Ecoli_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_E._coli_log.csv') %>% select(- all_of(remove_sites))
#Ecoli_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_E._coli_log.csv') %>% select(- all_of(remove_sites))
#Ecoli_Monthly$Year_mon <- as.yearmon(Ecoli_Monthly$Year_mon)


# Plot the number of entries per year with the fixed code
Fec_Entries <- tibble(as.data.frame(Fec_Annual)['Year'], rowSums(!is.na(Fec_Annual[,-1])))
names(Fec_Entries) <- c('Year', 'Entries')
ggplot(Fec_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Fecal Coliform Entries per Year')

# Plot the number of entries per year with the fixed code
Ecoli_Entries <- tibble(as.data.frame(Ecoli_Annual)['Year'], rowSums(!is.na(Ecoli_Annual[,-1])))
names(Ecoli_Entries) <- c('Year', 'Entries')
ggplot(Ecoli_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('E. Coli Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note discrepancy in the code)
# Results: x sites, 
#### You need site names tied to row names in this function #######
# This function will calculate the long term slopes as defined by the function inputs stated above
fecal_slopes <- LT_Slope_Dist(Fec_Annual, window = c(1979,2008,2013,2020), cutoff = c(5,5), units = c('CFU/100mL'))
write.csv(fecal_slopes,'./data_cache/LongTermTrends/fecal_Slopes.csv')

# Get the IQR of the distribution and percent change distribution
fec_quant <- quantile(fecal_slopes$`Median Slope (CFU/100mL/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
fec_pquant <- quantile(fecal_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(fecal_slopes, aes(x = `Median Slope (log(CFU/100mL)/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(fec_quant[2], fec_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = fec_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Fecal Coliform Log Slope Distribution') 

ggplot(fecal_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(fec_pquant[2], fec_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = fec_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Fecal Coliform Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Switch to log-space data
Fec_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Fecal_Coliform_log.csv') %>% select(- all_of(remove_sites))

# Fits all of the models I originally looped through myself automatically
fec_lc_mods <- Land_Cover_Modeling(Fec_Annual, CoverVariables, param = "Fecal_Coliform", window = c(2016, 2022))
Fecal_LC_results <- fec_lc_mods[[1]]
Fecal_LC_inputs <- fec_lc_mods[[2]]
# Saves the results table in a CSV
write.csv(fec_lc_mods[[1]],'./data_cache/LandCover/Fec_Coli_LandCover_Models.csv')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values


top_model <- fec_lc_mods[["Fecal_Coliform = Const. + Developed, All Intensities + Deciduous Forest + Agriculture, Total"]]

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

# Agriculture
ggplot() +
  geom_point(aes(top_model$model$d , top_model$residuals)) +
  xlab('% Agriculture') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.532
#
# rSquared(Fecal_LC_inputs$mean_Conc, Fecal_LC_inputs$combined_Resid)

# quantiles
qqnorm(Fecal_LC_inputs$combined_Resid)
qqline(Fecal_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(Fecal_LC_inputs$combined_Pred, Fecal_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(Fecal_LC_inputs$mean_Conc, Fecal_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Fec_Seasonal <- Seasonal_Analysis(Fec_Monthly)

ggplot(Fec_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
#  scale_y_continuous(limits = c(-250, 1000), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Fecal Coliform % Monthly Deviations from Annual Median")

Ecoli_Seasonal <- Seasonal_Analysis(Ecoli_Monthly)

ggplot(Ecoli_Seasonal, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
#  scale_y_continuous(limits = c(-250, 1000), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("E. Coli % Monthly Deviations from Annual Median")
