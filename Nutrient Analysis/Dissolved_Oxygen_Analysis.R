# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

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
DO_slopes <- LT_Slope_Dist(DO_Annual, units = c('mg/L'))
write.csv(DO_slopes[[1]],'./data_cache/LongTermTrends/Dissolved_Oxygen_Slopes.csv')
write.csv(DO_slopes[[2]],'./data_cache/ZscoreTrends/Dissolved_Oxygen_Scores.csv')
write.csv(DO_slopes[[4]],'./data_cache/ZscoreTrends/Dissolved_Oxygen_Values.csv')


if (quantile(DO_slopes[[1]]$`Mean Slope (mg/L/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(DO_slopes[[1]]$`Mean Slope (mg/L/decade)`, alternative = 'less', exact = FALSE)
} else {
  results <- wilcox.test(DO_slopes[[1]]$`Mean Slope (mg/L/decade)`, alternative = 'greater')
}

# Get the IQR of the distribution and percent change distribution
DO_quant <- quantile(DO_slopes[[1]]$`Mean Slope (mg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
DO_pquant <- quantile(DO_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(DO_slopes[[1]], aes(x = `Mean Slope (mg/L/decade)`)) +
  geom_histogram(binwidth = 0.05) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DO_quant[2], DO_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DO_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Dissolved Oxygen Slope Distribution') 

ggplot(DO_slopes[[1]], aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(DO_pquant[2], DO_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = DO_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Dissolved Oxygen Slope Distribution, Percent Change') 

## Comparing these long term trends as z-scores #######################

DO_slopes[[3]]+
  ggtitle("DO Annual Z-score Distribution") 
# correlate median z-scores with temperature

# Test for fitting a linear model to the z-score data. Possibly by site? is this possible?
test_lm <- lm(Z_scores~Year, data = DO_slopes[[2]], subset = Locator=="A315")
summary.lm(test_lm)
### Playing with some time series shenanigans #####
#library(xts)
#library(fable)
#test_ts <- as_tsibble(DO_Monthly %>% mutate(Year_mon = yearmonth(Year_mon)), index = Year_mon) %>% fill_gaps(.full = TRUE)
#test_arima <- test_ts %>% model(ARIMA(A315 ~ Year_mon))





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

DO_Seasonal <- Seasonal_Analysis(DO_Monthly, form = 'Relative')

ggplot(DO_Seasonal, aes(x= Month, y= geo_mean_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-75, 50), n.breaks = 10) +
  #scale_y_log10() +
  scale_y_log10(limits = c(0.8,1.2), n.breaks = 8) +
  ylab('Multiples of Median') +
  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Dissolved Oxygen")

DO_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(DO_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(DO_Seasonal, Month == i)$geo_mean_dev, mu =1, alternative = 'greater')
  }
  
  DO_Q_Months <- DO_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(DO_Seasonal, Month == i)$geo_mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(DO_Q_Months, file = './data_cache/SeasonalityResults/DO_Monthly_Dist.csv')

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
DOSatP_slopes <- LT_Slope_Dist(DOSatP_Annual, units = c('%'))
write.csv(DOSatP_slopes,'./data_cache/LongTermTrends/Dissolved_Oxygen_Saturation_Slopes.csv')

if (quantile(DOSatP_slopes$`Mean Slope (%/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(DOSatP_slopes$`Mean Slope (%/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(DOSatP_slopes$`Mean Slope (%/decade)`, alternative = 'greater')
}

# Get the IQR of the distribution and percent change distribution
DOSatP_quant <- quantile(DOSatP_slopes$`Mean Slope (%/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
DOSatP_pquant <- quantile(DOSatP_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(DOSatP_slopes, aes(x = `Mean Slope (%/decade)`)) +
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


# Fits all of the models I originally looped through myself automatically
write_csv(DOSatP_Annual, './data_cache/NutrientData/median_annual_Dissolved_Oxygen_Saturation.csv', col_name=TRUE)
DOSatP_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Dissolved_Oxygen_Saturation.csv')

DOSat_lc_mods <- Land_Cover_Modeling(DOSatP_Annual, CoverVariables, param = "Oxygen Saturation", window = c(2016, 2022), log_space = FALSE)
DOsat_LC_results <- DOSat_lc_mods[[1]]
DOsat_LC_inputs <- DOSat_lc_mods[[2]]

# Saves the results table in a CSV
write.csv(DOSat_lc_mods[[1]],'./data_cache/LandCover/Dissolved_Oxygen_Saturation_LandCover_Models.csv')


# Extra Stuff

DO_Table <- inner_join(rownames_to_column(DO_slopes, var = 'Locator'),DO_LC_inputs, by = 'Locator')
write.csv(DO_Table,'./data_cache/Misc/DissolvedOxygen_Combined.csv')

DOsat_Table <- inner_join(rownames_to_column(DOSatP_slopes, var = 'Locator'),DOsat_LC_inputs, by = 'Locator')
write.csv(DOsat_Table,'./data_cache/Misc/DOSaturation_Combined.csv')


Low_months <- DO_Monthly %>%
  pivot_longer(!Year_mon, values_to = 'DO', names_to = 'Site') %>%
  mutate(Site=ifelse(Site=='0450CC','0450', 
                     ifelse(Site=='0484A','0484', Site))) %>%
  mutate(Month = month(Year_mon), 
         Year = year(Year_mon),
         .before = 1) %>%
  subset(Month %in% c(7:8)) %>%
  subset(Year >= 1979 & Year <= 2022) %>%
  group_by(Site, Year) %>%
  summarise(DO = mean(DO, na.rm = T)) %>%
  mutate(DO = ifelse(is.nan(DO) , NA, DO)) 


ggplot(data = Low_months) +
  facet_wrap(.~ Site) +
  geom_point(aes(x = Year, y= DO)) +
  geom_hline(yintercept = 2, linetype = 'twodash', color = 'grey', linewidth = 1)
  xlab('Year') +
  ylab('Temperature')

# Fitting the warm months to land cover data ################################################

Warm_Annual_DO <- Low_months %>%
  pivot_wider(id_cols = Year, values_from = DO, names_from = Site)
  
  LowDO_lc_mods <- Land_Cover_Modeling(Warm_Annual_DO, CoverVariables, param = "DO", window = c(2016, 2022), log_space = FALSE)
  
  
  
  # Create a data frame of Estimated Henry's Constants, provided by D. Tromans (2000)
  TempK1 <- 10.25 + 273.15
  TempK2 <- 10.946 + 273.15
  diffT <- TempK2 - TempK1
  #
  #
  # Henry's Constant Estimations
  DOSat_1 <- exp((0.046*(TempK1^2) + 203.35*TempK1*log(TempK1/298) - (299.378 + 0.092*TempK1)*(TempK1 - 298) - 20.591*10^3)/(8.3144*TempK1))
  DOSat_2 <- exp((0.046*(TempK2^2) + 203.35*TempK2*log(TempK2/298) - (299.378 + 0.092*TempK2)*(TempK2 - 298) - 20.591*10^3)/(8.3144*TempK2))
  #
  #
  #
  # Convert to DO Saturation
  DOSat_1 <- DOSat_1 * 0.21 * 32000 * 0.99975
  DOSat_2 <- DOSat_2 * 0.21 * 32000 * 0.99975
  Diff <- DOSat_2 - DOSat_1
  #
  #
  #  