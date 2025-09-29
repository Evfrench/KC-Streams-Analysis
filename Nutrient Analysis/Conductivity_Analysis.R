# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# Load the Lab Samples
Cond_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Conductivity.csv') %>% select(- all_of(remove_sites))
Cond_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Combined_Conductivity.csv') %>% select(- all_of(remove_sites)) %>%
  mutate(Year_mon = as.yearmon(Year_mon), .before = 1)

#write_csv(Cond_Monthly, file = './data_cache/Misc/Cond_AllSites.csv')

# Plot the number of entries per year with the fixed code
Cond_Entries <- tibble(as.data.frame(Cond_Annual)['Year'], rowSums(!is.na(Cond_Annual[,-1])))
names(Cond_Entries) <- c('Year', 'Entries')
ggplot(Cond_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Conductivity Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (Note discrepancy in the code)
# Results: x sites, 
# This function will calculate the long term slopes as defined by the function inputs stated above
cond_slopes <- LT_Slope_Dist(Cond_Annual, units = c('umhos/cm'), window = c(1979,2012,2013,2022))
write.csv(cond_slopes[[1]],'./data_cache/LongTermTrends/Conductivity_Slopes.csv')
write.csv(cond_slopes[[2]],'./data_cache/ZscoreTrends/Conductivity_Scores.csv')
write.csv(cond_slopes[[4]],'./data_cache/ZscoreTrends/Conductivity_Values.csv')


if (quantile(cond_slopes[[1]]$`Mean Slope (umhos/cm/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(cond_slopes[[1]]$`Mean Slope (umhos/cm/decade)`, alternative = 'less')
} else {
  results <- wilcox.test(cond_slopes[[1]]$`Mean Slope (umhos/cm/decade)`, alternative = 'greater')
}

# Get the IQR of the distribution and percent change distribution
Cond_quant <- quantile(cond_slopes[[1]]$`Mean Slope (umhos/cm/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Cond_pquant <- quantile(cond_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(cond_slopes[[1]], aes(x = `Mean Slope (umhos/cm/decade)`)) +
  geom_histogram(binwidth = 3) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Cond_quant[2], Cond_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Cond_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Conductivity Slope Distribution') 

ggplot(cond_slopes[[1]], aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Cond_pquant[2], Cond_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Cond_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Conductivity Slope Distribution, Percent Change') 

## Comparing these long term trends as z-scores #######################

cond_slopes[[3]]+
  ggtitle("Conductivity Annual Z-score Distribution") 

# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits all of the models I originally looped through myself automatically
Cond_lc_mods <- Land_Cover_Modeling(Cond_Annual, CoverVariables, param = "Conductivity", window = c(2016, 2022), log_space = FALSE)
cond_LC_results <- Cond_lc_mods[[1]]
cond_LC_inputs <- Cond_lc_mods[[2]]

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
  xlab('% Deciduous Forest') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')

## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.669
#
# rSquared(cond_LC_inputs$mean_Conc, cond_LC_inputs$combined_Resid)

# quantiles
qqnorm(cond_LC_inputs$combined_Resid)
qqline(cond_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(cond_LC_inputs$combined_Pred, cond_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(cond_LC_inputs$mean_Conc, cond_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')


# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Cond_Seasonal <- Seasonal_Analysis(Cond_Monthly, form = 'Relative')

ggplot(Cond_Seasonal, aes(x= Month, y= mean_dev)) +
  geom_boxplot(aes(group= Month), outliers = FALSE) +
  scale_y_log10(limits = c(0.5,2)) +
  ylab('Multiple of Annual Median') +
  xlab(NULL) +
  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Conductivity Monthly Trends") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        title = element_text(size = 15))

Cond_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(Cond_Seasonal, Month == i)$mean_dev, mu =1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(Cond_Seasonal, Month == i)$mean_dev, mu =1, alternative = 'greater')
  }
  
  Cond_Q_Months <- Cond_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(Cond_Seasonal, Month == i)$mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(Cond_Q_Months, file = './data_cache/SeasonalityResults/Conductivity_Monthly_Dist.csv')

# Not merging all data points per site, plot is too messy

#Cond_Seasonal2 <- Seasonal_Analysis(Cond_Monthly, form = 'Relative2')

#ggplot(Cond_Seasonal2, aes(x= Month, y= rel_dev)) +
#  geom_boxplot(aes(group= Month), outliers = FALSE) +
#  scale_y_log10()+ #limits = c(0.5,2)) +
#  ylab('Multiple of Annual Median') +
#  xlab(NULL) +
#  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
#  ggtitle("Conductivity Monthly Trends") +
#  theme(axis.title.x = element_text(size = 15),
#        axis.title.y = element_text(size = 15),
#        axis.text.x = element_text(size = 20),
#        axis.text.y = element_text(size = 15),
#        title = element_text(size = 15))

# Extra Stuff

#Cond_Table <- inner_join(rownames_to_column(cond_slopes, var = 'Locator'),cond_LC_inputs, by = 'Locator')
#write.csv(Cond_Table,'./data_cache/Misc/Conductivity_Combined.csv')



