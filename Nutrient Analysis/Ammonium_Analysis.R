# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106', 
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# If already run once, these will load the frames from the data cache
Am_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Ammonia_Nitrogen.csv') %>% select(- all_of(remove_sites))
Am_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Ammonia_Nitrogen.csv') %>% select(- all_of(remove_sites))
Am_Monthly$Year_mon <- as.yearmon(Am_Monthly$Year_mon)

# Plot the number of entries per year with the fixed code
Am_Entries <- tibble(as.data.frame(Am_Annual)['Year'], rowSums(!is.na(Am_Annual[,-1])))
names(Am_Entries) <- c('Year', 'Entries')
ggplot(Am_Entries, aes(x = Year, y = Entries)) +
  geom_col() +
  ggtitle('Ammonia Entries per Year')

# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2020, 5 yrs required (
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
Ammonia_slopes <- LT_Slope_Dist(Am_Annual, units = c('μg/L'))
write.csv(Ammonia_slopes[[1]],'./data_cache/LongTermTrends/Ammonia_Slopes.csv')
write.csv(Ammonia_slopes[[2]],'./data_cache/ZscoreTrends/Ammonia_Scores.csv')
write.csv(Ammonia_slopes[[4]],'./data_cache/ZscoreTrends/Ammonia_Values.csv')


# Get the IQR of the distribution and percent change distribution
Am_quant <- quantile(Ammonia_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
Am_pquant <- quantile(Ammonia_slopes[[1]]$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

if (quantile(Ammonia_slopes[[1]]$`Mean Slope (μg/L/decade)`, probs = c(0.5)) < 0) {
  results <- wilcox.test(Ammonia_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'less', exact = FALSE)
} else {
  results <- wilcox.test(Ammonia_slopes[[1]]$`Mean Slope (μg/L/decade)`, alternative = 'greater')
}


# Make histograms of the resulting distributions
ggplot(Ammonia_slopes[[1]], aes(x = `Mean Slope (μg/L/decade)`)) +
  geom_histogram(binwidth = 3) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Am_quant[2], Am_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Am_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Ammonia Slope Distribution') 

ggplot(Ammonia_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(binwidth = 5) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(Am_pquant[2], Am_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = Am_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Ammonia Slope Distribution, Percent Change') 


Ammonia_slopes[[3]]+
  ggtitle("Ammonium Annual Z-score Distribution")





# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Fits all of the models I originally looped through myself automatically
Am_lc_mods <- Land_Cover_Modeling(Am_Annual, CoverVariables, param = "Ammonia", window = c(2016, 2022), log_space = FALSE)
Ammonia_LC_results <- Am_lc_mods[[1]]
Ammonia_LC_inputs <- Am_lc_mods[[2]]
# Saves the results table in a CSV
write.csv(Am_lc_mods[[1]],'./data_cache/LandCover/Ammonia_LandCover_Models.csv')

#pairs.panels(Ammonia_LC_inputs[, c(6:10)], smooth = FALSE, scale = FALSE, lm = FALSE, cex.cor = 0.5, main = 'Scatterplot Matrix for Landcover Data')

## Residual analysis ################################
# residual by predicted
# residual by quantiles
# residual by exogenous variables (studentized?)
# Cook's D values

# I looked through the results table and picked the "best" model
top_model <- Am_lc_mods[["Ammonia = Const. + Developed, All Intensities + Deciduous Forest + Open Water"]]

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

# Open Water
ggplot() +
  geom_point(aes(top_model$model$f , top_model$residuals)) +
  xlab('% Open Water') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ylab('residuals')


## Weighted Composite Model Residuals ###########################################################
#
# R-squared = 0.318
#
# rSquared(Ammonia_LC_inputs$mean_Conc, Ammonia_LC_inputs$combined_Resid)

# quantiles
qqnorm(Ammonia_LC_inputs$combined_Resid)
qqline(Ammonia_LC_inputs$combined_Resid)

# By Predicted Values
ggplot() +
  geom_point(aes(Ammonia_LC_inputs$combined_Pred, Ammonia_LC_inputs$combined_Resid)) +
  xlab('Predicted Value') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Predicted Values vs Residuals')

# By Response Variables
ggplot() +
  geom_point(aes(Ammonia_LC_inputs$mean_Conc, Ammonia_LC_inputs$combined_Resid)) +
  xlab('Response Variables') +
  ylab('Residuals') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black', linewidth = 1) +
  ggtitle('Response Variables vs Residuals')

# Examining Seasonality ##################################################################################
# This will use monthly data to do a seasonality analysis, I don't think this needs a function of its own

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened

Am_Seasonal <- Seasonal_Analysis(Am_Monthly, form = 'Relative')

ggplot(Am_Seasonal, aes(x= Month, y= geo_mean_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-100, 250), n.breaks = 10) +
  scale_y_log10(limits = c(0.3,3)) +
  ylab('Deviation from Median') +
  geom_hline(yintercept = 1, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Ammonia Monthly Deviations from Annual Median")

Am_Q_Months <- tibble('Month' = numeric(), '10th' = numeric(), '25th' = numeric(), '50th' = numeric(), '75th' = numeric(), '90th' = numeric(), 'p-val' = numeric())

for (i in 1:12) {
  if (quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.5) < 1) {
    test <- wilcox.test(subset(Am_Seasonal, Month == i)$geo_mean_dev, mu = 1, alternative = 'less')
  } else {
    test <- wilcox.test(subset(Am_Seasonal, Month == i)$geo_mean_dev, mu = 1, alternative = 'greater')
  }
  
  Am_Q_Months <- Am_Q_Months %>% add_row(Month = i, `10th` = quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.1),
                                           `25th` = quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.25),
                                           `50th` = quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.5),
                                           `75th` = quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.75),
                                           `90th` = quantile(subset(Am_Seasonal, Month == i)$geo_mean_dev, probs = 0.9),
                                           `p-val` = test$p.value)
  remove(test)
}

write.csv(Am_Q_Months, file = './data_cache/SeasonalityResults/Ammonia_Monthly_Dist.csv')

# Making a matrix of normalized concentrations to match Mike's work ################

Am_Month <- Am_Monthly %>%
  reshape2::melt(id.vars = 'Year_mon') %>%
  mutate(Year = year(Year_mon), 
         Month = month(Year_mon, label = T, abbr = F),
         .before = 1) %>%
  subset(Year_mon >= as.yearmon('Oct 1977')) %>%
  group_by(variable) %>%
  rowwise() %>%
  mutate(Wtr_Year = replace(Year, Month %in% c('October', 'November', 'December'), Year+1))  %>%
  select(- all_of('Year'))

write.csv(Am_Month, file = '~/KC-Streams-Analysis/data_cache/Misc/NO3_AllSites.csv')

# Unused Code
#   group_by(Year, variable) %>%
#   mutate(num = sum(!is.na(value))) %>% 
#   subset(num == 12) %>%

# Extra Stuff

NO3_Table <- inner_join(rownames_to_column(Ammonia_slopes, var = 'Locator'),Ammonia_LC_inputs, by = 'Locator') %>%
  subset(Locator %notin% dupes)
write.csv(NO3_Table,'./data_cache/Misc/Ammonia_Combined.csv')

library(factoextra)

LCs <- LandCover[,- 2] %>%
  column_to_rownames(var = "Locator")

Decomp <- prcomp(LCs, scale. = TRUE)

fviz_eig(Decomp)

fviz_pca_ind(Decomp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(Decomp, axes = c(3,4),
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(Decomp,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(Decomp, axes = c(3,4),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(Decomp, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

fviz_pca_biplot(Decomp, repel = TRUE, axes = c(3,4),
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

biplot(Decomp, choices = c(1,3))

biplot(Decomp, choices = c(2,3))

