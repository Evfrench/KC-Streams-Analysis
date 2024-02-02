# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0309','3106')

# Create the required data frames
#Fec_Annual <- summarize_WQ_data('Fecal_Coliform','annual') %>% select(- all_of(remove_sites))
#Fec_Monthly <- summarize_WQ_data('Fecal_Coliform','monthly') %>% select(- all_of(remove_sites))

# If already run once, these will load the frames from the data cache
Fec_Annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Fecal_Coliform.csv') %>% select(- all_of(remove_sites))
Fec_Monthly <- fread('~/KC-Streams-Analysis/data_cache/mean_monthly_Fecal_Coliform.csv') %>% select(- all_of(remove_sites))


# Long Term Trend Analysis ##################################################################
#
# Baseline: 1979 - 2008, 5 yrs required
# Current: 2013 - 2022, 5 yrs required
# Results: x sites, 

# This function will calculate the long term slopes as defined by the function inputs stated above
fecal_slopes <- LT_Slope_Dist(Fec_Annual, window = c(1979,2008,2013,2022), cutoff = c(5,5), units = c('CFU/100mL'))

# Get the IQR of the distribution and percent change distribution
fec_quant <- quantile(fecal_slopes$`Median Slope (CFU/100mL/decade)`, probs = c(0.1,0.25,0.5,0.75,0.9))
fec_pquant <- quantile(fecal_slopes$`% Change Per Decade`, probs = c(0.1,0.25,0.5,0.75,0.9))

# Make histograms of the resulting distributions
ggplot(fecal_slopes, aes(x = `Median Slope (CFU/100mL/decade)`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(fec_quant[2], fec_quant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = fec_quant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Fecal Coliform Slope Distribution') 

ggplot(fecal_slopes, aes(x = `% Change Per Decade`)) +
  geom_histogram(bins = 12) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(fec_pquant[2], fec_pquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = fec_pquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Fecal Coliform Slope Distribution, Percent Change') 


# Land Cover Analysis and Modeling ######################################################################
# 2016 - 2022

# Look at log space time series and the normal space to be sure you are making a good decision

# Fits allll of the models I originally looped through myself automatically
fec_lc_mods <- Land_Cover_Modeling(Fec_Annual, CoverVariables, param = "Fecal_Coliform", window = c(2016, 2022), log_space = TRUE)

# Saves the results table in a CSV
write.csv(fec_lc_mods[[1]],'./data_cache/Fec_Coli_LandCover_Models.csv')

# Residual analysis
