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
LandCover <- read_excel("data_cache/streams_2019lulc.xlsx", sheet = "LULC - %")

# This creates a scatterplot matric of all the land cover categories in the NLCD
pairs.panels(LandCover[, c(3:22)], smooth = FALSE, scale = TRUE, lm = TRUE, cex.cor = 3, main = 'Scatterplot Matrix for Landcover Data')
# This plot tells me that the best parameters to test are as follows: 
# Developed, All Intensities
# Developed, Open Space
# Deciduous Forest
# Total Agricultural
# Wetlands, Total
# Open Water


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



mod_inputs <- Fec_Annual %>%
  subset(Year < 2023 & Year > 2015)%>%
  select(- all_of('Year')) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'Locator') %>%
  rowwise(Locator) %>%
  summarise(count = sum(! is.na(c_across(V1:V7))), 
            mean = mean(log(c_across(V1:V7)), na.rm = TRUE)) %>%
  left_join(LandCover, by = 'Locator') %>%
  subset(count >=4)


# Create a scatterplot matrix of all of the landcover categories and the mean nitrate concentrations

n <- nrow(Fec_recent)

mod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AICc' = numeric(),
                       'AICwt' = numeric(), 'Intercept' = numeric(), 'coef_1' = numeric(), 
                       'coef_2' = numeric(), 'coef_3' = numeric())

model_form <- formula

# This for loop will fit multiple linear models to the average NO2/3 data
for (rep in 1:13){
  if (rep == 1){ 
    # 1. Total Developed Area
    N_mod <- glm(mean_NO3 ~ `Urban, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed Area'
  }
  if (rep == 2){ 
    # 2. Developed, High Intensity
    N_mod <- glm(mean_NO3 ~ `Developed, High Intensity`, data = Fec_recent, family = gaussian())
    mod_form <- 'Developed, High Intensity'
  }
  if (rep == 3){ 
    # 3. Developed, Medium Intensity
    N_mod <- glm(mean_NO3 ~ `Developed, Medium Intensity`, data = Fec_recent, family = gaussian())
    mod_form <- 'Developed, Medium Intensity'
  }
  if (rep == 4){ 
    # 4. Total Agricultural Area
    N_mod <- glm(mean_NO3 ~ `Agriculture, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Agricultural Area'
  }
  if (rep == 5){ 
    # 5. Total Forested Area
    N_mod <- glm(mean_NO3 ~ `Forest, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Forested Area'
  }
  if (rep == 6){ 
    # 6. Deciduous Forest
    N_mod <- glm(mean_NO3 ~ `Deciduous Forest`, data = Fec_recent, family = gaussian())
    mod_form <- 'Deciduous Forest'
  }
  if (rep == 7){ 
    # 7. Total Developed + Total Agricultural
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Agriculture, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Agricultural'
  }
  if (rep == 8){ 
    # 8. Total Developed + Deciduous Forest
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Deciduous Forest`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest'
  }
  if (rep == 9){ 
    # 9. Total Developed + Total Wetlands
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Wetlands, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Wetlands'
  }
  if (rep == 10){ 
    # 10. Total Developed + Open Water
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Open Water`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Open Water'
  }
  if (rep == 11){ 
    # 11. Total Developed + Deciduous Forest + Total Agricultural
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Deciduous Forest` + `Agriculture, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Agricultural'
  }
  if (rep == 12){ 
    # 12. Total Developed + Deciduous Forest + Total Wetlands
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'
  }
  if (rep == 13){ 
    # 13. Total Developed + Deciduous Forest + Open Water
    N_mod <- glm(mean_NO3 ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = Fec_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Open Water'
  }
  
  r2 <- rSquared(N_mod$y, N_mod$residuals) 
  k <- length(N_mod$coefficients) - 1
  aicc <- N_mod$aic + (2*k*(k+1))/(n-k-1)
  Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                           AICc = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                           coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3], 
                                           coef_3 = N_mod$coefficients[4])
}

# Add model selection diagnostics
Nmod_results$relLik <- exp(-0.5 * (Nmod_results$AICc - min(Nmod_results$AICc)))
Nmod_results$AICwt <- Nmod_results$relLik/sum(Nmod_results$relLik)
Nmod_results <- Nmod_results %>% 
  arrange(-AICwt) %>%
  column_to_rownames(var = 'Description') %>%
  round(digits = 3) %>%
  rownames_to_column(var = 'Description')
# Export Results to a csv
write.csv(Nmod_results,'./data_cache/NO2-3_LandCover_Models.csv')



