# This Urban gradient model is based on the findings of a 2005 Brett paper, where he fits a model of urban land cover to phosphate concentrations in the stream segments ##########################################
# What counts as urban land cover? It is defined differently in the KC streams report
library(plyr)
library(dplyr)
library(data.table)
library(tibble)
library(mgcv)
library(ggplot2)
library(miscTools)
library(readxl)

# Fitting various linear models to the nitrate data ############################

# Import Nitrate/Nitrite data, the land cover data is from 2019 so that will be the central year, lets say +/- 3 years
# this will give a maximum of 7 years worth of data

N_recent <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv') %>%
  subset(Year < 2023 & Year > 2015)%>%
  select(- all_of('Year')) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'Locator') %>%
  rowwise(Locator) %>%
  summarise(count = sum(! is.na(c_across(V1:V7))), 
            mean = mean(c_across(V1:V7), na.rm = TRUE)) %>%
  left_join(read_excel("data_cache/streams_2019lulc.xlsx", sheet = "LULC - %"), by = 'Locator') %>%
  subset(count >=4)

# For model selection we need to record the number of points
n <- nrow(N_recent)

Nmod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AICc' = numeric(),
                      'AICwt' = numeric(), 'Intercept' = numeric(), 'coef_1' = numeric(), 
                      'coef_2' = numeric(), 'coef_3' = numeric())

# This for loop will fit multiple linear models to the average NO2/3 data
for (rep in 1:13){
  if (rep == 1){ 
    # 1. Total Developed Area
    N_mod <- glm(mean ~ `Urban, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed Area'
    }
  if (rep == 2){ 
    # 2. Developed, High Intensity
    N_mod <- glm(mean ~ `Developed, High Intensity`, data = N_recent, family = gaussian())
    mod_form <- 'Developed, High Intensity'
  }
  if (rep == 3){ 
    # 3. Developed, Medium Intensity
    N_mod <- glm(mean ~ `Developed, Medium Intensity`, data = N_recent, family = gaussian())
    mod_form <- 'Developed, Medium Intensity'
  }
  if (rep == 4){ 
    # 4. Total Agricultural Area
    N_mod <- glm(mean ~ `Agriculture, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Agricultural Area'
  }
  if (rep == 5){ 
    # 5. Total Forested Area
    N_mod <- glm(mean ~ `Forest, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Forested Area'
  }
  if (rep == 6){ 
    # 6. Deciduous Forest
    N_mod <- glm(mean ~ `Deciduous Forest`, data = N_recent, family = gaussian())
    mod_form <- 'Deciduous Forest'
  }
  if (rep == 7){ 
    # 7. Total Developed + Total Agricultural
    N_mod <- glm(mean ~ `Urban, Total` + `Agriculture, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Agricultural'
  }
  if (rep == 8){ 
    # 8. Total Developed + Deciduous Forest
    N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest'
  }
  if (rep == 9){ 
    # 9. Total Developed + Total Wetlands
    N_mod <- glm(mean ~ `Urban, Total` + `Wetlands, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Wetlands'
  }
  if (rep == 10){ 
    # 10. Total Developed + Open Water
    N_mod <- glm(mean ~ `Urban, Total` + `Open Water`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Open Water'
  }
  if (rep == 11){ 
    # 11. Total Developed + Deciduous Forest + Total Agricultural
    N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Agriculture, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Agricultural'
  }
  if (rep == 12){ 
    # 12. Total Developed + Deciduous Forest + Total Wetlands
    N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = N_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'
  }
  if (rep == 13){ 
    # 13. Total Developed + Deciduous Forest + Open Water
    N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = N_recent, family = gaussian())
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


# Fitting various linear models to the phosphate data ############################

P_recent <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv') %>%
  subset(Year < 2023 & Year > 2015)%>%
  select(- all_of('Year')) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'Locator') %>%
  rowwise(Locator) %>%
  summarise(count = sum(! is.na(c_across(V1:V7))), 
            mean = mean(c_across(V1:V7), na.rm = TRUE)) %>%
  left_join(read_excel("data_cache/streams_2019lulc.xlsx", sheet = "LULC - %"), by = 'Locator') %>%
  subset(count >=4)

# For model selection we need to record the number of points
n <- nrow(P_recent)

# Creates a results table for the PO4
Pmod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AICc' = numeric(),
                       'AICwt' = numeric(), 'Intercept' = numeric(), 'coef_1' = numeric(), 
                       'coef_2' = numeric(), 'coef_3' = numeric())

# This for loop will fit multiple linear models to the average PO4 data
for (rep in 1:14){
  if (rep == 1){
    # 1. Total Developed Area
    P_mod <- glm(mean ~ `Urban, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed Area'
  }
  if (rep == 2){
    # 2. Developed, High Intensity
    P_mod <- glm(mean ~ `Developed, High Intensity`, data = P_recent, family = gaussian())
    mod_form <- 'Developed, High Intensity'
  }
  if (rep == 3){
    # 3. Developed, Medium Intensity
    P_mod <- glm(mean ~ `Developed, Medium Intensity`, data = P_recent, family = gaussian())
    mod_form <- 'Developed, Medium Intensity'
    
  }
  if (rep == 4){
    # 4. Total Agricultural Area
    P_mod <- glm(mean ~ `Agriculture, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Agricultural Area'
  }
  if (rep == 5){
    # 5. Total Forested Area
    P_mod <- glm(mean ~ `Forest, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Forested Area'
  }
  if (rep == 6){
    # 6. Total Developed + Total Agricultural
    P_mod <- glm(mean ~ `Urban, Total` + `Agriculture, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Agricultural'
  }
  if (rep == 7){
    # 7. Total Developed + Deciduous Forest
    P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest'
  }
  if (rep == 8){
    # 8. Total Developed + Evergreen Forest. This will test if the previous model is just benefiting from having forest in the model
    P_mod <- glm(mean ~ `Urban, Total` + `Evergreen Forest`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Evergreen Forest'
  }
  if (rep == 9){
    # 9. Total Developed + Open Water
    P_mod <- glm(mean ~ `Urban, Total` + `Open Water`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Open Water'
  }
  if (rep == 10){
    # 10. Total Developed + Total Wetlands
    P_mod <- glm(mean ~ `Urban, Total` + `Wetlands, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Wetlands'
  }
  if (rep == 11){
    # 11. Total Developed + Deciduous Forest + Open Water
    P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Open Water'
  }
  if (rep == 12){
    # 12. Total Developed + Deciduous Forest + Total Wetlands
    P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'
  }
  if (rep == 13){
    # 13. Total Developed + Deciduous Forest + Total Agricultural
    P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Agriculture, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Agricultural'
  }
  if (rep == 14){
    # 14. Total Developed + Open Water + Total Agricultural
    P_mod <- glm(mean ~ `Urban, Total` + `Open Water` + `Agriculture, Total`, data = P_recent, family = gaussian())
    mod_form <- 'Total Developed + Open Water + Total Agricultural'
  }
  r2 <- rSquared(P_mod$y, P_mod$residuals) 
  k <- length(P_mod$coefficients) - 1
  aicc <- P_mod$aic + (2*k*(k+1))/(n-k-1)
  Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                           AICc = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                           coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3], 
                                           coef_3 = P_mod$coefficients[4])
  
}

# Add model selection diagnostics
Pmod_results$relLik <- exp(-0.5 * (Pmod_results$AICc - min(Pmod_results$AICc)))
Pmod_results$AICwt <- Pmod_results$relLik/sum(Pmod_results$relLik)
Pmod_results <- Pmod_results %>% 
  arrange(-AICwt) %>%
  column_to_rownames(var = 'Description') %>%
  round(digits = 3) %>%
  rownames_to_column(var = 'Description')

write.csv(Pmod_results,'./data_cache/PO4_LandCover_Models.csv')

# Fitting various linear models to the fecal data ############################

F_recent <- fread('./data_cache/median_annual_Fecal_Coliform.csv') %>%
  subset(Year < 2023 & Year > 2015)%>%
  select(- all_of('Year')) %>%
  t() %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'Locator') %>%
  rowwise(Locator) %>%
  summarise(count = sum(! is.na(c_across(V1:V7))), 
            mean = mean(c_across(V1:V7), na.rm = TRUE)) %>%
  left_join(read_excel("data_cache/streams_2019lulc.xlsx", sheet = "LULC - %"), by = 'Locator') %>%
  subset(count >=4)

# For model selection we need to record the number of points
n <- nrow(F_recent)

# Creates a results table for the FO4
Fmod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AICc' = numeric(),
                       'AICwt' = numeric(), 'Intercept' = numeric(), 'coef_1' = numeric(), 
                       'coef_2' = numeric(), 'coef_3' = numeric())

# This for loop will fit multiple linear models to the average FO4 data
for (rep in 1:14){
  if (rep == 1){
    # 1. Total Developed Area
    F_mod <- glm(mean ~ `Urban, Total`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed Area'
  }
  if (rep == 2){
    # 2. Developed, High Intensity
    F_mod <- glm(mean ~ `Developed, High Intensity`, data = F_recent, family = gaussian())
    mod_form <- 'Developed, High Intensity'
  }
  if (rep == 3){
    # 3. Developed, Medium Intensity
    F_mod <- glm(mean ~ `Developed, Medium Intensity`, data = F_recent, family = gaussian())
    mod_form <- 'Developed, Medium Intensity'
    
  }
  if (rep == 4){
    # 4. Total Agricultural Area
    F_mod <- glm(mean ~ `Agriculture, Total`, data = F_recent, family = gaussian())
    mod_form <- 'Total Agricultural Area'
  }
  if (rep == 5){
    # 5. Total Forested Area
    F_mod <- glm(mean ~ `Forest, Total`, data = F_recent, family = gaussian())
    mod_form <- 'Total Forested Area'
  }
  if (rep == 6){
    # 6. Total Developed + Total Agricultural
    F_mod <- glm(mean ~ `Forest, Total` + `Developed, Medium Intensity`, data = F_recent, family = gaussian())
    mod_form <- 'Total Forested + Developed, Medium Intensity'
  }
  if (rep == 7){
    # 7. Total Developed + Deciduous Forest
    F_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest'
  }
  if (rep == 8){
    # 8. Total Developed + Evergreen Forest. This will test if the previous model is just benefiting from having forest in the model
    F_mod <- glm(mean ~ `Urban, Total` + `Evergreen Forest`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Evergreen Forest'
  }
  if (rep == 9){
    # 9. Total Developed + Open Water
    F_mod <- glm(mean ~ `Urban, Total` + `Open Water`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Open Water'
  }
  if (rep == 10){
    # 10. Total Developed + Total Wetlands
    F_mod <- glm(mean ~ `Urban, Total` + `Wetlands, Total`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Wetlands'
  }
  if (rep == 11){
    # 11. Total Developed + Deciduous Forest + Open Water
    F_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Open Water'
  }
  if (rep == 12){
    # 12. Total Developed + Deciduous Forest + Total Wetlands
    F_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'
  }
  if (rep == 13){
    # 13. Total Developed + Deciduous Forest + Total Agricultural
    F_mod <- glm(mean ~ `Urban, Total` + `Agriculture, Total` + `Deciduous Forest`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Agricultural + Deciduous Forest'
  }
  if (rep == 14){
    # 14. Total Developed + Open Water + Total Agricultural
    F_mod <- glm(mean ~ `Urban, Total` + `Agriculture, Total` + `Open Water`, data = F_recent, family = gaussian())
    mod_form <- 'Total Developed + Total Agricultural + Open Water'
  }
  r2 <- rSquared(F_mod$y, F_mod$residuals) 
  k <- length(F_mod$coefficients) - 1
  aicc <- F_mod$aic + (2*k*(k+1))/(n-k-1)
  Fmod_results <- Fmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                           AICc = aicc, Intercept = F_mod$coefficients[1], 
                                           coef_1 = F_mod$coefficients[2], coef_2 = F_mod$coefficients[3], 
                                           coef_3 = F_mod$coefficients[4])
  
}

# Add model selection diagnostics
Fmod_results$relLik <- exp(-0.5 * (Fmod_results$AICc - min(Fmod_results$AICc)))
Fmod_results$AICwt <- Fmod_results$relLik/sum(Fmod_results$relLik)
Fmod_results <- Fmod_results %>% 
  arrange(-AICwt) %>%
  column_to_rownames(var = 'Description') %>%
  round(digits = 3) %>%
  rownames_to_column(var = 'Description')

write.csv(Fmod_results,'./data_cache/FecalColi_LandCover_Models.csv')

