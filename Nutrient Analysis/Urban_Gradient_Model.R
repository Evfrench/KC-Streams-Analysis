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

# Creates frames for all sites, and eliminates all years except for the last 5
NOx_recent <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv') %>%
  subset(Year < 2023 & Year > 2017)

PO4_recent <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv') %>%
  subset(Year < 2023 & Year > 2017)

NOx_select <- sapply(NOx_recent, function(x) sum(! is.na(x)))
PO4_select <- sapply(PO4_recent, function(x) sum(! is.na(x)))

# We will only use the site in this analysis if there are 3 years or more of valid data
# This leaves us with 73 sites, not too bad
for (site in colnames(NOx_recent)){
  if (NOx_select[site] < 3){
    NOx_recent <- NOx_recent %>% select(- all_of(site))
  }
  if (PO4_select[site] <3){
    PO4_recent <- PO4_recent %>% select(- all_of(site))
  }
}

# Export site names as a csv so land cover data can be added ####

#urban_grad <- as.data.frame(colnames(NOx_recent[,-1]))
#colnames(urban_grad) <- c('Locator')
#write.csv(urban_grad, './data_cache/Urban_gradient_sites.csv')

# Import newly modified site information ####

#urban_grad <- fread('~/KC-Streams-Analysis/data_cache/Urban_gradient_sites.csv')

# Create the model data frame then fit the nutrient concentrations to a linear model ########################
# consider adding drainage area as a covariate
model_dat <- tibble(.rows = 73)
model_dat <- urban_grad[,c('Locator','% Developed','% Agricultural')]
model_dat$`NO2/3 (μg/L)` <- sapply(NOx_recent[,-1], function(x) mean(x, na.rm = TRUE))
model_dat$`PO4 (μg/L)` <- sapply(PO4_recent[,-1], function(x) mean(x, na.rm = TRUE))

# remove any sites with more than 5% agricultural cover
model_dat <- model_dat %>% subset(`% Agricultural` <= 5)
colnames(model_dat) <- c('Locator','Dev','Agr','NOx','PO4')

NOx_mod <- glm(NOx ~ Dev, data = model_dat, family = gaussian())
#NOx_mod2 <- gam(NOx ~ Dev, data = model_dat, family = gaussian())
Nx <- seq(0, 100, 1)
Ny <- predict(NOx_mod, list(Dev= Nx), type= 'response')
Nmod <- tibble(Nx,Ny)

ggplot() +
  geom_point(data= model_dat, aes(x= Dev, y= NOx)) +
  geom_line(data= Nmod, aes(x= Nx, y= Ny), size= 2, show.legend = FALSE) +
  ggtitle('NO2/3 recent concentrations and linear model')



PO4_mod <- glm(PO4 ~ Dev, data = model_dat, family = gaussian())
Px <- seq(0, 100, 1)
Py <- predict(PO4_mod, list(Dev= Px), type= 'response')
Pmod <- tibble(Px,Py)

ggplot() +
  geom_point(data= model_dat, aes(x= Dev, y= PO4)) +
  geom_line(data= Pmod, aes(x= Px, y= Py), size= 2, show.legend = FALSE) +
  ggtitle('PO4 recent concentrations and linear model')

# The intercept for NOx md

rSquared(NOx_mod$y, NOx_mod$residuals) 
rSquared(PO4_mod$y, PO4_mod$residuals)

# Attempt Number 2 #####################

# Lets try Nitrite/nitrate first ############

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
  subset(count >=4 & `Agriculture, Total` < 5)


# Fitting various linear models to the nitrate data ############################

Nmod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AIC' = numeric(),
                      'Intercept' = numeric(), 'coef_1' = numeric(), 'coef_2' = numeric(),
                      'coef_3' = numeric())

# 1. Total Developed Area
N_mod <- glm(mean ~ `Urban, Total`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed Area'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 2. Developed, High Intensity
N_mod <- glm(mean ~ `Developed, High Intensity`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Developed, High Intensity'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 3. Developed, Medium Intensity
N_mod <- glm(mean ~ `Developed, Medium Intensity`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Developed, Medium Intensity'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])
# 4. Developed, Low Intensity
N_mod <- glm(mean ~ `Developed, Low Intensity`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Developed, Low Intensity'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 5. Total Forested Area
N_mod <- glm(mean ~ `Forest, Total`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Forested Area'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 6. Deciduous Forest
N_mod <- glm(mean ~ `Deciduous Forest`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Deciduous Forest'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])


# 7. Total Developed + Deciduous Forest
N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 8. Total Developed + Total Wetlands
N_mod <- glm(mean ~ `Urban, Total` + `Wetlands, Total`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed + Total Wetlands'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 9. Total Developed + Open Water
N_mod <- glm(mean ~ `Urban, Total` + `Open Water`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed + Open Water'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3])

# 10. Total Developed + Deciduous Forest + Total Wetlands
N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                       AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                       coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3], 
                                       coef_3 = N_mod$coefficients[4])

# 11. Total Developed + Deciduous Forest + Open Water
N_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = N_recent, family = gaussian())
r2 <- rSquared(N_mod$y, N_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest + Open Water'

Nmod_results <- Nmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = N_mod$aic, Intercept = N_mod$coefficients[1], 
                                         coef_1 = N_mod$coefficients[2], coef_2 = N_mod$coefficients[3], 
                                         coef_3 = N_mod$coefficients[4])

# Export Results to a csv
write.csv(Nmod_results,'./data_cache/NO2-3_LandCover_Models.csv', col.names = TRUE)

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
  subset(count >=4 & `Agriculture, Total` < 5)

Pmod_results <- tibble('Description' = character(), 'R_Squared' = numeric(), 'AIC' = numeric(),
                       'Intercept' = numeric(), 'coef_1' = numeric(), 'coef_2' = numeric(),
                       'coef_3' = numeric())

# 1. Total Developed Area
P_mod <- glm(mean ~ `Urban, Total`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed Area'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 2. Developed, High Intensity
P_mod <- glm(mean ~ `Developed, High Intensity`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Developed, High Intensity'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 3. Developed, Medium Intensity
P_mod <- glm(mean ~ `Developed, Medium Intensity`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Developed, Medium Intensity'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])
# 4. Developed, Low Intensity
P_mod <- glm(mean ~ `Developed, Low Intensity`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Developed, Low Intensity'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 5. Total Forested Area
P_mod <- glm(mean ~ `Forest, Total`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Forested Area'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 6. Total Forested Area + Developed, Medium Intensity
P_mod <- glm(mean ~ `Forest, Total` + `Developed, Medium Intensity`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Forested Area + Developed, Medium Intensity'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])


# 7. Total Developed + Deciduous Forest
P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 8. Total Developed + Evergreen Forest. This will test if the previous model is just benefiting from having forest in the model
P_mod <- glm(mean ~ `Urban, Total` + `Evergreen Forest`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Evergreen Forest'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 9. Total Developed + Open Water
P_mod <- glm(mean ~ `Urban, Total` + `Open Water`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Open Water'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 10. Total Developed + Total Wetlands
P_mod <- glm(mean ~ `Urban, Total` + `Wetlands, Total`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Total Wetlands'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

# 11. Total Developed + Deciduous Forest + Open Water
P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Open Water`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest + Open Water'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3], 
                                         coef_3 = P_mod$coefficients[4])

# 12. Total Developed + Deciduous Forest + Total Wetlands
P_mod <- glm(mean ~ `Urban, Total` + `Deciduous Forest` + `Wetlands, Total`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Deciduous Forest + Total Wetlands'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3], 
                                         coef_3 = P_mod$coefficients[4])

# 8. Total Developed + Mixed Forest. This will test if the previous model is just benefiting from having forest in the model
P_mod <- glm(mean ~ `Urban, Total` + `Mixed Forest`, data = P_recent, family = gaussian())
r2 <- rSquared(P_mod$y, P_mod$residuals) 
mod_form <- 'Total Developed + Mixed Forest'

Pmod_results <- Pmod_results %>% add_row(Description = mod_form, R_Squared = r2[1,1], 
                                         AIC = P_mod$aic, Intercept = P_mod$coefficients[1], 
                                         coef_1 = P_mod$coefficients[2], coef_2 = P_mod$coefficients[3])

write.csv(Pmod_results,'./data_cache/PO4_LandCover_Models.csv', col.names = TRUE)
