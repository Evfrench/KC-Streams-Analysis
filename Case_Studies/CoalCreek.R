# 
# https://pakillo.github.io/R-GIS-tutorial/
# This is the study for Cochran Springs (A499), a rural basin that strongly follows dimension 2 and weakly along 4
# Stream Gage 40D, 1995-Present
source('./functions/get_socrata_data_func.R')
library(qgam)
library(gratia) 

WQ_params <- fread('./data_cache/SourceData/KC_WQ_Data') %>% subset(Locator == '0442')%>%
  rowwise() %>%
  mutate(DO_comb = mean(c_across(c('Dissolved_Oxygen,_Field','Dissolved_Oxygen')), na.rm = T),
         Cond_comb = mean(c_across(c('Conductivity,_Field','Conductivity')), na.rm = T),
         Year_mon = as.yearmon(Year_mon))%>%
  select( all_of(c('Year_mon', "Ammonia_Nitrogen", "Nitrite_+_Nitrate_Nitrogen", "Total_Nitrogen",
                   "Orthophosphate_Phosphorus", "Total_Phosphorus", "E._coli", "Fecal_Coliform", 
                   "pH,_Field", "Total_Alkalinity","Cond_comb", 
                   "Temperature","DO_comb", "Total_Suspended_Solids", "Turbidity"))) %>%
  arrange(Year_mon) %>%
  mutate(year = year(Year_mon),
         month = as_factor(month(Year_mon, label = T)),) %>%
  mutate(t = as.numeric(Year_mon-1975),
         t_y = year-1975,
         Total_Phosphorus = Total_Phosphorus * 1000,
         Orthophosphate_Phosphorus = Orthophosphate_Phosphorus * 1000) %>%
  rename(Nitrate_N = `Nitrite_+_Nitrate_Nitrogen`) %>%
  rename(pH = `pH,_Field`)

#Stream_Flow <- readRDS(file = '~/KC-Streams-Analysis/data_cache/Hydrological/DailyAveFlow_allgages.RDS') %>%
#  drop_na() %>%
#  mutate(Year_mon = as.yearmon(Date)) %>%
#  subset(SITE_CODE == '40d')

# Lets do a multi-quantile GAM  instead, use this: median, IQR, and 1.5 IQR
mqN <- mqgam(log(Total_Suspended_Solids) ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.1,0.25,0.5,0.75,0.9))
mqN$model$Year_mon <- as.yearmon(mqN$model$t + 1975)

ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$`log(Total_Suspended_Solids)` ), col = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')


ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$`log(Total_Suspended_Solids)` ), col = "darkgrey") +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1)+
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.1`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.9`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqN$model$Year_mon, ymin = mqN$fit$`0.25`$fitted.values, ymax = mqN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'TSS (mg/L)') +
  ggtitle(label = 'Coal Creek Springs TSS')

qdo(mqN, qu = 0.5, check)
qdo(mqN, qu = 0.5, summary)

#######################################################################################

mqN <- mqgam(Total_Alkalinity ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.1,0.25,0.5,0.75,0.9))
mqN$model$Year_mon <- as.yearmon(mqN$model$t + 1975)

ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Total_Alkalinity ), col = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')


ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Total_Alkalinity ), col = "darkgrey") +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1)+
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.1`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.9`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqN$model$Year_mon, ymin = mqN$fit$`0.25`$fitted.values, ymax = mqN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Alk (mgCaCO3/L)') +
  ggtitle(label = 'Coal Creek Springs Alkalinity')


############################################################################################

mqN <- mqgam(Cond_comb ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.1,0.25,0.5,0.75,0.9))
mqN$model$Year_mon <- as.yearmon(mqN$model$t + 1975)

ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Total_Alkalinity ), col = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')


ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Cond_comb), col = "darkgrey") +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1)+
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.1`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.9`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqN$model$Year_mon, ymin = mqN$fit$`0.25`$fitted.values, ymax = mqN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Alk (mgCaCO3/L)') +
  ggtitle(label = 'Coal Creek Springs Alkalinity')


############################################################################################

mqN <- mqgam(Orthophosphate_Phosphorus ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.1,0.25,0.5,0.75,0.9))
mqN$model$Year_mon <- as.yearmon(mqN$model$t + 1975)

ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Total_Alkalinity ), col = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')


ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Orthophosphate_Phosphorus), col = "darkgrey") +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1)+
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.1`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.9`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqN$model$Year_mon, ymin = mqN$fit$`0.25`$fitted.values, ymax = mqN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Phosphorus (mu g/L)') +
  ggtitle(label = 'Coal Creek Springs SRP')
