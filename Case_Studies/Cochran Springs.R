# https://pakillo.github.io/R-GIS-tutorial/
# This is the study for Cochran Springs (A499), a rural basin that strongly follows dimension 2 and weakly along 4
# Stream Gage 40D, 1995-Present
source('./functions/get_socrata_data_func.R')
library(qgam)
library(gratia)

test1 <- get_socrata_data_func(locns = 'A499', parms = default_data_parms,
                               SiteType = 'Streams and Rivers') %>%
  mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen')) %>%
  mutate(Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
         Censored = if_else(Value <= MDL, TRUE, FALSE, missing = FALSE))
%>%
  group_by(Parameter, Year, Month) %>% # if you want to left censor the data you probably shouldn't do this bit here
  summarise(Reading = mean(Value, na.rm = T),
            MDL = mean(MDL, na.rm = T),
            RDL = mean(RDL, na.rm = T))

WQ_params <- fread('./data_cache/SourceData/KC_WQ_Data') %>% subset(Locator == 'A499')%>%
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

plot(WQ_params$Year_mon, WQ_params$`Nitrite_+_Nitrate_Nitrogen`)

plot(WQ_params$Year_mon, log(WQ_params$Total_Suspended_Solids))
 # Development, Change in Land Cover Etc

site_dev <- readRDS('~/KC-Streams-Analysis/data_cache/SourceData/watershed_build_years.RDS') %>%
  subset(Locator == 'A499') %>%
  reshape2::melt(na.rm = T, id.vars = 'YRBUILT', measure.vars = c('ParcelsBuilt','ParcelsBuiltPer100Acres','ParcelsBuilt_Roll_Ave','ParcelsBuiltPer100Acres_Roll_Ave'), variable.name = 'Param')

ggplot(data = site_dev %>% subset(Param == 'ParcelsBuilt')) +
  geom_line(aes(x= YRBUILT, y= value))

ggplot(data = site_dev %>% subset(Param == 'ParcelsBuilt_Roll_Ave')) +
  geom_line(aes(x= YRBUILT, y= value))

# check the distributions #######################################################################

ggplot(data = WQ_params) +
  geom_density(aes(x= `Nitrate_N`))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Ammonia_Nitrogen`))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Fecal_Coliform`))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Orthophosphate_Phosphorus`))

ggplot(data = WQ_params) +
  geom_density(aes(x= pH))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Temperature`))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Total_Alkalinity`))

ggplot(data = WQ_params) +
  geom_density(aes(x= `Cond_comb`))
# check out some gaussian GAMs

smoothN <- gam(formula= Nitrate_N ~ s(year, k = 10) + s(month, k = 6, bs= 'cc') + ti(year, month, bs= c('tp','cc'), k= 5),  
               data = WQ_params, method = 'REML', family = gaussian())

smoothN2 <- gam(formula= Nitrate_N ~ s(t, k= 15, bs= 'cr'),  
                data = WQ_params, family = gaussian(), method = 'REML')

smoothN3 <- gam(formula= Nitrate_N ~ s(t, k= 8, by = month),  
                data = WQ_params, 
                method = 'REML', 
                family = gaussian())

draw(smoothN2)
qdo(mqN,qu=0.5, draw)
# Lets do a multi-quantile GAM  instead, use this: median, IQR, and 1.5 IQR
mqN <- mqgam(Nitrate_N ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqN$model$Year_mon <- as.yearmon(mqN$model$t + 1975)

ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Nitrate_N), col = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')


ggplot() +
  geom_point(aes(x = mqN$model$Year_mon, y = mqN$model$Nitrate_N), col = "darkgrey") +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqN$model$Year_mon, y = mqN$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqN$model$Year_mon, ymin = mqN$fit$`0.25`$fitted.values, ymax = mqN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(limits = c(0,3), name = 'Nitrate (mg/L)') +
  ggtitle(label = 'Cochran Springs Nitrate Concentration')

qdo(mqN, qu = 0.5, check)
qdo(mqN, qu = 0.5, summary)

# lets do a qgam for Total Nitrogen

mqTN <- mqgam(Total_Nitrogen ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqTN$model$Year_mon <- as.yearmon(mqTN$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqTN$model$Year_mon, y = mqTN$model$Total_Nitrogen), col = "darkgrey") +
  geom_line(aes(x = mqTN$model$Year_mon, y = mqTN$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqTN$model$Year_mon, y = mqTN$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqTN$model$Year_mon, y = mqTN$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqTN$model$Year_mon, ymin = mqTN$fit$`0.25`$fitted.values, ymax = mqTN$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
                     name = 'Total Nitrogen (mg/L)') +
  ggtitle(label = 'Cochran Springs Total Nitrogen Concentration')

qdo(mqTN, qu = 0.5, check)
qdo(mqTN, qu = 0.5, summary)

# now conductivity, this had the top positive trend for conductivity

mqCnd <- mqgam(Cond_comb ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqCnd$model$Year_mon <- as.yearmon(mqCnd$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqCnd$model$Year_mon, y = mqCnd$model$Cond_comb), col = "darkgrey") +
  geom_line(aes(x = mqCnd$model$Year_mon, y = mqCnd$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqCnd$model$Year_mon, y = mqCnd$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqCnd$model$Year_mon, y = mqCnd$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqCnd$model$Year_mon, ymin = mqCnd$fit$`0.25`$fitted.values, ymax = mqCnd$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
                     name = 'Conductivity (umhos/cm)') +
  ggtitle(label = 'Cochran Springs Conductivity')

qdo(mqCnd, qu = 0.5, check)
qdo(mqCnd, qu = 0.5, summary)

# Total Phosphorus

mqTP <- mqgam(Total_Phosphorus ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqTP$model$Year_mon <- as.yearmon(mqTP$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqTP$model$Year_mon, y = mqTP$model$Total_Phosphorus), col = "darkgrey") +
  geom_line(aes(x = mqTP$model$Year_mon, y = mqTP$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqTP$model$Year_mon, y = mqTP$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqTP$model$Year_mon, y = mqTP$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqTP$model$Year_mon, ymin = mqTP$fit$`0.25`$fitted.values, ymax = mqTP$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Phosphorus (μg/L)') +
  ggtitle(label = 'Cochran Springs Total Phosphorus')

qdo(mqTP, qu = 0.5, check)
qdo(mqTP, qu = 0.5, summary)

# Phosphate

mqSRP <- mqgam(Orthophosphate_Phosphorus ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqSRP$model$Year_mon <- as.yearmon(mqSRP$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqSRP$model$Year_mon, y = mqSRP$model$Orthophosphate_Phosphorus), col = "darkgrey") +
  geom_line(aes(x = mqSRP$model$Year_mon, y = mqSRP$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqSRP$model$Year_mon, y = mqSRP$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqSRP$model$Year_mon, y = mqSRP$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqSRP$model$Year_mon, ymin = mqSRP$fit$`0.25`$fitted.values, ymax = mqSRP$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Soluble Reactive Phosphorus (μg/L)') +
  ggtitle(label = 'Cochran Springs Soluble Reactive Phosphorus')

qdo(mqSRP, qu = 0.5, check)
qdo(mqSRP, qu = 0.5, summary)

# Fecal Coliform

mqFC <- mqgam(log(Fecal_Coliform) ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqFC$model$Year_mon <- as.yearmon(mqFC$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqFC$model$Year_mon, y = mqFC$model$`log(Fecal_Coliform)`), col = "darkgrey") +
  geom_line(aes(x = mqFC$model$Year_mon, y = mqFC$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqFC$model$Year_mon, y = mqFC$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqFC$model$Year_mon, y = mqFC$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqFC$model$Year_mon, ymin = mqFC$fit$`0.25`$fitted.values, ymax = mqFC$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'Fecal Coliform (CFU/100mL)') +
  ggtitle(label = 'Cochran Springs Fecal Coliform')

qdo(mqFC, qu = 0.5, check)
qdo(mqFC, qu = 0.5, summary)

# pH

mqpH <- mqgam(pH ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqpH$model$Year_mon <- as.yearmon(mqpH$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqpH$model$Year_mon, y = mqpH$model$pH), col = "darkgrey") +
  geom_line(aes(x = mqpH$model$Year_mon, y = mqpH$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqpH$model$Year_mon, y = mqpH$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqpH$model$Year_mon, y = mqpH$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqpH$model$Year_mon, ymin = mqpH$fit$`0.25`$fitted.values, ymax = mqpH$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = 'pH') +
  ggtitle(label = 'Cochran Springs pH')

qdo(mqpH, qu = 0.5, check)
qdo(mqpH, qu = 0.5, summary)

# Total Alkalinity

mqAlk <- mqgam(Total_Alkalinity ~ s(t, k = 10, bs = 'ad'), data = WQ_params, qu = c(0.12,0.25,0.5,0.75,0.88))
mqAlk$model$Year_mon <- as.yearmon(mqAlk$model$t + 1975)


ggplot() +
  geom_point(aes(x = mqAlk$model$Year_mon, y = mqAlk$model$Total_Alkalinity), col = "darkgrey") +
  geom_line(aes(x = mqAlk$model$Year_mon, y = mqAlk$fit$`0.5`$fitted.values),
            col = "black", lwd = 1) +
  geom_line(aes(x = mqAlk$model$Year_mon, y = mqAlk$fit$`0.12`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_line(aes(x = mqAlk$model$Year_mon, y = mqAlk$fit$`0.88`$fitted.values),
            col = "black", lwd = 0.5) +
  geom_ribbon(aes(x = mqAlk$model$Year_mon, ymin = mqAlk$fit$`0.25`$fitted.values, ymax = mqAlk$fit$`0.75`$fitted.values,
                  y = NULL),
              alpha = 0.2, fill = "black") +
  scale_y_continuous(#limits = c(0,3), 
    name = '(mg CaCO3/L)') +
  ggtitle(label = 'Cochran Springs Alkalinity ')

qdo(mqAlk, qu = 0.5, check)
qdo(mqAlk, qu = 0.5, summary)
