# https://pakillo.github.io/R-GIS-tutorial/
# This is the study for Newaukum Creek (0322)
# Stream Gage 12108500
source('./functions/get_socrata_data_func.R')
library(qgam)
library(gratia)



WQ_Params <- get_socrata_data_func(locns = '0322', parms = default_data_parms,
                               SiteType = 'Streams and Rivers') %>%
  mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),
         Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
         Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                 "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L'),
         Censored = if_else(Value <= MDL, TRUE, FALSE, missing = FALSE),
         Date = date(CollectDate)) %>%
  rowwise() %>%
  mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total_Nitrogen",
                                                         "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000))


Stream_Flow <- readRDS(file = '~/KC-Streams-Analysis/data_cache/Hydrological/DailyAveFlow_allgages.RDS') %>%
  drop_na() %>%
  subset(SITE_CODE == '12108500') %>%
   mutate(Date = date(Date))

Nitrate <- full_join(Stream_Flow, subset(WQ_Params, Parameter == "Nitrite + Nitrate Nitrogen"), by = "Date", multiple = "all") %>%
  subset(Date < dmy("01-01-2023") & Date > dmy("31-12-1977")) %>%
  mutate(Dec_Date = decimal_date(Date) - min(decimal_date(Date)),
         DOY = yday(Date),
         LogQ = log(AveQ)) %>%
  drop_na(matches('AveQ'))

# Let's make something similar to the WRTDS model Hirsch used
# Uses sampling date, discharge, and day of year 
# There are interactions between all of these, how are those accounted for? a single tensor? multiple tensors?
# Maybe add a tensor interaction between DOY and time sampled? Change the long term component to just the year?
# Add a temp component, but normalized to the deviation from the median of that.
Nq <- qgam(Value ~ s(Dec_Date, k = 25, bs = 'ad') + s(LogQ, k = 20, bs = 'ad') + s(DOY, k = 20, bs ='cp'), data = Nitrate, qu = 0.5)

Ngam <- gam(Value ~ s(Year.x, k = 40, bs = 'cr') + 
              s(LogQ, k = 25, bs = 'cr') + 
              s(DOY, k = 20, bs ='cc') + 
              ti(Year.x, LogQ, DOY, k = c(10,10,10), bs = c('cr', 'cr', 'cc')), 
            data = Nitrate, family = gaussian)
  
mqN <- mqgam(Value ~ s(Dec_Date, k = 10, bs = 'ad') + s(AveQ, k = 15, bs ='ad') + s(DOY, k = 15, bs ='cp'), data = Nitrate, qu = 0.5)

check(Nq)
summary(Nq)
plot.gam(Nq)
appraise(Nq)


gam.check(Ngam)
summary(Ngam)
draw(Ngam)
plot.gam(Ngam)
# Next Step is to predict the values

Nitrate <- Nitrate %>%
  subset(Date > dmy('01-01-2015') & Date < dmy('31-12-2020'))

preds<- predict.gam(Nq, Nitrate, type = 'response', se.fit = TRUE)

Nitrate <- Nitrate %>%
  mutate(Pred = preds$fit,
         CI_upper = preds$fit + preds$se.fit * 1.96,
         CI_lower = preds$fit - preds$se.fit * 1.96)



ggplot(data = Nitrate) +
  geom_point(aes(x = Date, y = Value)) +
  geom_line (aes(x = Date, y = Pred)) +
  geom_ribbon(aes(x = Date, ymin = CI_lower, ymax = CI_upper,
                  y = NULL),
              alpha = 0.2, fill = "black")

ggplot(data = Nitrate) +
  geom_point(aes(x = LogQ, y = Value))+
  geom_line (aes(x = LogQ, y = Pred)) +
  geom_ribbon(aes(x = LogQ, ymin = CI_lower, ymax = CI_upper,
                  y = NULL),
              alpha = 0.2, fill = "black")

?data_slice()
fitted_values(Nq)


# To do:
# Prediction of average concentration
# what is going on this the tensor interactions between discharge and day of year

## Generalized function test
source('./functions/get_socrata_data_func.R')

mod_list <- QuantileGamRun(SiteCode = c('A319'), Params = c('Nitrite + Nitrate Nitrogen'))

# what about predicting one year of concentrations? Is is a jack-knife cross-validation to exclude an entire year?


Print(ggplot(data = dat) +
       geom_point(aes(x = Date, y = Value), col = "darkgrey") +
       geom_line(aes(x = Date, y = q50),
                 col = "black", lwd = 1) +
       geom_line(aes(x = Date, y = q25),
                 col = "black", lwd = 0.5) +
       geom_line(aes(x = Date, y = q75),
                 col = "black", lwd = 0.5) +
       geom_line(aes(x = Date, y = q10),
                 col = "black", lwd = 0.5, linetype = 'twodash') +
       geom_line(aes(x = Date, y = q90),
                 col = "black", lwd = 0.5, linetype = 'twodash') +
       scale_y_continuous(#limits = c(0,3), 
         name = unique(dat$Units)[1]) +
       ggtitle(label = title))


# Applying this seasonal model to the Salish Sea Model. Let's see how we can use GAMs for this model
# Get Mike's info for USGS wet and dry years 
# WE could predict concentrations for riverine inputs to the SSM. Would give quicker access to boundary conditions? If it is taken up, there is always a reason why people do things. WHat would we lose by switching to this?
