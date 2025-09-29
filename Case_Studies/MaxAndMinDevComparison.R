# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

# These monitoring sites are redundant and will be removed from any analysis
remove_sites <- c('0305','0307','0308','0309','3106',
                  'C484','A438','F321','S484','A319','B319','0632','A631','S478','D474','KTHA01','KTHA02','0486','BB470') #

# Note: this should also remove major rivers, and possibly highly agricultural watersheds as well
major_rivers <- c('SKYKOMISH','SNQDUVALL','0438','X438','0450','0450CC','0486','0311')

# The object of this particular analysis is to contrast the group of 20% most developed sites with the time series of the 20% LEAST developed sites


# Load in some results first, using the long term trends and the site names and cover variables, should this just be a function? maybe
SiteNames <- fread('./data_cache/LongTermTrends/Phosphate_Slopes.csv')[! V1 %in% major_rivers] %>% 
  mutate(Locator = V1) %>%
  left_join(LandCover, by = 'Locator') %>%
  subset(`Agriculture, Total` <= 10) %>% #This will remove any watershed that is more than 10% total agricultural landcover
  select(all_of(c('Locator','Developed, Total', 'Forest, Total')))
  
Sites80 <- SiteNames %>% 
#  subset(`Developed, Total` >= quantile(SiteNames$`Developed, Total`, probs = 0.8))
  subset(`Developed, Total` >= 90)

Sites20 <- SiteNames %>%
#  subset(`Developed, Total` <= quantile(SiteNames$`Developed, Total`, probs = 0.2))
 subset(`Developed, Total` <= 50)
  

# Now load in some nutrient data, how about the alkalinity time series first
Alk<- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Alkalinity.csv')

Alk_z <- Z_Score_Box_Data(Alk,Sites20,Sites80,'Alkalinity','mgCaCO3/L')

# plot results
Alk_z[[8]] +
  annotate('text', x = Alk_z[[5]]$Year, y = rep(-2, each = length(Alk_z[[5]]$n)), label = Alk_z[[5]]$n, size = rel(3.5), color = 'red') +
  annotate('text', x = min(Alk_z[[5]]$Year)-1, y = -2, label = 'n =', size = rel(3.5)) +
  annotate('text', x = min(Alk_z[[6]]$Year)-1, y = -8, label = 'n =', size = rel(3.5)) +
  annotate('text', x = Alk_z[[6]]$Year, y = rep(-8, each = length(Alk_z[[6]]$n)), label = Alk_z[[6]]$n, size = rel(3.5), color = 'blue')
  
# Conductivity

Cond<- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Conductivity.csv')

Cond_z <- Z_Score_Box_Data(Cond,Sites20,Sites80,'Conductivity','uS/cm')

# plot results
Cond_z[[8]] +
  annotate('text', x = Cond_z[[5]]$Year, y = rep(-6, each = length(Cond_z[[5]]$n)), label = Cond_z[[5]]$n, size = rel(3.5), color = 'red') +
  annotate('text', x = min(Cond_z[[5]]$Year)-1, y = -6, label = 'n =', size = rel(3.5)) +
  annotate('text', x = min(Cond_z[[6]]$Year)-1, y = -16, label = 'n =', size = rel(3.5)) +
  annotate('text', x = Cond_z[[6]]$Year, y = rep(-16, each = length(Cond_z[[6]]$n)), label = Cond_z[[6]]$n, size = rel(3.5), color = 'blue')

# Dissolved Oxygen

DO <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Combined_Dissolved_Oxygen.csv')

DO_z <- Z_Score_Box_Data(DO,Sites20,Sites80,'Dissolved Oxygen','mg/L')

# plot results
DO_z[[8]] +
  annotate('text', x = DO_z[[5]]$Year, y = rep(1, each = length(DO_z[[5]]$n)), label = DO_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(DO_z[[5]]$Year)-1, y = 1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(DO_z[[6]]$Year)-1, y = 0.5, label = 'n =', size = rel(3.0)) +
  annotate('text', x = DO_z[[6]]$Year, y = rep(0.5, each = length(DO_z[[6]]$n)), label = DO_z[[6]]$n, size = rel(3.0), color = 'blue')

# Fecal Coliform

Fec <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Fecal_Coliform.csv')

Fec_z <- Z_Score_Box_Data(Fec,Sites20,Sites80,'Fecal Coliform Bacteria','CFU/100mL')

Fec_z[[8]] +
  annotate('text', x = Fec_z[[5]]$Year, y = rep(-10, each = length(Fec_z[[5]]$n)), label = Fec_z[[5]]$n, size = rel(3.0), color = 'blue') +
  annotate('text', x = min(Fec_z[[5]]$Year)-1, y = -10, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(Fec_z[[6]]$Year)-1, y = -50, label = 'n =', size = rel(3.0)) +
  annotate('text', x = Fec_z[[6]]$Year, y = rep(-50, each = length(Fec_z[[6]]$n)), label = Fec_z[[6]]$n, size = rel(3.0), color = 'red')

# Nitrate

Nit <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')

Nit_z <- Z_Score_Box_Data(Nit,Sites20,Sites80,'Nitrate','ug/L')

Nit_z[[8]] +
  annotate('text', x = Nit_z[[5]]$Year, y = rep(-1, each = length(Nit_z[[5]]$n)), label = Nit_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(Nit_z[[5]]$Year)-1, y = -1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(Nit_z[[6]]$Year)-1, y = -50, label = 'n =', size = rel(3.0)) +
  annotate('text', x = Nit_z[[6]]$Year, y = rep(-50, each = length(Nit_z[[6]]$n)), label = Nit_z[[6]]$n, size = rel(3.0), color = 'blue')


# pH

pH <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_pH,_Field.csv')

pH_z <- Z_Score_Box_Data(pH,Sites20,Sites80,'pH','pH')

pH_z[[8]] +
  scale_y_continuous(limits = c(4,10)) +
  annotate('text', x = pH_z[[5]]$Year, y = rep(4.5, each = length(pH_z[[5]]$n)), label = pH_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(pH_z[[5]]$Year)-1, y = 4.5, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(pH_z[[6]]$Year)-1, y = 4, label = 'n =', size = rel(3.0)) +
  annotate('text', x = pH_z[[6]]$Year, y = rep(4, each = length(pH_z[[6]]$n)), label = pH_z[[6]]$n, size = rel(3.0), color = 'blue')

# Srp

Srp <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Orthophosphate_Phosphorus.csv')

Srp_z <- Z_Score_Box_Data(Srp,Sites20,Sites80,'Phosphate','ug/L')

Srp_z[[8]] +
  annotate('text', x = Srp_z[[5]]$Year, y = rep(-1, each = length(Srp_z[[5]]$n)), label = Srp_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(Srp_z[[5]]$Year)-1, y = -1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(Srp_z[[6]]$Year)-1, y = -4, label = 'n =', size = rel(3.0)) +
  annotate('text', x = Srp_z[[6]]$Year, y = rep(-4, each = length(Srp_z[[6]]$n)), label = Srp_z[[6]]$n, size = rel(3.0), color = 'blue')

# Temperature

Temp <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Temperature.csv')

Temp_z <- Z_Score_Box_Data(Temp,Sites20,Sites80,'Stream Temperature','deg. C')

Temp_z[[8]] +
  annotate('text', x = Temp_z[[5]]$Year, y = rep(2.5, each = length(Temp_z[[5]]$n)), label = Temp_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(Temp_z[[5]]$Year)-1, y = 2.5, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(Temp_z[[6]]$Year)-1, y = 1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = Temp_z[[6]]$Year, y = rep(1, each = length(Temp_z[[6]]$n)), label = Temp_z[[6]]$n, size = rel(3.0), color = 'blue')

# Total Nitrogen

TN <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Nitrogen.csv') 

TN_z <- Z_Score_Box_Data(TN,Sites20,Sites80,'Total Nitrogen','ug/L')

TN_z[[8]] +
  annotate('text', x = TN_z[[5]]$Year, y = rep(-10, each = length(TN_z[[5]]$n)), label = TN_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(TN_z[[5]]$Year)-1, y = -10, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(TN_z[[6]]$Year)-1, y = -50, label = 'n =', size = rel(3.0)) +
  annotate('text', x = TN_z[[6]]$Year, y = rep(-50, each = length(TN_z[[6]]$n)), label = TN_z[[6]]$n, size = rel(3.0), color = 'blue')

# Total Phosphorus

TP <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Phosphorus.csv') 

TP_z <- Z_Score_Box_Data(TP,Sites20,Sites80,'Total Phosphorus','ug/L')

TP_z[[8]] +
  annotate('text', x = TP_z[[5]]$Year, y = rep(-2, each = length(TP_z[[5]]$n)), label = TP_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(TP_z[[5]]$Year)-1, y = -2, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(TP_z[[6]]$Year)-1, y = -10, label = 'n =', size = rel(3.0)) +
  annotate('text', x = TP_z[[6]]$Year, y = rep(-10, each = length(TP_z[[6]]$n)), label = TP_z[[6]]$n, size = rel(3.0), color = 'blue')
# Total Suspended Solids

TSS <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Total_Suspended_Solids.csv')

TSS_z <- Z_Score_Box_Data(TSS,Sites20,Sites80,'Total suspended Solids','mg/L')

TSS_z[[8]] +
  annotate('text', x = TSS_z[[5]]$Year, y = rep(-1, each = length(TSS_z[[5]]$n)), label = TSS_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(TSS_z[[5]]$Year)-1, y = -1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(TSS_z[[6]]$Year)-1, y = -2, label = 'n =', size = rel(3.0)) +
  annotate('text', x = TSS_z[[6]]$Year, y = rep(-2, each = length(TSS_z[[6]]$n)), label = TSS_z[[6]]$n, size = rel(3.0), color = 'blue')

# Turbidity

Turb <- fread ('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Turbidity.csv') 

Turb_z <- Z_Score_Box_Data(Turb,Sites20,Sites80,'Turbidity','NTU')

Turb_z[[8]] +
  annotate('text', x = Turb_z[[5]]$Year, y = rep(-0.5, each = length(Turb_z[[5]]$n)), label = Turb_z[[5]]$n, size = rel(3.0), color = 'red') +
  annotate('text', x = min(Turb_z[[5]]$Year)-1, y = -0.5, label = 'n =', size = rel(3.0)) +
  annotate('text', x = min(Turb_z[[6]]$Year)-1, y = -1, label = 'n =', size = rel(3.0)) +
  annotate('text', x = Turb_z[[6]]$Year, y = rep(-1, each = length(Turb_z[[6]]$n)), label = Turb_z[[6]]$n, size = rel(3.0), color = 'blue')
# Turbidity