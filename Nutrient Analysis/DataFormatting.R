#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#
# Create the data frames that will be used for nutrient analysis, both annual and monthly
#
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
source('./functions/get_socrata_data_func.R')

# Nitrate #################################################################################################################

summarize_WQ_data('Nitrite_+_Nitrate_Nitrogen','annual')
summarize_WQ_data('Nitrite_+_Nitrate_Nitrogen','monthly')

# Total Nitrogen ##########################################################################################################

summarize_WQ_data('Total_Nitrogen','annual')
summarize_WQ_data('Total_Nitrogen','monthly')

# Phosphate/SRP #######################################################################################################

summarize_WQ_data('Orthophosphate_Phosphorus','annual')
summarize_WQ_data('Orthophosphate_Phosphorus','monthly')

# Total Phosphorus #########################################################################################################

summarize_WQ_data('Total_Phosphorus','annual')
summarize_WQ_data('Total_Phosphorus','monthly')

# Fecal Coliform #############################################################################################################

summarize_WQ_data('Fecal_Coliform','annual')
summarize_WQ_data('Fecal_Coliform','monthly')

summarize_WQ_data('Fecal_Coliform_log','annual')
summarize_WQ_data('Fecal_Coliform_log','monthly')

# E.Coli #####################################################################################################################

summarize_WQ_data('E._coli','annual')
summarize_WQ_data('E._coli','monthly')

summarize_WQ_data('E._coli_log','annual')
summarize_WQ_data('E._coli_log','monthly')

# Total Suspended Solids #################################################################################################

summarize_WQ_data('Total_Suspended_Solids','annual')
summarize_WQ_data('Total_Suspended_Solids','monthly')

# Turbidity ###################################################################################################################

summarize_WQ_data('Turbidity','annual')
summarize_WQ_data('Turbidity','monthly')

# Total Alkalinity #########################################################################################################

summarize_WQ_data('Total_Alkalinity','annual')
summarize_WQ_data('Total_Alkalinity','monthly')

# Conductivity ###############################################################################################################

summarize_WQ_data('Conductivity','annual')
summarize_WQ_data('Conductivity','monthly')

# Conductivity, Field ########################################################################################################

summarize_WQ_data('Conductivity,_Field','annual')
summarize_WQ_data('Conductivity,_Field','monthly')

# Conductivity, Combined ####################################################################################################

# Load the Lab Samples
Condl_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Conductivity.csv')
Condl_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Conductivity.csv') 
Condl_Monthly$Year_mon <- as.yearmon(Condl_Monthly$Year_mon)

# Load the Field Samples
Condf_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Conductivity,_Field.csv')
Condf_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Conductivity,_Field.csv')
Condf_Monthly$Year_mon <- as.yearmon(Condf_Monthly$Year_mon)

# Merge the samples
sites = names(Condl_Annual)[-1] # Gathers list of site names

Cond_Annual <- Condl_Annual[,1] # Beginning list with all the available years, luckily both frames have the same number of years
Cond_Monthly <- Condl_Monthly[,1]

# This loop will merge the time series for each site, then take the mean of the two sets
# It will then put that resulting set on the end of the results data frame
for (site in sites){
  # Merging Annual Data
  merged_frame1 <- full_join(Condl_Annual[,c('Year',..site)], Condf_Annual[,c('Year',..site)], by = "Year") %>%
    rowwise(Year) %>%
    summarise(means = mean(c_across(1:2), na.rm = TRUE), .groups = 'drop')
  merged_frame1$means[is.nan(merged_frame1$means)] <- NA # Replaces all Nan's with NA for the sake of consistency
  
  # Merging Monthly Data
  merged_frame2 <- full_join(Condl_Monthly[,c('Year_mon',..site)], Condf_Monthly[,c('Year_mon',..site)], by = "Year_mon") %>%
    rowwise(Year_mon) %>%
    summarise(means = mean(c_across(1:2), na.rm = TRUE), .groups = 'drop')
  merged_frame2$means[is.nan(merged_frame2$means)] <- NA # Replaces all Nan's with NA for the sake of consistency
  
  # Adding the treated columns to the results frame
  Cond_Annual <- full_join(Cond_Annual, merged_frame1, by = "Year")
  Cond_Monthly <- full_join(Cond_Monthly, merged_frame2, by = "Year_mon")
  remove(merged_frame1, merged_frame2)
}

# Save Annual Data
names(Cond_Annual) <- c('Year', sites) # rename all columns to match their locations
write_csv(Cond_Annual, './data_cache/NutrientData/median_annual_Combined_Conductivity.csv', col_name=TRUE)

# Save Monthly Data
names(Cond_Monthly) <- c('Year_mon', sites) # rename all columns to match their locations
write_csv(Cond_Monthly, './data_cache/NutrientData/mean_monthly_Combined_Conductivity.csv', col_name=TRUE)

remove(Cond_Annual,Condf_Annual,Condl_Annual,Cond_Monthly,Condf_Monthly,Condl_Monthly,site,sites)

# pH ############################################################################################################################

summarize_WQ_data('pH,_Field','annual')
summarize_WQ_data('pH,_Field','monthly')

# Dissolved Oxygen #####################################################################

summarize_WQ_data('Dissolved_Oxygen','annual')
summarize_WQ_data('Dissolved_Oxygen','monthly')

# Dissolved Oxygen, Field ##############################################################

summarize_WQ_data('Dissolved_Oxygen,_Field','annual')
summarize_WQ_data('Dissolved_Oxygen,_Field','monthly')

# Dissolved Oxygen, Combined ####################################################################################################

# Load the Lab Samples
DOl_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Dissolved_Oxygen.csv')
DOl_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Dissolved_Oxygen.csv') 
DOl_Monthly$Year_mon <- as.yearmon(DOl_Monthly$Year_mon)

# Load the Field Samples
DOf_Annual <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/median_annual_Dissolved_Oxygen,_Field.csv')
DOf_Monthly <- fread('~/KC-Streams-Analysis/data_cache/NutrientData/mean_monthly_Dissolved_Oxygen,_Field.csv')
DOf_Monthly$Year_mon <- as.yearmon(DOf_Monthly$Year_mon)

# Merge the samples
sites = names(DOl_Annual)[-1] # Gathers list of site names

DO_Annual <- DOl_Annual[,1] # Beginning list with all the available years, luckily both frames have the same number of years
DO_Monthly <- DOl_Monthly[,1]

# This loop will merge the time series for each site, then take the mean of the two sets
# It will then put that resulting set on the end of the results data frame
for (site in sites){
  # Merging Annual Data
  merged_frame1 <- full_join(DOl_Annual[,c('Year',..site)], DOf_Annual[,c('Year',..site)], by = "Year") %>%
    rowwise(Year) %>%
    summarise(means = mean(c_across(1:2), na.rm = TRUE), .groups = 'drop')
  merged_frame1$means[is.nan(merged_frame1$means)] <- NA # Replaces all Nan's with NA for the sake of consistency
  
  # Merging Monthly Data
  merged_frame2 <- full_join(DOl_Monthly[,c('Year_mon',..site)], DOf_Monthly[,c('Year_mon',..site)], by = "Year_mon") %>%
    rowwise(Year_mon) %>%
    summarise(means = mean(c_across(1:2), na.rm = TRUE), .groups = 'drop')
  merged_frame2$means[is.nan(merged_frame2$means)] <- NA # Replaces all Nan's with NA for the sake of consistency
  
  # Adding the treated columns to the results frame
  DO_Annual <- full_join(DO_Annual, merged_frame1, by = "Year")
  DO_Monthly <- full_join(DO_Monthly, merged_frame2, by = "Year_mon")
  remove(merged_frame1, merged_frame2)
}

# Save Annual Data
names(DO_Annual) <- c('Year', sites) # rename all columns to match their locations
write_csv(DO_Annual, './data_cache/NutrientData/median_annual_Combined_Dissolved_Oxygen.csv', col_name=TRUE)

# Save Monthly Data
names(DO_Monthly) <- c('Year_mon', sites) # rename all columns to match their locations
write_csv(DO_Monthly, './data_cache/NutrientData/mean_monthly_Combined_Dissolved_Oxygen.csv', col_name=TRUE)

remove(DO_Annual,DOf_Annual,DOl_Annual,DO_Monthly,DOf_Monthly,DOl_Monthly,site,sites)

# Temperature #########################################################################

summarize_WQ_data('Temperature','annual')
summarize_WQ_data('Temperature','monthly')
