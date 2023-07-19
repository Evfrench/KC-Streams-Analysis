source('./functions/get_socrata_data_func.R')  
library(data.table)

bigTable <- fread('./data_cache/KC_WQ_Data')
AnnualNO3_NO2 <- get_annual_median(bigTable, 'Nitrite_+_Nitrate_Nitrogen')

Annual_OrthoP <- get_annual_median(bigTable, 'Orthophosphate_Phosphorus')