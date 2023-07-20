source('./functions/get_socrata_data_func.R')  
library(data.table)

bigTable <- fread('./data_cache/KC_WQ_Data')
AnnualNO3_NO2 <- get_annual_median(bigTable, 'Nitrite_+_Nitrate_Nitrogen')

Annual_OrthoP <- get_annual_median(bigTable, 'Orthophosphate_Phosphorus')

# AnnualNO3_NO2 <- AnnualNO3_NO2 %>% remove_rownames() %>% column_to_rownames(var = "Year")
# Annual_OrthoP <- Annual_OrthoP %>% remove_rownames() %>% column_to_rownames(var = "Year")

datSelect <- tibble(AnnualNO3_NO2[,"Year"], rowSums(!is.na(AnnualNO3_NO2[,-1])),rowSums(!is.na(Annual_OrthoP[,-1])))
names(datSelect) <- c('Year','NOx_Entries','PO4_Entries')

ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = NOx_Entries, color = 'Nitrogen')) 

ggplot() +
  geom_col(data = datSelect, aes(x = Year, y = PO4_Entries, color = 'Phosphorus'))

