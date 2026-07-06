source('./functions/get_socrata_data_func.R')

Parcels_Per100Acres <- read_csv("data_cache/Misc/Parcels_Per100Acres.csv", col_types = cols(...1 = col_skip())) %>% 
  pivot_longer(!YRBUILT, names_to = 'Locator', values_to = 'ParcelsBuiltPer100Acres') %>%
  subset(YRBUILT >= 1900) %>%
  replace_na(list(ParcelsBuiltPer100Acres = 0)) %>%
  group_by(Locator) %>%
  reframe(Year = YRBUILT,
    ParcelsPer100Acre_Cume = cumsum(ParcelsBuiltPer100Acres))
 
used_sites <- c("0311","0317","0321","0322","0430","0434","0438","0440",
                  "0442","0444","0446","0450CC","0470","0474","0478","0484",
                  "0631","A315","A320","A432","A456","A499","A617","A620",
                  "A630","A670","A680","A685","A687","A690","AMES_1","B484",
                  "B499","BSE_1MUDMTNRD","C320","C370","CHERRY_1","D320",
                  "G320","GRIFFIN","HARRIS_1","KSHZ06","KTHA03","LSIN1",
                  "LSIN9","N484","NFK_SNQ","PATTER_3","RAGING_MTH","SFK_SNQ",
                  "SNQDUVALL","TOLT_MTH","VA12A","VA37A","VA41A","VA42A",
                  "VA45A","VA65A")        

Dev_Correlation <- Parcels_Per100Acres %>% subset(Year == 2022) %>%
  inner_join(streams_2019lulc, by = 'Locator') %>%
  subset(Locator %in% used_sites) %>%
  select(all_of(c('Locator', 'ParcelsPer100Acre_Cume', 'TIA - Med', 'Developed, Total', 'Forest, Total')))

pairs.panels(Dev_Correlation[,2:5], smooth = FALSE, method = 'pearson', scale = TRUE, lm = TRUE, cex.cor = 1, main = 'Scatterplot Matrix for Landcover Data')

