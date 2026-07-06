source('./functions/get_socrata_data_func.R')
library(factoextra)

Parcels_Per100Acres <- readRDS("~/KC-Streams-Analysis/data_cache/SourceData/watershed_build_years.RDS") %>%
  pivot_wider(id_cols = YRBUILT, names_from = 'Locator', values_from = ParcelsBuiltPer100Acres) %>%
#Parcels_Per100Acres <- read_csv("data_cache/Misc/Parcels_Per100Acres.csv", col_types = cols(...1 = col_skip())) %>% 
  pivot_longer(!YRBUILT, names_to = 'Locator', values_to = 'ParcelsBuiltPer100Acres') %>%
  replace_na(list(ParcelsBuiltPer100Acres = 0)) %>%
  subset(YRBUILT >= 1900) %>%
  group_by(Locator) %>%
  reframe(Year = YRBUILT,
    ParcelsPer100Acre_Cume = cumsum(ParcelsBuiltPer100Acres))
 

Dev_Correlation <- Parcels_Per100Acres %>% subset(Year == 2022) %>%
  inner_join(streams_2019lulc, by = 'Locator') %>%
  select(all_of(c('Locator', 'ParcelsPer100Acre_Cume', 'TIA - Med', 'Developed, Total', 'Forest, Total'))) %>%
  inner_join(Srp_Parcel_Addition)

pairs.panels(Dev_Correlation[,2:5], smooth = FALSE, method = 'spearman', scale = TRUE, lm = FALSE, cex.cor = 1, main = 'Scatterplot Matrix for Landcover Data')

parcel_pca <- Parcels_Per100Acres %>%
  subset(Locator %in% Dev_Correlation$Locator) %>%
  pivot_wider(names_from = Year, values_from = ParcelsPer100Acre_Cume, id_cols = Locator) %>%
  column_to_rownames(var = 'Locator')

parcel_decomp <- prcomp(parcel_pca, scale. = FALSE, center = TRUE)

fviz_eig(parcel_decomp)

fviz_contrib(pacrel_decomp, choice = "var", axes = 1)
fviz_contrib(pacrel_decomp, choice = "ind", axes = 1)

fviz_pca_ind(parcel_decomp,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(parcel_decomp,
                   col.ind = "cos2", # Color by the quality of representation
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE     # Avoid text overlapping
  )

biplot(pacrel_decomp, choices = c(1,2))

parcel_PC1 <- as_tibble(parcel_decomp[['rotation']][,1], rownames = NA) %>% rownames_to_column(var = 'Year') %>%
  mutate(Year = as.numeric(Year))

parcel_PC2 <- as_tibble(parcel_decomp[['rotation']][,2], rownames = NA) %>% rownames_to_column(var = 'Year') %>%
  mutate(Year = as.numeric(Year))

ggplot(parcel_PC1, aes(Year, value)) +
  geom_line() +
  ggtitle('PC1')

ggplot(parcel_PC2, aes(Year, value)) +
  geom_line() +
  ggtitle('PC2')

# Low Cumulative Parcels #######################################################################

ggplot(data = Parcels_Per100Acres %>% 
       subset(Locator %in% c('GRIFFIN','TOLT_MTH','RAGING_MTH','NFK_SNQ','SFK_SNQ','SNQDUVALL',
                             'CHERRY_1','HARRIS_1','AMES_1','PATTER_3','LSIN1','LSIN9',
                             'BSE_1MUDMTNRD','VA12A','VA37A','VA41A','VA42A','VA45A',
                             '0311','0321','0322','0438','0631')),
       aes(x = Year, y = ParcelsPer100Acre_Cume, group = Locator, colour = Locator)) +
  geom_line(linewidth = 1.5) +
  ggtitle('PC 1 - Cluster 1')
  
# Second Group Plot ##################################################################################

ggplot(data = Parcels_Per100Acres %>% 
         subset(Locator %in% c('A320','C320','G320','0484','B484','N484','0478',
                               'A315','0440','A685','0450CC','B499','A680','0317','D320')),
       aes(x = Year, y = ParcelsPer100Acre_Cume, group = Locator, colour = Locator)) +
  geom_line(linewidth = 1.5) +
  ggtitle('PC 1 - Cluster 2')
