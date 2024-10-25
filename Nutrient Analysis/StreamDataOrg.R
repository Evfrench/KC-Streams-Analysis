source('./functions/get_socrata_data_func.R')

WRIA7<- get_socrata_data_func(locns = c('AMES_1','CHERRY_1','GRIFFIN','HARRIS_1',
                                        'PATTER_3', 'RAGING_MTH','SKYHOMISH','SNQDUVALL',
                                        'MKF_SNQ','NFK_SNQ','SFK_SNQ','TOLT_MTH'),
                                       parms = default_data_parms,
                                       SiteType = 'Streams and Rivers')


WRIA8_1<- get_socrata_data_func(locns = c('0484','0484A','C484','J484','0438','A438',
                                        'X438','0442','A499','N484','N484A','A685',
                                        'A690','B484','S484','0498','0456','0456A',
                                        'A456','A620','0631','0632','A631','A632',
                                        '0446','C446','0444'),
                              parms = default_data_parms,
                              SiteType = 'Streams and Rivers')


WRIA8_2<- get_socrata_data_func(locns = c('D444','A670','A617',
                                        '0478','S478','0430','0440','A432','0474',
                                        'D474','A680','KSHZ06','KTHA01','KTHA02',
                                        '0450','0486','0450CC','0470','BB470','0434',
                                        'A630','X630','KTHA03','B499','A687'),
                              parms = default_data_parms,
                              SiteType = 'Streams and Rivers')


WRIA9<- get_socrata_data_func(locns = c('C320','0321','F321','FF321','0305','0307',
                                        '0308','0309','0311','3106','A319','B319',
                                        'D320','G320','C370','J370','A315','0322',
                                        'LSIN9','LSIN1','A320','0317'),
                              parms = default_data_parms,
                              SiteType = 'Streams and Rivers')
   

WRIA10<- get_socrata_data_func(locns = c('BSE_1MUDMTNRD'),
                              parms = default_data_parms,
                              SiteType = 'Streams and Rivers')


WRIA15<- get_socrata_data_func(locns = c('VA23A','VA41A','VA65A','VA42A','VA45A','VA12A','VA37A'),
                               parms = default_data_parms,
                               SiteType = 'Streams and Rivers')


WRIA_Combined <- rbind(WRIA7,WRIA8_1,WRIA8_2,WRIA9,WRIA10,WRIA15) 

All_KC_WQ_Data <- normalize_water_quality_data_parameters(WRIA_Combined) # consider removing this function in the future. It is not necessary and time-intensive 
All_KC_WQ_Data[All_KC_WQ_Data == 0] <- NA
All_KC_WQ_Data <- ALL_KC_WQ_Data %>%
  mutate(Decimal_year = decimal_date(CollectDate),
         Year_Mon = as.yearmon(Decimal_year),
         .before = 2) %>%
  rowwise() %>%
  mutate(Wtr_Year = replace(Year, Month %in% c(10:12), Year+1),
         .before = 1)

write_csv(All_KC_WQ_Data, './data_cache/SourceData/KC_WQ_Data.csv', col_name=TRUE)

#bigTable <- fread('~/KC-Streams-Analysis/data_cache/SourceData/KC_WQ_Data.csv')
#bigTable <- bigTable %>%
#  mutate(Decimal_year = decimal_date(CollectDate),
#         Year_Mon = as.yearmon(Decimal_year),
#         .before = 2) %>%
#  rowwise() %>%
#  mutate(Wtr_Year = replace(Year, Month %in% c(10:12), Year+1),
#         .before = 1)
#
#write_csv(bigTable, './data_cache/SourceData/KC_WQ_Data.csv', col_name=TRUE)

