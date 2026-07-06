# Source The Function, Initialize data frames, Call any Additional Libraries ############################################
source('./functions/get_socrata_data_func.R')

watershed_build_years <- readRDS("~/KC-Streams-Analysis/data_cache/SourceData/watershed_build_years.RDS")
watershed_build_years <- watershed_build_years %>% group_by(Locator) %>%
  reframe(YRBUILT = YRBUILT,
            ParcelsBuilt = ParcelsBuilt,
            ParcelsBuiltCume = cumsum(ParcelsBuilt),
            ParcelsBuiltPer100Acres = ParcelsBuiltPer100Acres,
            ParcelsBuiltPer100AcresCume = cumsum(ParcelsBuiltPer100Acres))
parcel_record_window <- watershed_build_years %>% group_by(Locator) %>%
  summarise(min_year = min(YRBUILT),
            max_year = max(YRBUILT))
