source('./functions/get_socrata_data_func.R')
library(dataRetrieval)

# Sites are at Renton, Auburn, and Black Diamond

# Required Data ###############################################################


    ## Green River Water Quality #################################################
  GreenRiver_WQ_Data <- get_socrata_data_func(locns = c('3106','0311','A319','B319')) %>%
    subset(Parameter != 'Storm Or Non-Storm' & Parameter != 'Enterococcus' & Parameter != 'Silica' & Parameter != 'E. coli') %>%
    mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),  # merge the two DO and conductivity fields
           Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
           Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                   "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L')) %>%
    rowwise() %>%
    mutate(Value = replace(Value, Qualifier == '<MDL', MDL*0.5)) %>% # This will replace all left-censored samples with one-half of the MDL. Note: ~ 6% of Ammonia samples were left-censored, other parameters were negligible
    mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                   "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000)) %>% # Convert nutrient values to micrograms per liter for convenience
    group_by(Locator, Year, Month, Parameter) %>%
    summarise(Value = mean(Value, na.rm = T),
              Units = first(Units)) %>%
    mutate(Units = replace(Units, Units %in% c('ORG/100ml'), 'CFU/100ml'),
           Month = lubridate::month(Month, label = T))
    
    
    ## Green River Discharge Data ##################################################

  gages<-c(12113000,12113344,12113350,12112600,12108500) # From upstream to downstream
  
  Testflow <- USGS_Flow_Query(gages)
  
  GreenRiver_Discharge <- Testflow %>% pivot_wider(id_cols = 'Date', names_from = 'SITE_CODE', values_from = 'AveQ') %>%  # Separates the time series for each gage into separate columns for easier comparison
    subset((Date <= as_date('1984/12/31') & Date >= as_date('1970/01/01')) | (Date >= as_date('2011/01/01'))) %>%
    mutate(proportion = `12113344`/`12113000`)
  
  Seasonal_discharge <- GreenRiver_Discharge %>%   # This table is created to examine the summary statistics of the difference between the upstream and downstream gages. both in additive and multiplicative terms
    mutate(Month = lubridate::month(Date, label = T)) %>%
   group_by(Month)%>%
    summarise(Avg_Multiple_RecentYears = mean(proportion, na.rm = T),
              Median_Multiple_RecentYears = median(proportion, na.rm = T),
              StDev_Multiple_RecentYears = sd(proportion, na.rm = T),
              StErr__Multiple_RecentYears = sd(proportion, na.rm = T)/sqrt(n()))
  
  Discharge_Offsets <- Seasonal_discharge[,c(1,3)]   # Creates a list of conversion factors based on month for estimating discharge at the downstream site
  
  # This creates a discharge record for the Green River at site B319 that can then be used as training data for an ARIMA model that will backcast for previous readings
  UpUpDischargeEst <- Testflow %>%
    bind_rows(Crisp_Creek_Gage) %>%
    subset(SITE_CODE %in% c('12113000','12112600','12108500','40d') & (Date >= as_date('1994/08/25') & Date <= as_date('2025/10/20'))) %>%
    pivot_wider(id_cols = 'Date', names_from = 'SITE_CODE', values_from = 'AveQ') %>%
    mutate(Discharge_Estimate = `12113000`-`12112600`-`12108500`-`40d`,
           Year_Mon = as.yearmon(Date)) 
  %>%
    group_by(Year_Mon) %>%
    summarise(Green = mean(`12113000`,na.rm=T),
              Soos = mean(`12112600`,na.rm=T),
              Newaukum = mean(`12108500`,na.rm=T),
              Crisp = mean(`40d`,na.rm=T),
              Up_Green = mean(Discharge_Estimate,na.rm=T))
  
  fit <- auto.arima(rev(as.ts(UpUpDischargeEst[,6])), stepwise = F, approximation = F, seasonal = T)
  
  
  
  ggplot(data = Testflow %>% subset((Date <= 1984 & Date >= 1960) | (Date >= 2011)), aes(Date, AveQ)) +
    geom_line(aes(colour = factor(SITE_CODE)))
  
  Monthly_Dis_Upstream <- Testflow %>%  # Isolates the upstream gage readings and bins it into monthly averages
    subset(SITE_CODE == '12113000') %>%
    mutate(Year = lubridate::year(Date),
           Month = lubridate::month(Date, label = T)) %>%
    group_by(Year, Month) %>%
    summarise(`AveQ (cfs)` = mean(AveQ, na.rm = T))
  
  Monthly_Dis_Downstream <- Monthly_Dis_Upstream %>% # Estimates a more complete record for the downstream gage using monthly correction factors on the upstream gage
    rowwise() %>%
    mutate(`AveQ (cfs)` = ifelse(Month == 'Jan', `AveQ (cfs)` * Discharge_Offsets[1,2],
                                 ifelse(Month == 'Feb', `AveQ (cfs)`*Discharge_Offsets[2,2],
                                        ifelse(Month == 'Mar', `AveQ (cfs)`*Discharge_Offsets[3,2],
                                               ifelse(Month == 'Apr', `AveQ (cfs)`*Discharge_Offsets[4,2],
                                                      ifelse(Month == 'May', `AveQ (cfs)`*Discharge_Offsets[5,2],
                                                             ifelse(Month == 'Jun', `AveQ (cfs)`*Discharge_Offsets[6,2],
                                                                    ifelse(Month == 'Jul', `AveQ (cfs)`*Discharge_Offsets[7,2],
                                                                           ifelse(Month == 'Aug', `AveQ (cfs)`*Discharge_Offsets[8,2],
                                                                                  ifelse(Month == 'Sep', `AveQ (cfs)`*Discharge_Offsets[9,2],
                                                                                         ifelse(Month == 'Oct', `AveQ (cfs)`*Discharge_Offsets[10,2],
                                                                                                ifelse(Month == 'Nov', `AveQ (cfs)`*Discharge_Offsets[11,2],
                                                                                                       ifelse(Month == 'Dec', `AveQ (cfs)`*Discharge_Offsets[12,2],
                                                                                                              `AveQ (cfs)`)))))))))))))
  
  # flow gages at green river sites 3106 and 0311 (gage 12113350) was only active between 1970-1984
  # gage 12113390 is 3km downstream (2013-2020)
  # gage 12113344 is 8.1km upstream (2011-present)
  # A319 is very far upstream, requires use of gage 12113000 (1936-present)
  # gage 12112600 is Soos Creek
  # gage 12108500 is Newaukum Creek
  # B319 is estimated by taking 12113000 and subtracting gages 12112600, 12108500, and 40
    
    ## Annual Pink Salmon Run Population ##############################################
  GreenRiver_Salmon <- 1
    
    
# Analysis Section ##################################################################
    
    ## Creating TDS Estimation: Upstream #####################################
      
    GR_Upstream_WQ_data <- GreenRiver_WQ_Data %>%
      subset(Locator == 'A319') %>%
      left_join(Monthly_Dis_Upstream, by = c('Year', 'Month')) %>%
      mutate(Locator = 'Upstream',
             AveQ = as.numeric(`AveQ (cfs)`)) %>%
      select(-all_of('AveQ (cfs)'))

    # This data frame converts conductivity to total dissolved solids, then puts in back in a form that can be appended to the larger water quality data frame    
    # Conversion of conductivity to TDS, use 0.64 uS/cm -> ppm (mg/L)  EC25 = ECt [1+0.022(25-T)] correction for temperatures +- 15 deg C from 25 C
    
    GR_TDS <- GR_Upstream_WQ_data %>% 
      subset(Parameter %in% c('Temperature','Conductivity')) %>%
      select(-all_of(c('Units'))) %>%
      pivot_wider(names_from = Parameter, values_from = Value) %>%
      mutate(`Total Dissolved Solids` = 0.64*Conductivity*(1+0.022*(25-Temperature)),  
             Units = 'mg/L') %>%
      select(-all_of(c('Conductivity','Temperature'))) %>%
      pivot_longer(!c(Locator, Year, Month, AveQ, Units), names_to = 'Parameter', values_to = 'Value')
    
    GR_Upstream_WQ_data <- GR_Upstream_WQ_data %>%
      bind_rows(GR_TDS) # This adds the TDS calculations into the data frame
    
    
    
    ## Creating TDS Estimation and Merging Sites: Downstream #####################################
      
    GR_Downstream_WQ_data <- GreenRiver_WQ_Data %>%
      subset(Locator != 'A319') %>%
      left_join(Monthly_Dis_Downstream, by = c('Year', 'Month')) %>%
      subset(Locator != '3106' | Year > 1986 ) %>%
      group_by(Year, Month, Parameter) %>%
      summarise(Locator = 'Downstream',
                Value = mean(Value, na.rm = T),
                Units = first(Units),
                AveQ = mean(as.numeric(`AveQ (cfs)`), na.rm = T))
   
    # This data frame converts conductivity to total dissolved solids, then puts in back in a form that can be appended to the larger water quality data frame    
    GR_TDS <- GR_Downstream_WQ_data %>% 
      subset(Parameter %in% c('Temperature','Conductivity')) %>%
      select(-all_of('Units')) %>%
      pivot_wider(names_from = Parameter, values_from = Value) %>%
      mutate(`Total Dissolved Solids` = 0.64*Conductivity*(1+0.022*(25-Temperature)),  
             Units = 'mg/L') %>%
      select(-all_of(c('Conductivity','Temperature'))) %>%
      pivot_longer(!c(Locator, Year, Month, AveQ, Units), names_to = 'Parameter', values_to = 'Value')

    GR_Downstream_WQ_data <- GR_Downstream_WQ_data %>%
      bind_rows(GR_TDS) # This adds the TDS calculations into the data frame
    
    
    ## Merge Upstream & Downstream tables, Convert To Yield #################################
    GR_WQ_Conc <- GR_Upstream_WQ_data %>%
      bind_rows(GR_Downstream_WQ_data) %>%
      mutate(Even_Odd_Year = if_else(Year %% 2 == 0, 'Even_Year', 'Odd_Year'))
    
    GR_WQ_Yield <- GR_WQ_Conc %>%
      subset(!(Parameter %in% c('Conductivity', 'Fecal Coliform', 'Temperature', 'Dissolved Oxygen', 'Turbidity', 'pH, Field'))) %>% # Removes all constituents that are not important for calculating in terms of yield
      rowwise() %>%
      mutate(exp_Value = ifelse(Units == 'ug/L', Value*AveQ*2.447/1000, Value*AveQ*2.447),  # This calculates an average daily export for each month for each month
             exp_Units = 'kg/day')
    
    ggplot(data = GR_WQ_Conc %>% subset(Parameter == 'Ammonia Nitrogen'&Month=='Oct') %>%
           mutate(YearPeriod=ifelse(Year<2000,"Low Pinky Pop",ifelse(Year<3000,"Pinky Coming Up","pinky's Back, bitch"))), 
           aes(Month, Value, colour = Even_Odd_Year)) +
      facet_grid(YearPeriod~as_factor(Locator)) +
      geom_violin()+
      ggbeeswarm::geom_quasirandom(aes(group=Month*Even_Odd_Year)) +
      scale_y_log10() 
      
    
    ggplot(data = GR_WQ_Yield %>% subset(Parameter == 'Ammonia Nitrogen'&Month=='Oct') %>%
             mutate(YearPeriod=ifelse(Year<2000,"Low Pinky Pop",ifelse(Year<3000,"Pinky Coming Up","pinky's Back, bitch"))), 
           aes(Month, Value, colour = Even_Odd_Year)) +
      facet_grid(YearPeriod~as_factor(Locator)) +
      geom_violin()+
      ggbeeswarm::geom_quasirandom(aes(group=Month*Even_Odd_Year)) +
      scale_y_log10() 
    
    ## Odd-Even Year Analysis ###################################################################
      GR_Odd_Even_Conc <- GR_WQ_Conc %>%
      subset(Year >= 2000) %>%
      group_by(Month, Locator, Parameter, Even_Odd_Year) %>%
      summarise(Med_Value = median(Value, na.rm = T),
                Avg_Value = mean(Value, na.rm = T),
                Units = first(Units))
    
      GR_Odd_Even_Yield <- GR_WQ_Yield %>%
      subset(Year >= 2000) %>%
      group_by(Month, Locator, Parameter, Even_Odd_Year) %>%
      summarise(Med_Value = median(exp_Value, na.rm = T),
                Avg_Value = mean(exp_Value, na.rm = T),
                Units = first(exp_Units))
     
      

# Even/Odd year monthly time series for water quality constituents
        
      ### Median Concentration Plots ################
      i = 1
      Upstream_Conc_Summary <- list()
      Downstream_Conc_Summary <- list()
      
      for (Parm in unique(GR_Odd_Even_Conc$Parameter)) {
        
        Unit <- first((GR_Odd_Even_Conc %>% subset(Parameter == Parm))$Units)
        Plot <- ggplot(GR_Odd_Even_Conc %>% subset(Parameter == Parm), aes(Month,Med_Value, group = Even_Odd_Year, colour = Even_Odd_Year)) + 
          facet_wrap( . ~ as_factor(Locator), nrow = 2) +
          geom_line() +
          geom_point() +
          scale_y_continuous(limits = c(0,NA))+
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('2000 - 2025 Group Medians')
    
        print(Plot)
        
        Upstream_Conc_Summary[[i]] <- t.test(Value ~ Even_Odd_Year, data = GR_WQ_Conc %>% subset(Parameter == Parm & Locator == 'Upstream' & Year >= 2000 & Month == 'Oct'))
        names(Upstream_Conc_Summary)[i] <- Parm
        
        Downstream_Conc_Summary[[i]] <- t.test(Value ~ Even_Odd_Year, data = GR_WQ_Conc %>% subset(Parameter == Parm & Locator == 'Downstream' & Year >= 2000 & Month == 'Oct'))
        names(Upstream_Conc_Summary)[i] <- Parm
        
        remove(Unit, Plot)
        i <- i + 1
      } 
      ### Average Concentration Plots ################
#    ggplot(GR_Odd_Even_Conc %>% subset(Parameter == "Fecal Coliform"), aes(Month,Avg_Value, group = Even_Odd_Year, colour = Even_Odd_Year)) + 
#      facet_wrap( . ~ as_factor(Locator), nrow = 2) +
#      geom_line() +
#      geom_point() +
#      scale_y_continuous(limits = c(0,NA))+
#      ylab("Fecal Coliform (CFU/100 mL)") +
#      ggtitle('2006 - 2025 Group Means')
    
      
      
      ### Median Yield Plots ######################
      
      i = 1
      Upstream_Yield_Summary <- list()
      Downstream_Yield_Summary <- list()
      
      for (Parm in unique(GR_Odd_Even_Yield$Parameter)) {
        
        Unit <- first((GR_Odd_Even_Yield %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_Odd_Even_Conc %>% subset(Parameter == Parm), aes(Month,Med_Value, group = Even_Odd_Year, colour = Even_Odd_Year)) + 
          facet_wrap( . ~ as_factor(Locator), nrow = 2) +
          geom_line() +
          geom_point() +
          scale_y_continuous(limits = c(0,NA))+
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('2000 - 2025 Group Medians')
        
        print(Plot)
        
        Upstream_Yield_Summary[[i]] <- t.test(Value ~ Even_Odd_Year, data = GR_WQ_Yield %>% subset(Parameter == Parm & Locator == 'Upstream' & Year >= 2000 & Month == 'Oct'))
        names(Upstream_Yield_Summary)[i] <- Parm
        
        Downstream_Yield_Summary[[i]] <- t.test(Value ~ Even_Odd_Year, data = GR_WQ_Yield %>% subset(Parameter == Parm & Locator == 'Downstream' & Year >= 2000 & Month == 'Oct'))
        names(Downstream_Yield_Summary)[i] <- Parm
        
        remove(Unit, Plot)
        i <- i + 1
      } 
      
      ### Average Yield Plots #####################
    ggplot(GR_Odd_Even_Yield %>% subset(Parameter == "Total Suspended Solids"), aes(Month,Avg_Value, group = Even_Odd_Year, colour = Even_Odd_Year)) + 
      facet_wrap( . ~ as_factor(Locator), nrow = 2) +
      geom_line() +
      geom_point() +
      scale_y_continuous(limits = c(0,NA))+
      ylab("Total Suspended Solids (kg/day)") +
      ggtitle('2006 - 2025 Group Means')
    
      
    ## Creating Annual TS Using Sep-Nov Data ############################
    
      GR_Annual_Salmon_Conc <- GR_WQ_Conc %>%
        subset(Month %in% c('Oct')) %>%
        group_by(Locator, Year, Parameter) %>%
        summarise(Value = mean(Value, na.rm=T),
                  Units = first(Units),
                  AveQ = mean(AveQ))
      
      GR_Annual_Salmon_Yield <- GR_WQ_Yield %>%
        subset(Month %in% c('Oct')) %>%
        group_by(Locator, Year, Parameter) %>%
        summarise(Value = mean(Value, na.rm=T),
                  Units = first(Units),
                  exp_Value = mean(exp_Value, na.rm=T),
                  exp_Units = first(exp_Units),
                  AveQ = mean(AveQ))  
      
      
      ### Concentration Plots ###############
      
      for (Parm in unique(GR_Annual_Salmon_Conc$Parameter)) {
        
        Unit <- first((GR_Annual_Salmon_Conc %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_Annual_Salmon_Conc %>% subset(Parameter == Parm), aes(Year, Value)) +
          facet_wrap( . ~ as_factor(Locator), nrow = 2) +
          geom_line() +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('October In The Green River')
        
        print(Plot)
        

        remove(Unit, Plot)
      }
      
      
      ### Yield Plots ###########################
    
      
      for (Parm in unique(GR_Annual_Salmon_Yield$Parameter)) {
        
        Unit <- first((GR_Annual_Salmon_Yield %>% subset(Parameter == Parm))$exp_Units)
        
        Plot <- ggplot(GR_Annual_Salmon_Yield %>% subset(Parameter == Parm), aes(Year, exp_Value)) +
          facet_wrap( . ~ as_factor(Locator), nrow = 2) +
          geom_line() +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('October In The Green River')
        
        print(Plot)
        
        
        remove(Unit, Plot)
      }
      
    ## Pre- and Post-Outfall Study #########################
      
      ### Exclusively Using Years 1976-1996, Create Monthly, Annual, and Yield TS ############################
      
      GR_Outfall_Monthly <- GreenRiver_WQ_Data %>%
        full_join(Monthly_Dis_Downstream, by = c('Year','Month')) %>%
        subset(Locator == '3106' & Year <= 1996 & Year >= 1976 & Parameter != 'Total Nitrogen') %>%
        mutate(Outfall = if_else(Year <= 1986, 'Before', 'After'))
     
      GR_Outfall_Annual <- GR_Outfall_Monthly %>%
        group_by(Year, Parameter, Outfall) %>%
        summarise(Value = median(Value, na.rm = T),
                  Units = first(Units),
                  AveQ = median (as.numeric(`AveQ (cfs)`, na.rm = T)))
      
      GR_Outfall_Yield <- GR_Outfall_Monthly %>%
        subset(Parameter %in% c("Ammonia Nitrogen","Nitrite + Nitrate Nitrogen","Orthophosphate Phosphorus","Total Alkalinity","Total Dissolved Solids","Total Nitrogen","Total Phosphorus","Total Suspended Solids")) %>%
        rowwise() %>%
        mutate(exp_Value = ifelse(Units == 'ug/L', Value*`AveQ (cfs)`*2.447/1000, Value*`AveQ (cfs)`*2.447),  # This calculates an average daily export for each month for each month
               exp_Units = 'kg/day') %>%
        group_by(Year, Parameter, Outfall) %>%
        summarise(Value = median(exp_Value, na.rm = T),
                  Units = first(exp_Units),
                  AveQ = median (as.numeric(`AveQ (cfs)`, na.rm = T)))
      
      
      ### Concentration Plots and T Tests ########################
      
      i = 1
      Stat_Summary <- list()
      
      for (Parm in unique(GR_Outfall_Annual$Parameter)) {
        
        Unit <- first((GR_Outfall_Annual %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_Outfall_Annual %>% subset(Parameter == Parm), aes(Year, Value)) +
          geom_line() +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          geom_vline(xintercept = 1986.5, linetype = 2) +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('Green River Pre- & Post-Outfall Removal')
        
        print(Plot)
        
        Stat_Summary[[i]] <- t.test(Value ~ Outfall, data = GR_Outfall_Monthly %>% subset(Parameter == Parm))
        names(Stat_Summary)[i] <- Parm
        
        remove(Unit, Plot)
        i <- i + 1
      } 
      
      
      ### Yield Plots and T Tests ################################
      
      i = 1
      Stat_Summary2 <- list()
      
      for (Parm in unique(GR_Outfall_Yield$Parameter)) {
        
        Unit <- first((GR_Outfall_Yield %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_Outfall_Yield %>% subset(Parameter == Parm), aes(Year, Value)) +
          geom_line() +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          geom_vline(xintercept = 1986.5, linetype = 2) +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('Green River Pre- & Post-Outfall Removal: Yield')
        
        print(Plot)
        
        Stat_Summary2[[i]] <- t.test(Value ~ Outfall, data = GR_Outfall_Yield %>% subset(Parameter == Parm))
        names(Stat_Summary2)[i] <- Parm
        
        remove(Unit, Plot)
        i <- i + 1
      }
      
      
      GR_Outfall_Med <- GR_Outfall_Monthly %>%
        group_by(Parameter, Outfall) %>%
        summarise(Median = median(Value, na.rm = T),
                  Units = first(Units),
                  AveQ = median(as.numeric(`AveQ (cfs)`)))
      
    

    
      
    ## Long Term Trend Analysis ##############################
      
      # Using the downstream site, and ignoring the Outfall
      GR_WQ_LT_Conc <- GR_WQ_Conc %>%
        subset(Locator == "Downstream") %>%
        group_by(Year, Parameter) %>%
        summarise(Value = median(Value, na.rm = T),
                  Units = first(Units))
      
      GR_WQ_LT_Yield <- GR_WQ_Yield %>%
        subset(Locator == "Downstream") %>%
        group_by(Year, Parameter) %>%
        summarise(Value = median(exp_Value, na.rm = T),
                  Units = first(exp_Units))
     
      
      ### Linear Trends: Concentration ################################
      
      LT_Trends <- list()
      i = 1
      
      for (Parm in unique(GR_WQ_LT_Conc$Parameter)) {
        
        Unit <- first((GR_WQ_LT_Conc %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_WQ_LT_Conc %>% subset(Parameter == Parm), aes(Year, Value)) +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          geom_smooth(method = 'lm', col = 'blue') +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('Green River Downstream Long-Term Trend')
        
        print(Plot)
        
        LT_Trends[[i]] <- lm(Value ~ Year, data = GR_WQ_LT_Conc %>% subset(Parameter == Parm))
        names(LT_Trends)[i] <- Parm
        
        i = i + 1
        remove(Unit, Plot)
      }
      
      
      ### Linear Trends: Yield #########################################
      
      LT_Trends_Yield <- list()
      i = 1
      
      for (Parm in unique(GR_WQ_LT_Yield$Parameter)) {
        
        Unit <- first((GR_WQ_LT_Yield %>% subset(Parameter == Parm))$Units)
        
        Plot <- ggplot(GR_WQ_LT_Yield %>% subset(Parameter == Parm), aes(Year, Value)) +
          geom_point() +
          #scale_y_log10(limits = c(1, NA)) +
          scale_y_continuous(limits = c(0,NA)) +
          geom_smooth(method = 'lm', col = 'blue') +
          ylab(paste(Parm, ' (', Unit, ')', sep = '')) +
          ggtitle('Green River Downstream Long-Term Trend: Yield')
        
        print(Plot)
        
        LT_Trends_Yield[[i]] <- lm(Value ~ Year, data = GR_WQ_LT_Yield %>% subset(Parameter == Parm))
        names(LT_Trends_Yield)[i] <- Parm
        
        i = i + 1
        remove(Unit, Plot)
      }
      
      
# A Dataframe to Check Individual Values #########################    
    GreenRiver_Check <- get_socrata_data_func(locns = c('3106','0311','A319')) %>%
      subset(Parameter != 'Storm Or Non-Storm' & Parameter != 'Enterococcus' & Parameter != 'Silica' & Parameter != 'E. coli') %>%
      mutate(Parameter = replace(Parameter, Parameter == 'Dissolved Oxygen, Field', 'Dissolved Oxygen'),  # merge the two DO and conductivity fields
             Parameter = replace(Parameter, Parameter == 'Conductivity, Field', 'Conductivity'),
             Units = replace(Units, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                     "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), 'ug/L')) %>%
      rowwise() %>%
      mutate(Value = replace(Value, Qualifier == '<MDL', MDL*0.5)) %>% # This will replace all left-censored samples with one-half of the MDL. Note: ~ 6% of Ammonia samples were left-censored, other parameters were negligible
      mutate(Value = replace(Value, Parameter %in% c("Ammonia Nitrogen", "Organic Nitrogen", "Nitrite + Nitrate Nitrogen", "Total Kjeldahl Nitrogen", "Total Nitrogen",
                                                     "Orthophosphate Phosphorus", "Total Phosphorus", "Total Hydrolyzable Phosphorus"), Value*1000))  # Convert nutrient values to micrograms per liter for convenience
    