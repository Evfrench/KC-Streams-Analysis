#let's do annual means (wateryear) and SD trend
library(mgcv)

#use log-transformed data for nutrients, tss, turbidity, fecal bacteria; "norm_parms" use untransformed data
norm_parms<-c('Temperature','Dissolved Oxygen','Conductivity','pH','Total Alkalinity')

water_year_summary<-wq_data %>%
  select(Locator,WaterYear,Parameter=newParameter,Conc=newValueMDL,Month=Month,nonDetect_Flag) %>%
  arrange(Locator,WaterYear,Parameter) %>%
  group_by(Locator,Parameter) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(WaterYear), cumsum(c(TRUE, diff(WaterYear) != 1))))))
         #YearsRange=paste(range(WaterYear),collapse='-')
  )%>%
  filter(YearsMonitored>=5) %>%
  filter(Parameter!='E. coli') %>%
  group_by(Locator,Parameter,WaterYear) %>%
  mutate(SamplesInYear=n()) %>%
  filter(SamplesInYear>=6) %>% # at least 6 samples in eyar
  summarise(
    MeanValue=ifelse(Parameter=='Temperature',mean(Conc),
                     ifelse(length(which(!nonDetect_Flag))<=2,
                            mean(Conc), #take mean detection limit
                            NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetect_Flag,Cdf=F,printstat = F)$KMmean)
    )[1],
    nonDetect_Flag=length(which(nonDetect_Flag)),
    SDValue=ifelse(Parameter=='Temperature',sd(Conc),
                   ifelse(length(which(!nonDetect_Flag))<=2,
                          sd(Conc), #take sd detection limit
                          NADA2::cfit(ifelse(Conc==0,0.1,Conc),nonDetect_Flag,Cdf=F,printstat = F)$KMsd)
    )[1]
  ) %>%
  mutate(YearsMonitored=length(unique(WaterYear)),
         YearsRange=toString(gsub(":", "-",
                                  as.character(split(as.integer(WaterYear), cumsum(c(TRUE, diff(WaterYear) != 1))))))
         #YearsRange=paste(range(WaterYear),collapse='-')
  ) %>%
  filter(YearsMonitored>=5)

#use general additive model to smooth data between years,
#use smoothed, detreneded data to calculate standard deviation
detrended_data<-water_year_summary %>%
  group_by(Locator,Parameter) %>%
  ###detrend annual means
  nest() %>%
  mutate(detrend.out=map2(data,Parameter,~{
    n_year=unique(.x$YearsMonitored)
    if(.y %in% norm_parms){
      gam(MeanValue~s(WaterYear,k=round(n_year/2))
          ,data=.x,method='REML')
    } else{
      gam(log(MeanValue)~s(WaterYear,k=round(n_year/2))
          ,data=.x,method='REML')
    }}),
    detrended.values=map2(detrend.out,Parameter,~{
      if(.y %in% norm_parms){
        # detrended.values=mean(.x$data$MeanValue)+resid(.x)
        detrended.values=mean(.x$model[,1])+resid(.x)
        smoothed.values<-fitted(.x)
      } else{
        # detrended.values=exp(mean(log(.x$data$MeanValue))+resid(.x))
        detrended.values=exp(mean(.x$model[,1])+resid(.x))
        smoothed.values<-exp(fitted(.x))
      }
      data.frame(Smoothed.Value=smoothed.values,
                 Detrended.Value=detrended.values)
    }
    )) %>%
  unnest(c(data,detrended.values))


#calculate long-term mean and SD thresholds, calculate mean of the last 5 years monitored (this can have gaps!)
#nutrients, tss, turbidity, and fecal coliform means & SDs are based on a log-transformation (geometric)
overall_ave=detrended_data%>%
  group_by(Locator,Parameter,YearsRange) %>%
  summarise(
    ArithMean=mean(MeanValue),
    SD=sd(Detrended.Value),
    MeanLog=mean(log(Detrended.Value)),
    SDLog=sd(log(Detrended.Value)),
    Mean=ifelse(unique(Parameter) %in% norm_parms,
                ArithMean,
                exp(MeanLog)),
    Last5Mean=ifelse(unique(Parameter) %in% norm_parms,
                     mean(MeanValue[(length(MeanValue)-5):length(MeanValue)]),
                     exp(mean(log(MeanValue[(length(MeanValue)-5):length(MeanValue)]))))) %>%
  mutate(UpperThreshold_1SD=ifelse(Parameter %in% norm_parms,
                                   ArithMean+SD,exp(MeanLog+SDLog)),
         LowerThreshold_1SD=ifelse(Parameter %in% norm_parms,
                                   ArithMean-SD,exp(MeanLog-SDLog)),
         noise=ifelse(Parameter %in% norm_parms,
                      SD,sqrt((exp(SDLog^2)-1)*exp(2*MeanLog+SDLog^2)))) %>%
  mutate(Change=ifelse(Last5Mean>=UpperThreshold_1SD,'Meaningful Increase',
                       ifelse(Last5Mean<=LowerThreshold_1SD,'Meaningful Decrease','Maintaining'))) %>%
  mutate(Parameter=factor(Parameter,levels=parm_order)) %>%
  filter(!is.na(Parameter))