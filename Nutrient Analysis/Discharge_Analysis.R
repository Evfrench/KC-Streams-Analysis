# This will take some of the flow data from across the county and and make a seasonal analysis and other types ofthe hydrological features
# How do I find the county gauges that can be used for this analysis?
# There should be one that have set gauges that pair with monitoring sites?
# 
source('./functions/get_socrata_data_func.R')

#Summarize to monthly average discharge
monthly <- readRDS(file = '~/KC-Streams-Analysis/data_cache/Hydrological/DailyAveFlow_allgages.RDS') %>%
  drop_na() %>%
  mutate(Year_mon = as.yearmon(Date)) %>%
  group_by(SITE_CODE, Year_mon) %>%
  summarize(AveQ30 = mean(AveQ, na.rm = TRUE))%>%
  reshape2::dcast(Year_mon ~ SITE_CODE, value.var = 'AveQ30')

seasonal_flow <- Seasonal_Analysis(monthly, form = 'Mean-Dev')

ggplot(seasonal_flow, aes(x= Month, y= mean_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  #scale_y_continuous(limits = c(-100, 200), n.breaks = 10) +
  ylab('Deviation from Mean (cfs)') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Discharge Deviation from Annual Mean")


# Long Term Hydrologic Trends ######################
# Upper and Lower 10% of ave daily discharges over time
# seasonal mean flow per year, then plot

# Plot the upper, lower, and middle daily discharge per water year
annualquant <- readRDS(file = '~/KC-Streams-Analysis/data_cache/Hydrological/DailyAveFlow_allgages.RDS') %>%
  drop_na() %>%
  mutate(WtrYear= ifelse(Month >= 10, Year+1, Year)) %>%
  group_by(SITE_CODE, SITE_NAME, WtrYear) %>%
  summarise(Annual90 = quantile(AveQ, probs = 0.9, na.rm = TRUE),
            Annual50 = quantile(AveQ, probs = 0.5, na.rm = TRUE),
            Annual10 = quantile(AveQ, probs = 0.1, na.rm = TRUE))

annualentries <-  data.frame(table(annualquant$WtrYear))

### Trends for the upper 90th percentile ##################################
uppertrends <- LT_Slope_Dist(reshape2::dcast(annualquant, WtrYear ~ SITE_CODE, value.var = 'Annual90'), window = c(1994,2013,2014,2023), cutoff = c(9,4), units = 'cfs', hydro = TRUE)
upperquant <- quantile(uppertrends$`Mean Slope (cfs/wtryear)`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)
upperpquant <- quantile(uppertrends$`% Change Per Water Year`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)

ggplot(uppertrends, aes(x = `Mean Slope (cfs/wtryear)`)) +
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(upperquant[2], upperquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = upperquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 90th Annual Percentile Slope Distribution') 

ggplot(uppertrends, aes(x = `% Change Per Water Year`)) +
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(upperpquant[2], upperpquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = upperpquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 90th Annual Percentile, Percent Change') 

# Check out the time series for the more interesting gages

### Trends for the lower 10th percentile ########################################
lowertrends <- LT_Slope_Dist(reshape2::dcast(annualquant, WtrYear ~ SITE_CODE, value.var = 'Annual10'), window = c(1994,2013,2014,2023), cutoff = c(9,4), units = 'cfs', hydro = TRUE)
lowerquant <- quantile(lowertrends$`Mean Slope (cfs/wtryear)`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)
lowerpquant <- quantile(lowertrends$`% Change Per Water Year`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)

ggplot(lowertrends, aes(x = `Mean Slope (cfs/wtryear)`)) + 
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(lowerquant[2], lowerquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = lowerquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 10th Annual Percentile Slope Distribution') 

ggplot(lowertrends, aes(x = `% Change Per Water Year`)) +
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(lowerpquant[2], lowerpquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = lowerpquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 10th Annual Percentile, Percent Change') 

# Check out the time series for the more interesting gages

### Trends for the 50th Percentile ################################################
midtrends <- LT_Slope_Dist(reshape2::dcast(annualquant, WtrYear ~ SITE_CODE, value.var = 'Annual50'), window = c(1994,2013,2014,2023), cutoff = c(9,4), units = 'cfs', hydro = TRUE)
midquant <- quantile(midtrends$`Mean Slope (cfs/wtryear)`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)
midpquant <- quantile(midtrends$`% Change Per Water Year`, probs = c(0.1,0.25,0.5,0.75,0.9), na.rm = T)

ggplot(midtrends, aes(x = `Mean Slope (cfs/wtryear)`)) +
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(midquant[2], midquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = midquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 50th Annual Percentile Slope Distribution') 

ggplot(midtrends, aes(x = `% Change Per Water Year`)) +
  geom_histogram(bins = 15) + 
  geom_vline(xintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  geom_vline(xintercept = c(midpquant[2], midpquant[4]), linetype = 'dashed', color = 'black', linewidth = 0.5) +
  geom_vline(xintercept = midpquant[3], linetype = 'solid', color = 'black', linewidth = 0.5) +
  ggtitle('Daily Average Discharge: 50th Annual Percentile, Percent Change') 

# Check out the time series for the more interesting gages
