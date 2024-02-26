library(MARSS)
library(marssTMB)
library(corrplot)
source('./functions/get_socrata_data_func.R')

# Analysis of Monthly Data ################################################
# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened
Nmonthly <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('monthly'))
Nmonthly$Year_mon <- as.yearmon(Nmonthly$Year_mon)


# Need to find a good starting point for the time series.
# Less empty slots mean a better performance
N_Entries <- tibble(Nmonthly[,'Year_mon'],rowSums(!is.na(Nmonthly[,-1])))
names(N_Entries) <- c('Year_mon','Entries')

# Plot the results
ggplot(N_Entries, aes(x = Year_mon, y = Entries)) +
  geom_col()
# NOTE: It looks like the beginning of 1979 is good spot to start


# We will cut off any time before 1979, and remove any site with readings for less than half of the months in the window
Nmonthly1 <- Nmonthly %>%
  subset(Year_mon >= 1979)

Nfilter <- sapply(Nmonthly1, function(x) sum(!is.na(x)))

for (site in names(Nfilter)){
  if (Nfilter[site] < (0.50*nrow(Nmonthly1))){
    Nmonthly1 <- Nmonthly1 %>% select(- all_of(site))
  }
}
# This leaves us with 52 sites, now to plot the results and determine if the data is acceptable


# Convert to a long table and plot the resulting time series
Nmonthly_graph <- Nmonthly1 %>%
  reshape2::melt(id.var='Year_mon')

ggplot(Nmonthly_graph, aes(Year_mon, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Monthly Nitrite+Nitrate Readings") +
  scale_y_continuous(name = "NO2+NO3, μg/L")

# There is some clear seasonality present, so lets make a typical "year" of box and whisker plots
# Start by adding a year and month column
# Take the median of each year for each monitoring site then calculate the monthly deviation
Nmonthly_graph1 <- Nmonthly_graph %>%
  mutate(Year = year(Year_mon),
         Month = month(Year_mon)) %>%
  group_by(variable,Year) %>%
  reframe(annual_dev = 100*(value - median(value, na.rm=TRUE))/(median(value, na.rm=TRUE)),
            Month = Month) %>%
  group_by(variable,Month) %>%
  reframe(med_annual_dev = median(annual_dev, na.rm= TRUE))
  #dcast(variable + Year~Month, value.var = "annual_dev")

ggplot(Nmonthly_graph1, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  ylab('% Deviation from Median') +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-100,200), n.breaks = 13) +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("NO3 % Monthly Deviations from Annual Median (separated by site)")

# (median of all years, separated by site)
# Before running the DFA, lets do a cross-correlation function to see if there is any basis
corrplot(corr = cor(Nmonthly1[,-1], use = 'pairwise.complete.obs'), method = 'circle')
# There are definitely correlations between sites, how many of these are from the same river system?
# lets say..... ___ distinct-ish pairs

# Since I am most familiar with the MARSS package, we will use that for now

# Put the dissolved nutrient records into the correct format and z-score it to improve algorithm performance
# I looked around and median-normalization seems infrequently used and possibly performs worse
N_DFA <- Nmonthly1 %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year_mon') %>%
  t() %>%
  zscore()


# Lets get a visual of the normalized data. How does it differ?
d <- N_DFA %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Year_mon') %>%
  reshape2::melt(id.var = 'Year_mon')
  
d[,1] <- as.yearmon(d[,1]) 
  
ggplot(d, aes(Year_mon, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Monthly Nitrite+Nitrate Readings, Normalized") + 
  scale_y_continuous(name = "St. Deviations from Mean")

## DFA of Monthly Data ########################################################
# Create a table to record the model results
dfa_results <- tibble(Trends = NA, Covariates = NA, AICc = NA, Iterations = NA, Converged = NA, Err_Var = NA, .rows = 24)
k = 0

# Create a covariate matrix with monthly effects, then add it to a list with with NULL so that the loop can cycle through each
month_cov <- matrix(0,12,ncol(N_DFA))
month_row <- match(months(Nmonthly1$Year_mon, abbreviate = TRUE), month.abb)[1:ncol(N_DFA)]
month_cov[cbind(month_row,1:ncol(N_DFA))] <- 1

# Add possible model parameters
cov_list <- list('None','Monthly Effect')
R_list <- list('diagonal and equal', 'diagonal and unequal')

# Lets see how long the DFA takes, it will look though 24 options, 6 states, 2 variance matrices, 2 covariate options

#NOTE: Add a column to ensure that convergence happens this time
#NOTE: There is obvious seasonality in the factors, you should add a fourier series covariate to remove this seasonality
# Alternatively you can just do this same experiment with annual data instead
for (h in 1:2) {
  for (i in 1:6) {
    for (j in 1:2) {
      k = k + 1
      states = i + 2
      
      if (j == 1){
        dfa <- MARSS(N_DFA, model = list(R= Rlist[h], m= states, tinitx= 1), form = 'dfa', method = 'TMB', covariates = NULL)
      }
      else{
        dfa <- MARSS(N_DFA, model = list(R= Rlist[h], m= states, tinitx= 1), form = 'dfa', method = 'TMB', covariates = month_cov)
      }
     # tabulate results of the DFA models 
      dfa_results$Trends[k] <- states
      dfa_results$AICc[k] <- dfa$AICc
      dfa_results$Iterations[k] <- dfa$numIter
      dfa_results$Covariates[k] <- cov_list[j]
      dfa_results$Converged[k] <- dfa$iter.record$message
      dfa_results$Err_Var[k] <- R_list[h]
      remove(dfa)
    }
  }
}

# Adding more model statistics
converged_models <- dfa_results %>%
  subset(Converged == 'relative convergence (4)') %>%
  mutate(relLike = exp(-0.5 * (AICc - min(AICc))), 
         AICwt = relLike/sum(relLike)) %>%
  arrange(-AICwt)


# Whats the best model of all of these?, apparently 8 trends, and equal variance matrix, and a month covariate

# Lets run the 'best' model again and perform a Varimax rotation on the results (thanks Mark)
set_DFA <- MARSS(N_DFA, model = list(R= 'diagonal and equal', m= 8, tinitx= 1), form = 'dfa', method = 'TMB', covariates = month_cov)

# Pull the estimated factor loadings
Load_est <- coef(set_DFA, type = 'matrix')$Z

# Get the inverse rotation matrix
inv_H <- varimax(Load_est)$rotmat

# Rotate the factor loadings
Load_rot <- Load_est %*% inv_H

# Now rotate the process trends
Trend_rot <- solve(inv_H) %*% set_DFA$states
colnames(Trend_rot) <- colnames(N_DFA)
Trend_rot <- Trend_rot %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Year_Mon')

## Plotting the DFA Results ######################################################

for (i in 1:8) {
tsTitle <- paste('Factor',i,sep = ' ')

print(ggplot(Trend_rot) +
  geom_line(aes(as.yearmon(Year_Mon), Trend_rot[,i+1]), linewidth = 0.75) +
  ylab('Deviations') + xlab("Date") +
  ggtitle(label = tsTitle))

barTitle <-  barTitle <- paste('Factor',i,'Loadings',sep = ' ')

print(ggplot(as.data.frame(Load_rot), aes(x = rownames(Load_rot), y = Load_rot[,i])) +
  geom_col(fill = 'darkcyan') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Monitoring Site') + ylab('Loading Coefficient') +
  scale_y_continuous(limits = c(-0.8,0.8), n.breaks = 9) +
  ggtitle(label = barTitle))
}

autoplot.marssMLE(set_DFA2)


# It looks like not all of the seasonality was removed from the factors
# Need to check for correlation between the factors
# 1, 2, and 8 look very similar
# so do 3 and 4

Ccf(Trend_rot[1,],Trend_rot[2,])
Ccf(Trend_rot[1,],Trend_rot[8,])
Ccf(Trend_rot[2,],Trend_rot[8,])
Ccf(Trend_rot[3,],Trend_rot[4,])


# Analysis of Annual Data ###################################################
N_annual <- summarize_WQ_data(bigTable, c('Nitrite_+_Nitrate_Nitrogen'), c('annual')) %>%
  subset(Year >= 1979 & Year <2023) %>%
  remove_rownames()

Nfilter1 <- sapply(N_annual, function(x) sum(!is.na(x)))

for (site in names(Nfilter1)){
  if (Nfilter1[site] < (0.50*nrow(N_annual))){
    N_annual <- N_annual %>% select(- all_of(site))
  }
}


# Put the data in normalized DFA form
N_annual_dfa <- N_annual %>%
  remove_rownames() %>%
  column_to_rownames(var = 'Year') %>%
  t() %>%
  zscore() 

# What it the built parcels have a delayed effect on the 
# Test for covariance between the parcel data and water quality records, ignore all positive lags
for (i in 1:nrow(dev_dfa)) {
  ccf(dev_dfa[i,], N_annual_dfa[i,],type = 'covariance', na.action = na.pass, lag.max = 10)
}
# It looks like there is no consistent covariance at different time lags

# Same thing but for correlation
for (i in 1:nrow(dev_dfa)) {
  ccf(dev_dfa[i,], N_annual_dfa[i,],type = 'correlation', na.action = na.pass, lag.max = 10)
}


# Create a data frame of the normalized data that can be plotted
N_annual_plot <- N_annual_dfa %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Yr') %>%
  reshape2::melt(id.var = 'Yr') %>%
  mutate(Year = as.numeric(Yr), .keep = 'unused')

ggplot(N_annual_plot, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Normalized Nitrite+Nitrate Readings") +
  scale_y_continuous(name = "NO2+NO3, St. Dev's")
# If I had to guess off these, then there are between 4 to 7 common trends?

N_annual_plot1 <- N_annual %>%
  as.data.frame() %>%
  reshape2::melt(id.var = 'Year') 

ggplot(N_annual_plot1, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Annual Median Nitrite+Nitrate Readings") +
  scale_y_continuous(name = "NO2+NO3, μg/L")


corrplot(corr = cor(N_annual[,-1], use = 'pairwise.complete.obs'), method = 'circle')

## DFA of Annual Data #########################################################
# Create a table to record the model results
dfa_results_an <- tibble(Trends = NA, AICc = NA, Iterations = NA, Converged = NA, .rows = 6)
k = 0

# Create a covariate matrix with monthly effects, then add it to a list with with NULL so that the loop can cycle through each
# Lets see how long the DFA takes, it will look though 24 options, 6 states, 2 variance matrices, 2 covariate options

#NOTE: Add a column to ensure that convergence happens this time
#NOTE: There is obvious seasonality in the factors, you should add a fourier series covariate to remove this seasonality
# Alternatively you can just do this same experiment with annual data instead
  for (i in 1:6) {
      k = k + 1
      states = i + 2
      
        dfa <- MARSS(N_annual_dfa, model = list(R= 'diagonal and equal', m= states, tinitx= 1), form = 'dfa', method = 'TMB', covariates = NULL)

      # tabulate results of the DFA models 
      dfa_results_an$Trends[k] <- states
      dfa_results_an$AICc[k] <- dfa$AICc
      dfa_results_an$Iterations[k] <- dfa$numIter
      dfa_results_an$Converged[k] <- dfa$iter.record$message
      remove(dfa)
    }

# Adding more model statistics
converged_models_an <- dfa_results_an %>%
  subset(Converged == 'relative convergence (4)') %>%
  mutate(relLike = exp(-0.5 * (AICc - min(AICc))), 
         AICwt = relLike/sum(relLike)) %>%
  arrange(-AICwt)


# Whats the best model of all of these?, apparently 8 trends, and equal variance matrix, and a month covariate

# Lets run the 'best' model again and perform a Varimax rotation on the results (thanks Mark)
set_DFA2 <- MARSS(N_annual_dfa, model = list(R= 'diagonal and equal', m= 8, tinitx= 1), form = 'dfa', method = 'TMB')

# Pull the estimated factor loadings
Load_est <- coef(set_DFA2, type = 'matrix')$Z

# Get the inverse rotation matrix
inv_H <- varimax(Load_est)$rotmat

# Rotate the factor loadings
Load_rot2 <- Load_est %*% inv_H

# Now rotate the process trends
Trend_rot2 <- solve(inv_H) %*% set_DFA2$states
colnames(Trend_rot2) <- colnames(N_annual_dfa)
Trend_rot2 <- Trend_rot2 %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Year')


## Plotting the Annual DFA Results ######################################################

for (i in 1:8) {
  tsTitle <- paste('Factor',i,sep = ' ')
  
  print(ggplot(Trend_rot2) +
          geom_hline(yintercept = 0, linetype = 'dashed', color = 'darkgrey', linewidth = 1) +
          geom_line(aes(as.numeric(Year), Trend_rot2[,i+1]), linewidth = 0.75) +
          ylab('Deviations') + xlab("Date") +
          ggtitle(label = tsTitle))
  
  barTitle <-  barTitle <- paste('Factor',i,'Loadings',sep = ' ')
  
  print(ggplot(as.data.frame(Load_rot2), aes(x = rownames(Load_rot2), y = Load_rot2[,i])) +
          geom_hline(yintercept = c(-0.3,0.3), linetype = 'dashed', color = 'darkgrey', linewidth = 1) +
          geom_col(fill = 'darkcyan') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
          xlab('Monitoring Site') + ylab('Loading Coefficient') +
          scale_y_continuous(limits = c(-1.0,1.0), breaks = (-5:5 * 0.2)) +
          ggtitle(label = barTitle)) 
          
}


# Looking at the land-use data and performing a DFA #####################


# Import the parcel development time series, we will use a 1-year time lag because many of the 2022 values are not updated
devts <- readRDS('~/KC-Streams-Analysis/data_cache/watershed_build_years.RDS') 
#%>%
#  subset(Locator %in% colnames(N_annual) & (YRBUILT >= 1979 & YRBUILT < 2023)) 

ggplot(devts, aes(YRBUILT, ParcelsBuiltPer100Acres)) +
  facet_wrap(. ~ Locator, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Parcels Built per 100 Acres per Year") +
  scale_y_continuous(name = "Parcels")

# It looks like there are missing values in the parcel time series, I will need to explore more if we want to use this in the DFA
# Well this was the main reason I put the nutrient data into annual summaries. It will have to wait


dev_dfa <- devts %>%
  reshape2::dcast(Locator ~ YRBUILT, value.var = 'ParcelsBuiltPer100Acres') %>%
  column_to_rownames(var = 'Locator') %>%
  as.matrix()
 

norm_dev <- dev_dfa %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Year') %>%
  reshape2::melt(id.var = "Year")
norm_dev$Year <- as.numeric(norm_dev$Year)

ggplot(norm_dev, aes(Year, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Parcels Built per 100 Acres per Year, Normalized") +
  scale_y_continuous(name = "Deviations from mean")

dfa_results_dev <- tibble(Trends = NA, AICc = NA, Iterations = NA, Converged = NA, .rows = 6)
k = 0

# Create a covariate matrix with monthly effects, then add it to a list with with NULL so that the loop can cycle through each
# Lets see how long the DFA takes, it will look though 24 options, 6 states, 2 variance matrices, 2 covariate options

#NOTE: Add a column to ensure that convergence happens this time
#NOTE: There is obvious seasonality in the factors, you should add a fourier series covariate to remove this seasonality
# Alternatively you can just do this same experiment with annual data instead
for (i in 1:6) {
  k = k + 1
  states = i + 2
  
  dfa <- MARSS(dev_dfa, model = list(R= 'diagonal and equal', m= states, tinitx= 1), form = 'dfa', method = 'TMB', covariates = NULL)
  
  # tabulate results of the DFA models 
  dfa_results_dev$Trends[k] <- states
  dfa_results_dev$AICc[k] <- dfa$AICc
  dfa_results_dev$Iterations[k] <- dfa$numIter
  dfa_results_dev$Converged[k] <- dfa$iter.record$message
  remove(dfa)
}

# Adding more model statistics
converged_models_dev <- dfa_results_dev %>%
  subset(Converged == 'relative convergence (4)') %>%
  mutate(relLike = exp(-0.5 * (AICc - min(AICc))), 
         AICwt = relLike/sum(relLike)) %>%
  arrange(-AICwt)


# Whats the best model of all of these?, apparently 8 trends, and equal variance matrix, and a month covariate

# Lets run the 'best' model again and perform a Varimax rotation on the results (thanks Mark)
set_DFA3 <- MARSS(dev_dfa, model = list(R= 'diagonal and equal', m= 8, tinitx= 1), form = 'dfa', method = 'TMB')

# Pull the estimated factor loadings
Load_est <- coef(set_DFA3, type = 'matrix')$Z

# Get the inverse rotation matrix
inv_H <- varimax(Load_est)$rotmat

# Rotate the factor loadings
Load_rot3 <- Load_est %*% inv_H

# Now rotate the process trends
Trend_rot3 <- solve(inv_H) %*% set_DFA3$states
colnames(Trend_rot3) <- colnames(dev_dfa)
Trend_rot3 <- Trend_rot3 %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'Year')


## Plotting the Development DFA Results ######################################################

for (i in 1:8) {
  tsTitle <- paste('Factor',i,sep = ' ')
  
  print(ggplot(Trend_rot3) +
          geom_line(aes(as.numeric(Year), Trend_rot3[,i+1]), linewidth = 0.75) +
          ylab('Deviations') + xlab("Date") +
          ggtitle(label = tsTitle))
  
  barTitle <-  barTitle <- paste('Factor',i,'Loadings',sep = ' ')
  
  print(ggplot(as.data.frame(Load_rot3), aes(x = rownames(Load_rot3), y = Load_rot3[,i])) +
          geom_col(fill = 'darkcyan') +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
          xlab('Monitoring Site') + ylab('Loading Coefficient') +
          scale_y_continuous(limits = c(-0.8,0.8), n.breaks = 9) +
          ggtitle(label = barTitle))
}



# Attempted PCA of Development data ####################################################
library(fdapace)
# How does this PCA thing work? I need to experiment more
yearvec <- list()
readings <- list()

for (i in 1:ncol(dev_dfa)) {
  readings[[i]] <- dev_dfa[,i]
  yearvec[[i]] <- as.numeric(rownames(dev_dfa))
}


for (loc in colnames(dev_dfa)){
  yearvec[, loc] <- as.numeric(rownames(dev_dfa))
}


dev_pca <- FPCA(readings, yearvec, list(plot = TRUE))
CreateDesignPlot(dev_dfa)

# Testing for Seasonality in Phosphate ##############################################

# Create a monthly data frame for the target parameter
# NOTE: saving the workspace image and restarting will coerce the 'yearmon' class in this data frame to a decimal year
# It must be converted back whenever the workspace is reopened
Pmonthly <- summarize_WQ_data(bigTable, c('Orthophosphate_Phosphorus'), c('monthly'))
Pmonthly$Year_mon <- as.yearmon(Pmonthly$Year_mon)

# We will cut off any time before 1979, and remove any site with readings for less than half of the months in the window
Pmonthly1 <- Pmonthly %>%
  subset(Year_mon >= 1979)

Pfilter <- sapply(Pmonthly1, function(x) sum(!is.na(x)))

for (site in names(Pfilter)){
  if (Pfilter[site] < (0.50*nrow(Pmonthly1))){
    Pmonthly1 <- Pmonthly1 %>% select(- all_of(site))
  }
}
# This leaves us with 50 sites, now to plot the results and determine if the data is acceptable


# Convert to a long table and plot the resulting time series
Pmonthly_graph <- Pmonthly1 %>%
  reshape2::melt(id.var='Year_mon')

ggplot(Pmonthly_graph, aes(Year_mon, value)) +
  facet_wrap(. ~ variable, shrink = FALSE) + 
  geom_point() +
  geom_line() +
  ggtitle("Monthly Orthophosphate Readings") +
  scale_y_continuous(name = "PO4, μg/L")

# There is some clear seasonality present, so lets make a typical "year" of box and whisker plots
# Start by adding a year and month column
# Take the median of each year for each monitoring site then calculate the monthly deviation
Pmonthly_graph1 <- Pmonthly_graph %>%
  mutate(Year = year(Year_mon),
         Month = month(Year_mon)) %>%
  group_by(variable,Year) %>%
  reframe(annual_dev = 100*(value - median(value, na.rm=TRUE))/(median(value, na.rm=TRUE)),
          Month = Month) %>%
  group_by(variable,Month) %>%
  reframe(med_annual_dev = median(annual_dev, na.rm= TRUE)) 

ggplot(Pmonthly_graph1, aes(x= Month, y= annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("PO4 Median Monthly Deviations from Annual Median (separated by site and year)")

ggplot(Pmonthly_graph1, aes(x= Month, y= med_annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  scale_y_continuous(limits = c(-75, 150), n.breaks = 10) +
  ylab('% Deviation from Median') +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("PO4 % Monthly Deviations from Annual Median (separated by site)")

KC_Pop <- tibble(Year = c(1970, 1980, 1990, 2000, 2010, 2020), Population = c(1156633,1269749,1507319,1737034,1931249,2269675))

ggplot(KC_Pop, aes(x= Year, y= Population)) +
  ggtitle('King County Population') +
  scale_y_continuous(limits = c(0,2300000)) +
  geom_line(lwd = 2) +
  geom_point(size = 4)


