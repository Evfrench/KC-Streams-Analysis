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
  reframe(annual_dev = value - median(value, na.rm=TRUE),
            Month = Month) 
#%>%
 # group_by(variable,Month) %>%
  #reframe(med_annual_dev = median(annual_dev, na.rm= TRUE))
  #dcast(variable + Year~Month, value.var = "annual_dev")

ggplot(Nmonthly_graph1, aes(x= Month, y= annual_dev)) +
  geom_boxplot(aes(group= Month)) +
  scale_x_continuous(breaks = 1:12,labels = 1:12) +
  geom_hline(yintercept = 0, linetype = 'twodash', color = 'grey', linewidth = 1) +
  ggtitle("Median Monthly Deviations from Annual Median (separated by site and year)")

# (median of all years, separated by site)
# Before running the DFA, lets do a cross-correlation function to see if there is any basis
corrplot(corr = cor(Nmonthly1[,-1], use = 'pairwise.complete.obs'), method = 'circle')
# There are definitely correlations between sites, how many of these are from the same river system?
# lets say..... ___ distinct-ish pairs

## DFA of Monthly Data ########################################################
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

# Here are the actual DFA runs
dfa_results <- tibble(Trends = NA, Variance = NA, AICc = NA, Iterations = NA, .rows = 12)
Rlist <- c('diagonal and equal', 'unconstrained')
k = 0

# Lets see how long the DFA takes, loop through 10 options: 5 states and 2 error matrices
#NOTE: You need to re-run this, most of the unconstrained models likely did not converge
#NOTE: Add a column to ensure that convergence happens this time
#NOTE: There is obvious seasonality in the factors, you should add a fourier series covariate to remove this seasonality
# Alternatively you can just do this same experiment with annual data instead
for (i in 1:6) {
  for (j in 1:2) {
    k = k + 1
    states = i + 2
    
    dfa <- MARSS(N_DFA, model = list(R=Rlist[j], m= states), form = 'dfa', method = 'TMB')
    
    dfa_results$Trends[k] <- states
    dfa_results$Variance[k] <- Rlist[j]
    dfa_results$AICc[k] <- dfa$AICc
    dfa_results$Iterations[k] <- dfa$numIter
  }
}
# Adding more model statistics
dfa_results$relLik <- exp(-0.5 * (dfa_results$AICc - min(dfa_results$AICc)))
dfa_results$AICwt <- dfa_results$relLik/sum(dfa_results$relLik)
dfa_results <- arrange(dfa_results, -AICwt)
# Whats the best model of all of these?


# Lets run the 'best' model again and perform a Varimax rotation on the results (thanks Mark)
checkmodel <- MARSS(N_DFA, model = list(R=dfa_results$Variance[1], m= dfa_results$Trends[1]), form = 'dfa', method = 'TMB')

# Pull the estimated factor loadings
Load_est <- coef(checkmodel, type = 'matrix')$Z

# Get the inverse rotation matrix
inv_H <- varimax(Load_est)$rotmat

# Rotate the factor loadings
Load_rot <- Load_est %*% inv_H

# Now rotate the process trends
Trend_rot <- solve(inv_H) %*% checkmodel$states
colnames(Trend_rot) <- colnames(N_DFA)

## Plotting the DFA Results ######################################################

# Create a 1x2 plot space, one for a process, the other for its loading
#Note: set axis limits to show scale of loadings across all of the factors
for (i in 1:8) {
  tsTitle <- paste('Factor',i,sep = ' ')
  barTitle <- paste('Factor',i,'Loadings',sep = ' ')
  par(mfcol = c(2,1))
  plot(Nmonthly1$Year_mon, Trend_rot[i,], type = 'l', lwd = 2, main = tsTitle, xlab = 'Months', ylab = '')
  barplot(Load_rot[,i], main = barTitle)
}

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

# Import the parcel development time series, we will use a 1-year time lag because many of the 2022 values are not updated
devts <- readRDS('~/KC-Streams-Analysis/data_cache/watershed_build_years.RDS') %>%
  subset(Locator %in% colnames(N_annual) & (YRBUILT >= 1979 & YRBUILT < 2023)) 

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
  as.matrix() %>%
  zscore()

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

corrplot(corr = cor(N_annual[,-1], use = 'pairwise.complete.obs'), method = 'circle')

## DFA of Annual Data #########################################################
dfa_results <- tibble(Trends = NA, Variance = NA, AICc = NA, Iterations = NA, .rows = 12)
Rlist <- c('diagonal and equal', 'unconstrained')
k = 0
# Lets see how long the DFA takes, loop through 10 options: 5 states and 
for (i in 1:6) {
  for (j in 1:2) {
    k = k + 1
    states = i + 2
    
    dfa <- MARSS(N_annual_dfa, model = list(R=Rlist[j], m= states), form = 'dfa', method = 'TMB')

    dfa_results$Trends[k] <- states
    dfa_results$Variance[k] <- Rlist[j]
    dfa_results$AICc[k] <- dfa$AICc
    dfa_results$Iterations[k] <- dfa$numIter
  }
}
# Adding more model statistics
dfa_results$relLik <- exp(-0.5 * (dfa_results$AICc - min(dfa_results$AICc)))
dfa_results$AICwt <- dfa_results$relLik/sum(dfa_results$relLik)


