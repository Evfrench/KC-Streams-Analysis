library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
source('./functions/get_socrata_data_func.R')  
bigTable <- fread('./data_cache/KC_WQ_Data')

NOx_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
PO4_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')

NOx_annual <- subset(NOx_annual, Year < 2023)
PO4_annual <- subset(PO4_annual, Year < 2023)

test_mod <- gam(A620 ~ s(Year), data = NOx_annual)
summary(test_mod)