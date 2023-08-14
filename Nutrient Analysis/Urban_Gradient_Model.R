# This Urban gradient model is based on the findings of a 2005 Brett paper, where he fits a model of urban land cover to phosphate concentrations in the stream segments ##########################################
# What counts as urban land cover? It is defined differently in the KC streams report
library(plyr)
library(dplyr)
library(data.table)
library(mgcv)
library(ggplot2)
source('./functions/get_socrata_data_func.R')
bigTable <- fread('./data_cache/KC_WQ_Data')

NOx_annual <- fread('~/KC-Streams-Analysis/data_cache/median_annual_Nitrite_+_Nitrate_Nitrogen.csv')
PO4_annual <- fread('./data_cache/median_annual_Orthophosphate_Phosphorus.csv')