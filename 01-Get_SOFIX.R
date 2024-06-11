# Forecasting Volatility of SOFIX Index with GARCH Models 
# Paper at mdpi
# The resulting dataset is serialized (saved) in a rds file named data/SOFIX.rds,
# to be used in the next step.

## MAIN OPTIONS (fell free to edit it)

first_date <- '2000-10-23' # first date in sample 
last_date <- '2024-03-28' # set Sys.Date() for current date 
my_ticker <- '^SOFIX' 
series_name <- 'SOFIX' # Name of index/stock that will show up in all plots

## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)

# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data 



library(readxl)
df_prices  <- read_excel("data/Sofix.xls", col_types = c("date", 
                                                     "text", "numeric", "numeric", "numeric", "numeric", 
                                                     "text"))




# save data into file
rds_out <- 'data/SOFIX.rds'
write_rds(df_prices, rds_out)

# download inflation  data 
df_infl <- read_excel("data/CPI-monthly_pecent-sample.xls", col_types = c("date", "numeric"))

df_infl$ref.date <- as.Date(df_inflation$ref.date, format = "%Y-%m-%d")

df_inflation <- df_infl %>% 

mutate(df_infl,inf_index  = cumprod(1+df_inflation$value/100))
print(df_inflation,n = 288)

rds_out <- 'data/cpi.rds'
write_rds(df_inflation, rds_out)
                                                              
