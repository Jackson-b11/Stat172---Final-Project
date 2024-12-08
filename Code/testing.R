
rm(list=ls())

library(tidyverse)
library(logistf)
library(glmnet)
library(haven)
library(knitr)

source("Code/clean_cps.R")


f = cps_data %>% 
  select(c(FSTOTXPNC_perpers,FSSTATUS,FSSTATUSMD,FSFOODS,FSWROUTY,FSBAL,FSRAWSCRA,FSTOTXPNC))

# Assuming your data frame is named 'df'

# Specify the columns of interest
columns_of_interest <- c("FSTOTXPNC_perpers", "FSSTATUS", "FSSTATUSMD", 
                         "FSFOODS", "FSWROUTY", "FSBAL", "FSRAWSCRA", "FSTOTXPNC")

# Count NAs for each column
na_counts <- sapply(f[columns_of_interest], function(col) sum(is.na(col)))

# Create a new data frame with the counts
na_counts_df <- data.frame(Column = names(na_counts), NA_Count = na_counts)

# Display the new data frame
na_counts_df_flipped <- as.data.frame(t(na_counts_df))





