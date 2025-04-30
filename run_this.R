# Run this file to get estimates
library(here)
# create functions
source("01_functions.R")

# create 4 estimates for 4 regions 
source("10_create_CI_tables_indiv.R")

# create 3 estimates for 4 regions where regions 3+4 are considered 1 "region"
source("11_create_CI_tables_modified.R")


