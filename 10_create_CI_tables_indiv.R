# 10_create_CI_tables.R


# here we'll list all of the files we have
library(here)

source("01_functions.R")

#path for files -- enter here
dhs_dirpath <- here("inputs/")
#this looks for .DTA files in the dhs_dirpath above.
survey_files <- list.files(path = dhs_dirpath, pattern = "\\.DTA$", full.names = FALSE, recursive = TRUE)



for(i in survey_files)
{
  # load the data
  in_surv <- load_data(dhs_dirpath,i)
  
  #get CIs
  calculate_output_CI(in_surv, append = "indiv")
  
}





