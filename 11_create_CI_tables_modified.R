# 11_create_CI_tables_modified.R
# The purpose of this file is to create tables for the mean and SE of the survey data, but combining regions 3 and 4. This is represented by variable v024, so we'll modify this variable
# so that if(v024 == 3) or (v024 == 4), we set v024 = 3. This will allow us to combine the two regions in the output tables.

# The code below is a modified version of the code in 10_create_CI_tables.R, with the addition of the region modification.



for(i in survey_files)
{
  # # load the data
  # in_surv_mod <- load_data(dhs_dirpath,i)
  #combine regions 3 and 4
  in_surv_mod <- in_surv %>%
    mutate(v024_mod = ifelse(v024 == (4), (3), v024)) 
  
  #give it the same labels as the original variable
  in_surv_mod$v024_mod <- labelled(c(in_surv_mod$v024_mod),
                                   val_labels(in_surv_mod$v024))
  #reassign back to original variable name
  in_surv_mod$v024 <- in_surv_mod$v024_mod
  #get CIs
  calculate_output_CI(in_surv_mod, append = "combined")
  
}




