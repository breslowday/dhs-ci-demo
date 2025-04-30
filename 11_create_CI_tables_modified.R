# 11_create_CI_tables_modified.R
# The purpose of this file is to create tables for the mean and SE of the survey data, but combining regions 3 and 4. This is represented by variable v024, so we'll modify this variable
# so that if(v024 == 3) or (v024 == 4), we set v024 = 3. This will allow us to combine the two regions in the output tables.

# The code below is a modified version of the code in 10_create_CI_tables.R, with the addition of the region modification.



for(i in survey_files)
{
  # load the data
  in_surv_mod <- load_data(dhs_dirpath,i)
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


# let's look at the function call here:
ad <- in_surv_mod #%>% filter(v024 == 1)
DHSdesign <- svydesign(id = ~ad$v001,        # Equivalent to hv001
                       weights = ~ad$wt,   
                       strata = ~ad$v022,    
                       data = ad, 
) 

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")



# ok now we have this working for each state, can we output this as a loop for each state here?
n_adm1 <- (ad %>% distinct(v024)) %>% as_vector()

# create database to store country name, DHS survey iteration, and ADM1 name
adm1_labels <- rownames_to_column(as.data.frame(ad$v024 %>% attr('labels'))) 
colnames(adm1_labels) <- c("adm1", "adm1_code")


#add country and DHS phase (1-7)
adm1_labels <- adm1_labels %>% mutate(country_v000 = unique(ad$v000))
#create placeholder for estimate and standard error
adm1_labels <- adm1_labels %>% mutate(estimate = NA, se = NA)

# run through each state and calculate the mean and SE of coverage for that state.
# once you have the mean and SE you can sample from this normal (logit) distribution and then plogis that sample back to a proportion
for(i in 1:length(n_adm1))
{
  adsub = subset(DHSdesign,v024==adm1_labels[i,2])
  adm1_svyglm <- summary((svyglm(ch_meas_either~ 1, design = adsub, family = "quasibinomial")))
  adm1_labels[i, 4] <- adm1_svyglm$coefficients[1]
  adm1_labels[i, 5] <- adm1_svyglm$coefficients[2]
  # adsub = subset(DHSdesign,v024==i)
  # adm1_svyglm <- summary((svyglm(ch_meas_either~ 1, design = adsub, family = "quasibinomial")))
  # estimate <- adm1_svyglm$coefficients[1]
  # se <- adm1_svyglm$coefficients[2]
  # here we can add in the row binding structure to append these values to the full outcome for each adm1
  # we'll also need to add in which survey, and which labelled adm1 region it is.
}

#filename of mean and se
fn_mean_se = paste0(unique(KRvac$v000), "_estimates_", today(), ".csv")
write_csv(adm1_labels, here("outputs", fn_mean_se))





