# 01_functions.R


#load packages
library(survey)
library(haven)
library(tidyverse)
library(labelled)

check_existing_output <- function(in_path)
{
  # check if the file exists
  # pull the filename from the survey_files[i]], so anything that is after the last slash
  # if any csv file begins with the same pattern of the first 2 characters and the fifth character and matches today's date
  # of the DTA file pulled from the survey files vector in the folder "dhs-ci/outputs" then we don't run the following loop below
  # get today's date
  today <- Sys.Date()
  
  # get the file name without extension
  file_name <- tools::file_path_sans_ext(basename(in_path))
  
  # create a pattern to match
  pattern <- paste0(paste0(substr(file_name, 1, 2), substr(file_name, 5, 5)), "_estimates_", today, ".csv")
  print(pattern)
  # check if any csv files match the pattern
  existing_files <- list.files(path = here("outputs"), pattern = pattern, full.names = TRUE)
  print(existing_files)
  if (length(existing_files) > 0) {
    message("Output already exists for ", in_path)
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# check_existing_output("TZKR7BDT/TZKR7BFL.DTA")


load_data <- function(dir_path, in_path)
{
  #open dataset from directory path and file path within directory
  data <-  read_dta(paste0(dir_path, in_path))
  print(paste0(dir_path, in_path))
  # check if the data is loaded
  if (is.null(data)) {
    stop("Data not loaded")
  }
  
  return(clean_KR_vac(data))
}






clean_KR_vac <- function(KRdata)
{
  # weight variable 
  KRdata <- KRdata %>%
    mutate(wt = v005/1000000)
  
  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}
  
  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }
  
  # *** Two age groups used for reporting. # By default the a24_35 flag is set to 0 and thus 13-24 month old kids are the only ones included.
  KRdata <- KRdata %>%
    mutate(agegroup = 
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")
  
  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children 
  
  # *******************************************************************************
  
  # Source of vaccination information. We need this variable to code vaccination indicators by source.
  KRvac <- KRvac %>%
    mutate(source = 
             case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
    set_value_labels(source = c("card" = 1, "mother"=2)) %>%
    set_variable_labels(source = "source of vaccination information")
  
  KRvac <- KRvac %>%
    mutate(ch_meas_either = 
             case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_meas_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_either = "Measles vaccination according to either source")
  
  # //Measles mother's report
  KRvac <- KRvac %>%
    mutate(ch_meas_moth = 
             case_when(h9%in%c(1,2,3) & source==2 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(ch_meas_moth = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_moth = "Measles vaccination according to mother")
  
  # //Measles by card
  KRvac <- KRvac %>%
    mutate(ch_meas_card = 
             case_when(h9%in%c(1,2,3) & source==1 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(ch_meas_card = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_card = "Measles vaccination according to card")
}


calculate_output_CI <- function(KRvac, append = "")
{
  ad <- KRvac #%>% filter(v024 == 1)
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
  adm1_labels <- adm1_labels %>% mutate(country_v000 = unique(KRvac$v000))
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
   
    # here we can add in the row binding structure to append these values to the full outcome for each adm1
    # we'll also need to add in which survey, and which labelled adm1 region it is.
  }
  
  # we can also create a file of the mean and SE of coverage for each state that is not in logit form
  adm1_labels <- adm1_labels %>% mutate(transformed_estimate = plogis(estimate),
                                        transformed_lb       = plogis(estimate - 1.96*se),
                                        transformed_ub       = plogis(estimate + 1.96*se))
  #filename of mean and se
  fn_mean_se = paste0(unique(KRvac$v000), "_estimates_", append, "_", today(), ".csv")
  write_csv(adm1_labels, here("outputs", fn_mean_se))
}

## USED ONLY FOR DRC PHASE V (Phase 5)
calculate_output_CI2 <- function(KRvac)
{
  ad <- KRvac #%>% filter(v024 == 1)
  DHSdesign <- svydesign(id = ~ad$v001,        # Equivalent to hv001
                         weights = ~ad$wt,   
                         strata = ~ad$strat,    # Equivalent to strata(hv023)
                         data = ad, 
                         # nest = "T",
  ) # Equivalent to singleunit(centered)
  
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  
  # adsub = subset(DHSdesign,v024==1)
  # summary(adsub)
  
  # # 
  #   
  # all_states_glm <- glm(ch_meas_either~1, data = ad %>% filter(v024 == 1),family = "quasibinomial", weights = wt)
  # summary(all_states_glm)
  # 
  # #   
  # all_states_svyglm <- (svyglm(ch_meas_either~ 1, design = adsub, family = "quasibinomial"))
  # summary(all_states_svyglm)
  # all_states_svyglm$fitted.values
  # plogis(confint.default(all_states_svyglm))
  # plogis(confint.default(all_states_glm))
  # svyby(~ch_meas_either, design = DHSdesign, by = ~v024, FUN=svymean, vartype = c("se", "ci"))  
  
  
  # ok now we have this working for each state, can we output this as a loop for each state here?
  n_adm1 <- (KRvac %>% distinct(v024)) %>% as_vector()
  
  # create database to store country name, DHS survey iteration, and ADM1 name
  adm1_labels <- rownames_to_column(as.data.frame(KRvac$v024 %>% attr('labels'))) 
  colnames(adm1_labels) <- c("adm1", "adm1_code")
  #add country and DHS phase (1-7)
  adm1_labels <- adm1_labels %>% mutate(country_v000 = unique(KRvac$v000))
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
}

