
library(tidyverse)
library(logistf)
library(glmnet)
library(haven)
library(knitr)



cps = read.csv("Data/cps_00006.csv")

cps <- cps %>%
  mutate(SEX = SEX - 1 , # Create dummy variables
         CHILD = ifelse(AGE < 18, 1, 0),
         ELDERLY = ifelse(AGE > 59, 1, 0), 
         BLACK = ifelse(RACE==200, 1, 0),
         HISPANIC = ifelse(HISPAN>0, 1, 0),
         EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)),
         EMP = as.integer(EMPSTAT %in% c(1,10,12)),
         MARRIED = as.integer(MARST %in% c(1,2)),
         DIFF = ifelse(DIFFANY==2, 1, 0),
         COUNTY = as.factor(COUNTY))


cps_data <- cps %>%
  group_by(CPSID = as.factor(CPSID)) %>%
  summarise(COUNTY = first(COUNTY),
            #family level weight
            weight = first(HWTFINL),
            #household size
            hhsize = n(),
            #Y variables - i.e., measures of hunger
            #see CPS website for details
            #FSSTATUS, etc. is the same for each member -just take first value for each family
            FSTOTXPNC_perpers = FSTOTXPNC/hhsize, # In per person terms
            FSSTATUS = first(FSSTATUS),
            FSSTATUSMD = first(FSSTATUSMD),
            FSFOODS = first(FSFOODS),
            FSWROUTY = first(FSWROUTY),
            FSBAL = first(FSBAL),
            FSRAWSCRA = first(FSRAWSCRA),
            FSTOTXPNC = first(FSTOTXPNC),
            FSSTATUS = first(FSSTATUS),
            #count of family members in various categories
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black= sum(BLACK),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            married= sum(MARRIED),
            livalone = ifelse(hhsize == 1, 1, 0)
          
            
            ) %>%  ungroup()



cps_data <- cps_data %>%
  mutate(FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
         FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
         FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
         FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
         FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
         FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),#raw score
         FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC)) %>%
  mutate(FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
         FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
         FSFOODS = ifelse(FSFOODS > 1, 1, 0),
         FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),#more missings
         FSBAL = ifelse(FSBAL > 1, 1, 0),
         FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))

