#install.packages("purrr")
#install.packages("openxlsx")
#install.packages("Hmisc")
#install.packages("fastDummies")

rm(list=ls()) # clear workspace
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

library(haven) #read dataframes
library(dplyr) #modify columns
library(purrr) #merge dataframes by columns
library(Hmisc)
library(stargazer)
library(sandwich)
library(lmtest)
library(fastDummies)
library(fwildclusterboot)
library(broom)
library(fixest)

df_in_ven <- read_dta("Migrant_informal.dta")
df_fo_ven <- read_dta("Migrant_formal.dta")

df_in_ven <- df_in_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_fo_ven <- df_fo_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

#####################
# DATASET: INFORMAL #
#####################

################
#  VENEZUELAN  #
################

############
# LOG_WAGE #
############

feols_didreg_informal_ven = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                    DPTO+Time_FE, data = df_in_ven)
summary(feols_didreg_informal_ven)

############
# THO_WAGE #
############

feols_didreg2_informal_ven = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                     DPTO + Time_FE, data = df_in_ven)
summary(feols_didreg2_informal_ven)

############
# HRS_WEEK #
############

feols_didreg3_informal_ven = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                     DPTO + Time_FE, data = df_in_ven)

summary(feols_didreg3_informal_ven)

################
# LOG_HRS_WEEK #
################
feols_didreg4_informal_ven = feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                     DPTO + Time_FE, data = df_in_ven)
summary(feols_didreg4_informal_ven)

#####################
# DATASET:   FORMAL #
#####################

############
# LOG_WAGE #
############

feols_didreg_formal_ven = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                  DPTO +Time_FE, data = df_fo_ven)
summary(feols_didreg_formal_ven)


############
# THO_WAGE #
############

feols_didreg2_formal_ven = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO+Time_FE, data = df_fo_ven)

summary(feols_didreg2_formal_ven)

############
# HRS_WEEK #
############

feols_didreg3_formal_ven = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                   DPTO + Time_FE, data = df_fo_ven)

summary(feols_didreg3_formal_ven)

################
# LOG_HRS_WEEK #
################
feols_didreg4_formal_ven = feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                   DPTO + Time_FE, data = df_fo_ven)
summary(feols_didreg4_formal_ven)

