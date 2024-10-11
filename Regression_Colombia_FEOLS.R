#install.packages("purrr")
#install.packages("openxlsx")
#install.packages("Hmisc")
#install.packages("fastDummies")

rm(list=ls()) # clear workspace
setwd("C:/Users/.../Colombia/GEIH DATA/")

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

df_in_col <- read_dta("Colombian_informal.dta")
df_fo_col <- read_dta("Colombian_formal.dta")

df_in_col <- df_in_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_fo_col <- df_fo_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


params<-c("DID")

#####################
# DATASET: INFORMAL #
#####################

################
#  COLOMBIAN   #
################

############
# LOG_WAGE #
############

feols_didreg_informal_COL = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_in_col)
summary(feols_didreg_informal_COL)

############
# THO_WAGE #
############

feols_didreg2_informal_COL = feols(THO_MONTHLY_WAGE~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_in_col)
summary(feols_didreg2_informal_COL)

############
# HRS_WEEK #
############

feols_didreg3_informal_COL=feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_in_col)

summary(feols_didreg3_informal_COL)

################
# LOG_HRS_WEEK #
################
feols_didreg4_informal_COL=feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_in_col)
summary(feols_didreg4_informal_COL)

#####################
# DATASET:   FORMAL #
#####################

############
# LOG_WAGE #
############

feols_didreg_formal_COL = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                  DPTO + Time_FE, data = df_fo_col)
summary(feols_didreg_formal_COL)

############
# THO_WAGE #
############

feols_didreg2_formal_COL = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO+ Time_FE, data = df_fo_col)

summary(feols_didreg2_formal_COL)

############
# HRS_WEEK #
############

feols_didreg3_formal_COL = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_fo_col)

summary(feols_didreg3_formal_COL)

################
# LOG_HRS_WEEK #
################
feols_didreg4_formal_COL = feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_fo_col)
summary(feols_didreg4_formal_COL)
# Extracting coefficients for one covariate (e.g., "wt")


