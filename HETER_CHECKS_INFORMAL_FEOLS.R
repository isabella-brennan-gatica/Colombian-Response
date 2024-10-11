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

df_in_ven <- read_dta("Migrant_informal.dta")


##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df_in_col <- df_in_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_in_ven <- df_in_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

params<-c("DID")


#######################
# DATASET:  EDUCATION #
#######################

#Primary 6
df_prim <- subset(df_in_col, EDUC<11)
#Highschool 11<14
df_high <- subset(df_in_col, EDUC==11|EDUC==12|EDUC==13)
#Highereducation 14,15
df_uni <- subset(df_in_col, EDUC==14|EDUC==15)
#elite 17
df_mas <- subset(df_in_col, EDUC==17)

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_in_col, GENDER==0)

###############################################################
#                       INFORMAL COLOMBIAN                    #
###############################################################

#####################
# DATASET:  PRIMARY #
#####################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_prim = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                          covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_prim)
summary(feols_didreg_prim)


############
# HRS_WEEK #
############

feols_didreg4_didreg_prim = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_prim)
summary(feols_didreg4_didreg_prim)


########################
# DATASET:  HIGHSCHOOL #
########################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                          covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_high)
summary(feols_didreg_HIGH)


################
# LOG_HRS_WEEK #
################

feols_didreg4_didreg_HIGH = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +
                                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_high)
summary(feols_didreg4_didreg_HIGH)

########################
# DATASET: HIGHER EDUC #
########################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_HIGHER = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +
                              covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                              DPTO + Time_FE, data = df_uni)
summary(feols_didreg_HIGHER)


################
# LOG_HRS_WEEK #
################

feols_didreg4_didreg_HIGHER = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                                      covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                      DPTO + Time_FE, data = df_uni)
summary(feols_didreg4_didreg_HIGHER)

########################
# DATASET: ELITE EDUC  #
########################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_ELITE = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                           covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                             DPTO + Time_FE, data = df_mas)
summary(feols_didreg_ELITE)


################
# LOG_HRS_WEEK #
################

feols_didreg4_didreg_ELITE = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                                   covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                                     DPTO + Time_FE, data = df_mas)
summary(feols_didreg4_didreg_ELITE)


##################################
#              WOMEN             #       
##################################


####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                             covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                             DPTO + Time_FE, data = df_female)
summary(feols_didreg_WOMEN)

################
# LOG_HRS_WEEK #
################

feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                     DPTO + Time_FE, data = df_female)
summary(feols_didreg4_didreg_WOMEN)

#######################
# DATASET:  EDUCATION #
#######################

#HIGH OR LESS
df_high <- subset(df_in_ven, EDUC<14)
#University or more
df_uni <- subset(df_in_ven, EDUC>=14)

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_in_ven, GENDER==0)


########################
# DATASET:  HIGHSCHOOL #
########################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_high)
summary(feols_didreg_HIGH)

############
# HRS_WEEK #
############

feols_didreg4_didreg_HIGH = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_high)
summary(feols_didreg4_didreg_HIGH)

########################
# DATASET: uNIVERSITY EDUC #
########################

####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_HIGHER = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                              DPTO + Time_FE, data = df_uni)
summary(feols_didreg_HIGHER)


############
# HRS_WEEK #
############

feols_didreg4_didreg_HIGHER = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                                      DPTO + Time_FE, data = df_uni)
summary(feols_didreg4_didreg_HIGHER)


##################################
#              WOMEN             #       
##################################


####################
# THO_MONTHLY_WAGE #
####################

feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                             DPTO + Time_FE, data = df_female)
summary(feols_didreg_WOMEN)

############
# HRS_WEEK #
############

feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                                     DPTO + Time_FE, data = df_female)
summary(feols_didreg4_didreg_WOMEN)


