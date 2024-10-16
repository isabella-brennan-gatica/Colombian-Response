#EXCLUDE BORDER: 54,20,44

#interact with year dummy ANO_2021, ANO_2022, ANO_2023
library(fixest)
library(fwildclusterboot)
library(dplyr)
library(haven)

#####################
# DATASET: INFORMAL #
#####################

################
#  COLOMBIAN   #
################
df2a <- read_dta("Colombian_informal.dta")

df2a <- subset(df2a, DPTO != 54)
df2a <- subset(df2a, DPTO != 20)
df2a <- subset(df2a, DPTO != 44)

df2a<-df2a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

params<-c("DID")


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df2a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df2a)
summary(feols_df2a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(0123)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(0123)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#did pvalue:0.3213213 conflow -41.94633 confhigh 116.9012 estimate 26.39131 tstat:1.028471

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df2a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                     |DPTO+Time_FE, data = df2a)
summary(feols_df2a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID Pvalue: 0.03703704, Conflow:-0.8841664 Conf:high -0.0375692 estimate-0.5406794 statistic:-3.620389

#####################
# DATASET: FORMAL #
#####################

################
#  COLOMBIAN   #
################

df4a <- read_dta("Colombian_formal.dta")

df4a <- subset(df4a, DPTO != 54)
df4a <- subset(df4a, DPTO != 20)
df4a <- subset(df4a, DPTO != 44)

df4a<-df4a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df4a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.0970971 conflow-63.51621 confhigh 8.320491 estimate-27.51926 tstat-2.031119

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df4a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                     |DPTO+Time_FE, data = df4a)
summary(feols_df4a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID pvalue: 0.3813814 conflow : -0.6971223, confhigh : 0.359036 estimate : -0.2854958 tstat -1.30518

#####################
# DATASET: INFORMAL #
#####################

################
#  VENEZUELAN  #
################
df3a <- read_dta("Migrant_informal.dta")

df3a <- subset(df3a, DPTO != 54)
df3a <- subset(df3a, DPTO != 20)
df3a <- subset(df3a, DPTO != 44)


df3a<-df3a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df3a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  
                   |DPTO+Time_FE, data = df3a)
summary(feols_df3a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue : 0.8328328 conflow : -39.0891 confhigh : 59.08853 estimate: 4.999863 tstat : 0.2468634

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df3a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                     |DPTO+Time_FE, data = df3a)
summary(feols_df3a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345678)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

# DID pvalue: 0.1111111 # conflow : -1.738011 confhigh : 0.2462553 estimate : -0.7973082 tstat : -1.983266

#####################
# DATASET: FORMAL #
#####################

################
#  VENEZUELAN  #
################
df5a <- read_dta("Migrant_formal.dta")

df5a <- subset(df5a, DPTO != 54)
df5a <- subset(df5a, DPTO != 20)
df5a <- subset(df5a, DPTO != 44)

df5a<-df5a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df5a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df5a)
summary(feols_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456789)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.1041041 conflow : -476.7174 confhigh : 30.97591 estimate : -240.1289 tsat: -2.456237

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df5a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  
                     |DPTO+Time_FE, data = df5a)
summary(feols_df5a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567899)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567899)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue 0.967968, conflow: -1.54285, confhigh : 1.358158 estiamte : -0.03767598 tsat: -0.06803462



