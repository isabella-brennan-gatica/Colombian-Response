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

df2a<-df2a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

params<-c("DID")


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df2a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                    pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                    |DPTO+Time_FE, data = df2a)
summary(feols_df2a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(0123)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(0123)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#did pvalue:0.4764765 conflow -47.59214 confhigh 132.8111 estimate 24.39231 tstat:0.8471967

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df2a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                      covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                      pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                    |DPTO+Time_FE, data = df2a)
summary(feols_df2a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID Pvalue: 0.07507508, Conflow:-0.6382661 Conf:high 0.03918482 estimate-0.4236187 statistic:-3.504512

#####################
# DATASET: FORMAL #
#####################

################
#  COLOMBIAN   #
################
df4a <- read_dta("Colombian_formal.dta")

df4a<-df4a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df4a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  
                   covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                     pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                  |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                  pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023+
                as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.06106106 conflow-56.40868 confhigh 1.590812 estimate-26.75696 tstat-2.149069

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df4a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                      covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                      pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                    |DPTO+Time_FE, data = df4a)
summary(feols_df4a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID pvalue: 0.7867868 conflow : -0.614604, confhigh : 0.5286942 estimate : -0.08710002 tstat -0.3426725

#####################
# DATASET: INFORMAL #
#####################

################
#  VENEZUELAN  #
################
df3a <- read_dta("Migrant_informal.dta")

df3a<-df3a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df3a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                    pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                  |DPTO+Time_FE, data = df3a)
summary(feols_df3a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue : 0.8708709 conflow : -63.66005 confhigh : 65.25054 estimate: -14.85384 tstat : -0.6328171

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df3a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                       pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 
                    |DPTO+Time_FE, data = df3a)
summary(feols_df3a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345678)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

# DID pvalue: 0.1051051 # conflow : -1.224539 confhigh : 0.1615181 estimate : -0.5207828 tstat : -2.158289


#####################
# DATASET: FORMAL #
#####################

################
#  VENEZUELAN  #
################
df5a <- read_dta("Migrant_formal.dta")

df5a<-df5a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df5a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                    pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023
                  |DPTO+Time_FE, data = df5a)
summary(feols_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456789)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.1491491 conflow : -443.4727 confhigh : 70.88418 estimate : -219.2187 tsat: -2.196921


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df5a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                       pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023  
                    |DPTO+Time_FE, data = df5a)
summary(feols_df5a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 pop_vene_2005_ANO_2021 + pop_vene_2005_ANO_2022 + pop_vene_2005_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567899)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567899)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue 0.3863864, conflow: -1.035957, confhigh : 1.52098 estiamte : -0.5138606 tsat: -1.103166




