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
                    DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                    DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                    DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                    DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                    DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                    DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                    DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                    pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                  |DPTO+Time_FE, data = df2a)
summary(feols_df2a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(0123)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(0123)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#term pvalue    conflow  confhigh estimate statistic
#DID 0.6546547 -79.17926 149.664 18.10424 0.4705731




###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df2a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                      covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                      DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                      DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                      DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                      DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                      DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                      DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                      DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                      pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                    |DPTO+Time_FE, data = df2a)
summary(feols_df2a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                 DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                 DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                 DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                 pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#term pvalue    conflow     confhigh    estimate statistic
#DID 0.06506507 -0.6503576 0.02342037 -0.4419497 -3.59245

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
feols_df4a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                    DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                    DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                    DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                    DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                    DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                    DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                    DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                    pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                  |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +  
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#term pvalue    conflow     confhigh    estimate statistic
#DID 0.1501502 -113.7847    16.15377   -35.63763 -1.502588

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df4a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                      covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                      DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                      DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                      DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                      DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                      DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                      DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                      DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                      pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                    |DPTO+Time_FE, data = df4a)
summary(feols_df4a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                 DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                 DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                 DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                 pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#term pvalue    conflow     confhigh    estimate statistic
#DID 0.6666667 -0.6175578 0.4776944 -0.1222965 -0.5026665


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
                    DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                    DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                    DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                    DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                    DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                    DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                    DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                    pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                  |DPTO+Time_FE, data = df3a)
summary(feols_df3a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#term pvalue    conflow     confhigh    estimate statistic
#DID 0.5965966 -65.87617 63.00942 -26.945 -1.232042

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df3a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                      
                      DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                      DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                      DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                      DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                      DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                      DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                      DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                      pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                    |DPTO+Time_FE, data = df3a)
summary(feols_df3a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                 DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                 DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                 DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                 pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023+
                 as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345678)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#term pvalue    conflow     confhigh    estimate statistic

# DID 0.05805806 -1.238066 0.0464104 -0.5363724 -2.698284


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
                    DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                    DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                    DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                    DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                    DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                    DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                    DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                    pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                  |DPTO+Time_FE, data = df5a)
summary(feols_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456789)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#term pvalue    conflow     confhigh    estimate statistic
#	DID 0.1001001,  -428.2645 50.16549 -240.3792 -2.523732

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df5a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     
                      DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                      DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                      DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                      DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                      DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                      DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                      DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                      pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023
                    |DPTO+Time_FE, data = df5a)
summary(feols_df5a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                 DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                 DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                 DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                 pop_vene_1993_ANO_2021 + pop_vene_1993_ANO_2022 + pop_vene_1993_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567899)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567899)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue 0.978979 estimate 0.01358313  tvalue 0.02878072


