install.packages("AER")  # If not already installed
library(AER)
library(sandwich)
library(lmtest)
library(boot)

# Run the 2SLS regression
#create instrument 2005 
df2$instr_05<- df2$it3 * df2$Policy

feols_df2 = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  
                   |DPTO+Time_FE, data = df2)
summary(feols_df2)
boot<-boottest(feols_df2, param="DID", B=999, clustid="DPTO")
print(boot)
#p value: 0.7608 
#confidence interval: -0.0112 0.0108 
#test statistic -0.3668 
#estimate  -1.227460e-03
#std error 0.003346518

iv_model_2 <- ivreg(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  +
                       as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df2)

# Cluster-robust standard errors
cluster_se <- vcovCL(iv_model_2, cluster = ~DPTO)

# First-stage F-statistic
ftest <- coeftest(iv_model_2, vcov. = cluster_se)
print(ftest)
# 4.7789e-03
#DID                        1.8639e-03  5.4982e-03    0.3390 0.7346111 
#####################
# DATASET: INFORMAL #
#####################

################
#  COLOMBIAN   #
################

#create instrument 2005 
df2a$instr_05<- df2a$it3 * df2a$Policy

feols_df2a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df2a)
summary(feols_df2a)

# Load the required package

# Assuming `data` is your dataframe containing all the variables
iv_model_2a <- ivreg(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                    covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                    as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df2a)

# Summary of the model
summary(iv_model_2a)

# Cluster-robust standard errors
cluster_se <- vcovCL(iv_model_2a, cluster = ~DPTO)

# First-stage F-statistic
ftest <- coeftest(iv_model_2a, vcov. = cluster_se)
print(ftest)

#####################
# DATASET: FORMAL #
#####################

################
#  COLOMBIAN   #
################

df4a <- read_dta("Colombian_formal.dta")

df4a<-df4a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df4a$instr_05<- df4a$it3 * df4a$Policy
df4a$instr_93<- df4a$it2 * df4a$Policy
#################################################################################################
#                                           WAGE                                                #
#################################################################################################
###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df4a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
iv_model_4a <- ivreg(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                       as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_93, data = df4a)
# Summary of the model with first stage f test
summary_4a<- summary(iv_model_4a, diagnostics = TRUE)

print(summary_4a)

# Cluster-robust standard errors
cluster_se <- vcovCL(iv_model_4a, cluster = ~DPTO)

# First-stage F-statistic
ftest <- coeftest(iv_model_4a, vcov. = cluster_se)
print(ftest)

#################################################################################################
#                                           HOURS                                               #
#################################################################################################
###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_HRS_df4a = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                   |DPTO+Time_FE, data = df4a)
summary(feols_HRS_df4a)

boottest(feols_HRS_df4a, parma="DID", B=999, clustid="DPTO")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
iv_model_HRS_4a <- ivreg(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                       as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df4a)
# Summary of the model with first stage f test
summary_HRS_4a<- summary(iv_model_HRS_4a, diagnostics = TRUE)

print(summary_HRS_4a)

# Cluster-robust standard errors
cluster_se_HRS <- vcovCL(iv_model_HRS_4a, cluster = ~DPTO)

# First-stage F-statistic
ftest_HRS <- coeftest(iv_model_HRS_4a, vcov. = cluster_se_HRS)
print(ftest_HRS)


#####################
# DATASET: INFORMAL #
#####################

################
#  VENEZUELAN  #
################

df3a <- read_dta("Migrant_informal.dta")

df3a<-df3a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df3a$instr_05<- df3a$it3 * df3a$Policy

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
iv_model_3a <- ivreg(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                       as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df3a)
# Summary of the model with first stage f test
summary_3a<- summary(iv_model_3a, diagnostics = TRUE)

print(summary_3a)

# Cluster-robust standard errors
cluster_se <- vcovCL(iv_model_3a, cluster = ~DPTO)

# First-stage F-statistic
ftest <- coeftest(iv_model_3a, vcov. = cluster_se)
print(ftest)

#####################
# DATASET: FORMAL #
#####################

################
#  VENEZUELAN  #
################
df5a <- read_dta("Migrant_formal.dta")

df5a<-df5a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df5a$instr_05<- df5a$it3 * df5a$Policy
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
iv_model_5a <- ivreg(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                       as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df5a)
# Summary of the model with first stage f test
summary_5a<- summary(iv_model_3a, diagnostics = TRUE)

print(summary_5a)

# Cluster-robust standard errors
cluster_se <- vcovCL(iv_model_5a, cluster = ~DPTO)

# First-stage F-statistic
ftest <- coeftest(iv_model_5a, vcov. = cluster_se)
print(ftest)

#################################################################################################
#                                           HOURS                                               #
#################################################################################################
###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_HRS_df5a = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                         DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                         DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                         DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                         DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                         DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                         DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                         covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
                       |DPTO+Time_FE, data = df5a)
summary(feols_HRS_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
iv_model_HRS_5a <- ivreg(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                           DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                           DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                           DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                           DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                           DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                           DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                           covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                           as.factor(DPTO) + as.factor(Time_FE) | . - DID + instr_05, data = df5a)
# Summary of the model with first stage f test
summary_HRS_5a<- summary(iv_model_HRS_5a, diagnostics = TRUE)

print(summary_HRS_5a)

# Cluster-robust standard errors
cluster_se_HRS <- vcovCL(iv_model_HRS_5a, cluster = ~DPTO)

# First-stage F-statistic
ftest_HRS <- coeftest(iv_model_HRS_5a, vcov. = cluster_se_HRS)
print(ftest_HRS)


