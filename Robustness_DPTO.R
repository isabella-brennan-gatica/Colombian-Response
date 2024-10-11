library(fixest)
library(fwildclusterboot)

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
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                     
                   |DPTO+Time_FE, data = df2a)
summary(feols_df2a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
               
                as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(0123)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(0123)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#did pvalue:0.967968 estimate 1.376403 tstat:0.03754547

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df2a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                       
                     |DPTO+Time_FE, data = df2a)
summary(feols_df2a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                 DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                 DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                
                 as.factor(Time_FE)+ as.factor(DPTO), data = df2a)

REGRESSION<-didreg_2b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID Pvalue: 0.03803804, Conflow:-0.6665513 Conf:high-0.03333268 estimate-0.4756373 statistic:-4.26009


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
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                     
                   |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
               
                as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.1391391 conflow-107.2133 confhigh21.15412 estimate-43.88878 tstat-2.001407

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df4a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                       
                     |DPTO+Time_FE, data = df4a)
summary(feols_df4a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                 DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                 DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                 DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                 DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                 DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                
                 as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID pvalue: 0.6156156 conflow : -0.6233043, confhigh : 0.4598751 estimate : -0.1376323 tstat -0.6048146


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
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                     
                   |DPTO+Time_FE, data = df3a)
summary(feols_df3a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
               
                as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue : 0.5265265 conflow : -63.12884 confhigh : 67.00279 estimate: -25.895 tstat : -1.360627

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df3a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                       
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
                 DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                 DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                 DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                
                 as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345678)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

# DID pvalue: 0.07607608 # conflow : -1.197446 confhigh : 0.1270711 estimate : -0.4868422 tstat : -2.562184


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
                     DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                     DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                     DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                     DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                     DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                     DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                     
                   |DPTO+Time_FE, data = df5a)
summary(feols_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
               
                as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-feols_df5a
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456789)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.1761762 conflow : -403.5744 confhigh : 101.9802 estimate : -233.5169 tsat: -2.945767

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df5a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                       DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                       DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                       DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                       DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                       DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 
                      
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
                 DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                 DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                 DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567899)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567899)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue 0.6626627, conflow: -1.280291, confhigh : 1.04387 estiamte : -0.2505969 tsat: -0.6260964


