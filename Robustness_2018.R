DF<-data.frame(
  ImmPop2018=c(98761,9965,207231,48963,8037,6731,734,4733,40241,11194,49527,749,2905,45739,44745,5775,5295,100613,5960,11652,45980,15995,5277,61338),
  DPTO=c(5,8,11,13,15,17,18,19,20,23,25,27,41,44,47,50,52,54,63,66,68,70,73,76)
)

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


merged_df <- merge(df2a,DF, by = "DPTO")
merged_df$POP18_ANO21 <-merged_df$ImmPop2018* merged_df$ANO_2021
merged_df$POP18_ANO22 <-merged_df$ImmPop2018* merged_df$ANO_2022
merged_df$POP18_ANO23 <-merged_df$ImmPop2018* merged_df$ANO_2023

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df2a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                     POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                   |DPTO+Time_FE, data = merged_df)
summary(feols_df2a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                POP18_ANO21 + POP18_ANO22 + POP18_ANO23 +
                as.factor(Time_FE)+ as.factor(DPTO), data = merged_df)

REGRESSION<-didreg_2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(0123)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(0123)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#did DID PVALUE 0.5995996 -72.30682 144.9778 21.3619 0.6770379




feols_df2a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                       POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                     |DPTO+Time_FE, data = merged_df)
summary(feols_df2a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_2b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                 POP18_ANO21 + POP18_ANO22 + POP18_ANO23 +
                 as.factor(Time_FE)+ as.factor(DPTO), data = merged_df)

REGRESSION<-didreg_2b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID Pvalue:0.05705706 -0.8573962 0.01259585 -0.5179234 -3.759891

#####################
# DATASET: FORMAL #
#####################

################
#  COLOMBIAN   #
################
df4a <- read_dta("Colombian_formal.dta")

df4a <- merge(df4a,DF, by = "DPTO")

df4a$POP18_ANO21 <-df4a$ImmPop2018* df4a$ANO_2021
df4a$POP18_ANO22 <-df4a$ImmPop2018* df4a$ANO_2022
df4a$POP18_ANO23 <-df4a$ImmPop2018* df4a$ANO_2023

df4a<-df4a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))
###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df4a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                     POP18_ANO21 + POP18_ANO22 + POP18_ANO23 
                   |DPTO+Time_FE, data = df4a)
summary(feols_df4a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                POP18_ANO21 + POP18_ANO22 + POP18_ANO23 +
                as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.07507508 conflow-50.07426 confhigh 2.978924 estimate-25.09439 tstat-2.384119

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df4a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                       POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                     |DPTO+Time_FE, data = df4a)
summary(feols_df4a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_4b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 POP18_ANO21 + POP18_ANO22 + POP18_ANO23+
                 as.factor(Time_FE)+ as.factor(DPTO), data = df4a)

REGRESSION<-didreg_4b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#DID pvalue: 0.4634635 conflow : -0.6373932, confhigh : 0.3767021 estimate : -0.2307123 tstat -1.115732

#####################
# DATASET: INFORMAL #
#####################

################
#  VENEZUELAN  #
################
df3a <- read_dta("Migrant_informal.dta")

df3a <- merge(df3a,DF, by = "DPTO")

df3a$POP18_ANO21 <-df3a$ImmPop2018* df3a$ANO_2021
df3a$POP18_ANO22 <-df3a$ImmPop2018* df3a$ANO_2022
df3a$POP18_ANO23 <-df3a$ImmPop2018* df3a$ANO_2023


df3a<-df3a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df3a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                     POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                   |DPTO+Time_FE, data = df3a)
summary(feols_df3a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                POP18_ANO21 + POP18_ANO22 + POP18_ANO23+
                as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue : 0.8688689 conflow : -59.99427 confhigh : 56.27368 estimate: -5.614962 tstat : -0.2582896

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df3a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                       POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                     |DPTO+Time_FE, data = df3a)
summary(feols_df3a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_3b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 POP18_ANO21 + POP18_ANO22 + POP18_ANO23+
                 as.factor(Time_FE)+ as.factor(DPTO), data = df3a)

REGRESSION<-didreg_3b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(12345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(12345678)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

# DID pvalue: 0.04404404 # conflow : -1.165783 confhigh : 0.02113334 estimate : -0.6116355 tstat : -2.478658

#####################
# DATASET: FORMAL #
#####################

################
#  VENEZUELAN  #
################
df5a <- read_dta("Migrant_formal.dta")

df5a <- merge(df5a,DF, by = "DPTO")

df5a$POP18_ANO21 <-df5a$ImmPop2018* df5a$ANO_2021
df5a$POP18_ANO22 <-df5a$ImmPop2018* df5a$ANO_2022
df5a$POP18_ANO23 <-df5a$ImmPop2018* df5a$ANO_2023


df5a<-df5a%>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_df5a = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                     covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                     POP18_ANO21 + POP18_ANO22 + POP18_ANO23
                   |DPTO+Time_FE, data = df5a)
summary(feols_df5a)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5 = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                POP18_ANO21 + POP18_ANO22 + POP18_ANO23+ +
                as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123456789)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	DID pvalue 0.04304304 conflow : -393.4347 confhigh : 3.916099 estimate : -204.4157 tsat: -2.498607

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

feols_df5a_b = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                       POP18_ANO21 + POP18_ANO22 + POP18_ANO23 
                     |DPTO+Time_FE, data = df5a)
summary(feols_df5a_b)

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
didreg_5b = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                 POP18_ANO21 + POP18_ANO22 + POP18_ANO23+
                 as.factor(Time_FE)+ as.factor(DPTO), data = df5a)

REGRESSION<-didreg_5b

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(1234567899)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(1234567899)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID pvalue 0.7527528, conflow: -1.106728, confhigh : 1.227451 estiamte : -0.1586099 tsat: 0.3386153
