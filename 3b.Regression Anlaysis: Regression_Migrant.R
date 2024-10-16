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

df_in_ven <- read_dta("Migrant_informal.dta")
df_fo_ven <- read_dta("Migrant_formal.dta")


##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df_in_ven <- df_in_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_fo_ven <- df_fo_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_in_ven$COVID<-ifelse(df_in_ven$ANO=="2020" & df_in_ven$MES<="3",1,0)
df_fo_ven$COVID<-ifelse(df_fo_ven$ANO=="2020" & df_fo_ven$MES<="3",1,0)

params<-c("DID") 
#          "DPTO_AGR_ANO_2018","DPTO_AGR_ANO_2019","DPTO_AGR_ANO_2020","DPTO_AGR_ANO_2021","DPTO_AGR_ANO_2022","DPTO_AGR_ANO_2023",
#          "DPTO_SERV_ANO_2018","DPTO_SERV_ANO_2019","DPTO_SERV_ANO_2020","DPTO_SERV_ANO_2021","DPTO_SERV_ANO_2022","DPTO_SERV_ANO_2023",
#          "DPTO_IND_ANO_2018","DPTO_IND_ANO_2019","DPTO_IND_ANO_2020","DPTO_IND_ANO_2021","DPTO_IND_ANO_2022","DPTO_IND_ANO_2023",
#          "DPTO_GINI_ANO_2018","DPTO_GINI_ANO_2019","DPTO_GINI_ANO_2020","DPTO_GINI_ANO_2021","DPTO_GINI_ANO_2022","DPTO_GINI_ANO_2023",
#          "DPTO_HOMIC_ANO_2018","DPTO_HOMIC_ANO_2019","DPTO_HOMIC_ANO_2020","DPTO_HOMIC_ANO_2021","DPTO_HOMIC_ANO_2022","DPTO_HOMIC_ANO_2023",
 #         "DPTO_UNSAT_ANO_2018","DPTO_UNSAT_ANO_2019","DPTO_UNSAT_ANO_2020","DPTO_UNSAT_ANO_2021","DPTO_UNSAT_ANO_2022","DPTO_UNSAT_ANO_2023",
 #         "DPTO_TERROR_ANO_2018","DPTO_TERROR_ANO_2019","DPTO_TERROR_ANO_2020","DPTO_TERROR_ANO_2021", "DPTO_TERROR_ANO_2022","DPTO_TERROR_ANO_2023")

#df_in_ven <- subset(df_in_ven, TREATED == 1|UNTREATED ==1)
#df_fo_ven <- subset(df_fo_ven, TREATED == 1|UNTREATED ==1)
#df_fo_ven <- subset(df_fo_ven, ANO>="2019")
#df_in_ven <- subset(df_in_ven, ANO>="2019")

#####################
# DATASET: INFORMAL #
#####################

#########################
#  VENEZUELAN MIGRANT   #
#########################

############
# LOG_WAGE #
############

didreg_informal_ven = lm(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                        as.factor(Time_FE)+ as.factor(DPTO), data = df_in_ven)


REGRESSION<-didreg_informal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_informal_ven = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                 DPTO+Time_FE, data = df_in_ven)
summary(feols_didreg_informal_ven)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(5)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(5)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_INFORMAL_log_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_informal_ven = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_in_ven)

REGRESSION<-didreg2_informal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg2_informal_ven = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                  DPTO + Time_FE, data = df_in_ven)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")


#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(6)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(6)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")


############
# HRS_WEEK #
############

didreg3_informal_ven = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_in_ven)


REGRESSION<-didreg3_informal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################


feols_didreg3_informal_ven = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                            DPTO + Time_FE, data = df_in_ven)


source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(7)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(7)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# log_HRS_WEEK #
################

didreg4_informal_ven = lm(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_in_ven)


REGRESSION<-didreg4_informal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################


feols_didreg4_informal_ven = feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                     DPTO + Time_FE, data = df_in_ven)
summary(feols_didreg4_informal_ven)


source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(79)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(79)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_INFORMAL_LOG_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

#####################
# DATASET:   FORMAL #
#####################

############
# LOG_WAGE #
############

didreg_formal_ven = lm(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_ven)


REGRESSION<-didreg_formal_ven
###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_formal_ven = feols(LOG_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                         DPTO +Time_FE, data = df_fo_ven)
summary(feols_didreg_formal_ven)


source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")


#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(51)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(51)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_FORMAL_log_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_formal_ven = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  
                        as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_ven)

REGRESSION<-didreg2_formal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg2_formal_ven = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO+Time_FE, data = df_fo_ven)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")


#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(61)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(61)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg3_formal_ven = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_ven)

REGRESSION<-didreg3_formal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg3_formal_ven = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                DPTO + Time_FE, data = df_fo_ven)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")


#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(71)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(71)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_formal_ven = lm(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                          as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_ven)

REGRESSION<-didreg4_formal_ven

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_formal_ven = feols(LOG_HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |
                                   DPTO + Time_FE, data = df_fo_ven)
summary(feols_didreg4_formal_ven)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")


#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(719)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(719)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Migrant_FORMAL_LOG_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")
