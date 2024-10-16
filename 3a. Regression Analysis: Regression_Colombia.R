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

#df_in_col$COVID<-ifelse(df_in_col$ANO=="2020" & df_in_col$MES<="3",1,0)
#df_fo_col$COVID<-ifelse(df_fo_col$ANO=="2020" & df_fo_col$MES<="3",1,0)

#df_in_col$EMPLOYED <- ifelse(df_in_col$EMPLOYED!=1,0,df_in_col$EMPLOYED)

#df_fo_col$EMPLOYED <- ifelse((df_fo_col$FULL_FORMAL==1|df_fo_col$FULL_INFORMAL==1)&(df_fo_col$LOG_WAGE>0 | df_fo_col$HRS_WRKD>0),1,0)
#df_fo_col$EMPLOYED <- ifelse(df_fo_col$EMPLOYED!=1,0,df_fo_col$EMPLOYED)

#df_in_col <- subset(df_in_col, TREATED == 1|UNTREATED ==1)
#df_fo_col <- subset(df_fo_col, TREATED == 1|UNTREATED ==1)
#df_fo_col <- subset(df_fo_col, ANO>="2019")
#df_in_col <- subset(df_in_col, ANO>="2019")


df_in_col <- df_in_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_fo_col <- df_fo_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))


params<-c("DID_2")

df_in_col$permit2 <- ifelse(df_in_col$ANO==2021 & df_in_col$MES>=10,1,0)
df_in_col$DID_2<-df_in_col$LOG_PTP * df_in_col$permit2

df_fo_col$permit2 <- ifelse(df_fo_col$ANO==2021 & df_fo_col$MES>=10,1,0)
df_fo_col$DID_2<-df_fo_col$LOG_PTP * df_fo_col$permit2
           
#####################
# DATASET: INFORMAL #
#####################

################
#  COLOMBIAN   #
################

#didreg_informal_EMP_COL = lm(EMPLOYED ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
#                           as.factor(Time_FE)+ as.factor(DPTO), data = df_in_col)

#REGRESSION<-didreg_informal_EMP_COL
#boot<-boottest(didreg_informal_EMP_COL, param="DID",B=999, clustid = "DPTO")
#print(boot)

#feols_didreg_informal_EMP_COL = f(EMPLOYED ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_in_col)
#summary(feols_didreg_informal_EMP_COL)


############
# LOG_WAGE #
############

didreg_informal_COL = lm(LOG_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_in_col)

REGRESSION<-didreg_informal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_informal_COL = feols(LOG_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_in_col)
summary(feols_didreg_informal_COL)
source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(2)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")


#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_INFORMAL_log_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_informal_COL = lm(THO_MONTHLY_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                          as.factor(Time_FE)+ as.factor(DPTO), data = df_in_col)


REGRESSION<-didreg2_informal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg2_informal_COL = feols(THO_MONTHLY_WAGE~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_in_col)
source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(3)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(3)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg3_informal_COL = lm(HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                          as.factor(Time_FE)+ as.factor(DPTO), data = df_in_col)


REGRESSION<-didreg3_informal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg3_informal_COL=feols(HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_in_col)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(4)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(4)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")


################
# LOG_HRS_WEEK #
################

didreg4_informal_COL = lm(LOG_HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_in_col)


REGRESSION<-didreg4_informal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_informal_COL=feols(LOG_HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_in_col)
summary(feols_didreg4_informal_COL)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(4)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(4)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_INFORMAL_LOG_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")



#####################
# DATASET:   FORMAL #
#####################

############
# LOG_WAGE #
############

didreg_formal_COL = lm(LOG_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                        as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_col)

REGRESSION<-didreg_formal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_formal_COL = feols(LOG_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                  DPTO + Time_FE, data = df_fo_col)
summary(feols_didreg_formal_COL)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(21)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(21)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_FORMAL_log_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_formal_COL = lm(THO_MONTHLY_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_col)

REGRESSION<-didreg2_formal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg2_formal_COL = feols(THO_MONTHLY_WAGE ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO+ Time_FE, data = df_fo_col)
source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(31)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(31)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg3_formal_COL = lm(HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                       as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_col)

REGRESSION<-didreg3_formal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg3_formal_COL = feols(HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                               DPTO + Time_FE, data = df_fo_col)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(41)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(41)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_formal_COL = lm(LOG_HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+ 
                          as.factor(Time_FE)+ as.factor(DPTO), data = df_fo_col)

REGRESSION<-didreg4_formal_COL

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_formal_COL = feols(LOG_HRS_WRKD ~ DID_2 + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                   DPTO + Time_FE, data = df_fo_col)
summary(feols_didreg4_formal_COL)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(41)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(41)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(merged_df, "Colombia_FORMAL_LOG_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

#DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
#  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
#  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
#  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
#  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
#  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
#  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
  
