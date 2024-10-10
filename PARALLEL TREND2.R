rm(list=ls()) # clear workspace
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

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

#######################
#       EDIT          #
#     DATAFRAME       #
#######################

df<- read_dta("GEIH_REDUCED_2018_2023.dta")

##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df <- df %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

########################
#  CREATE ANO DUMMIES  #
########################

# Create a combined identifier
df$YearMonth <- paste(df$ANO, df$MES, sep = "_")
df$YearMonth<-factor(df$YearMonth)

# Generate dummy variables for each unique Year-Month combination
dummy_vars <- model.matrix(~ YearMonth - 1, data = df)

# Combine the dummy variables with the original data
data_with_dummies <- cbind(df, dummy_vars)

df2<-data_with_dummies

df2 <- df2 %>%
  rename(Y_M = YearMonth)

############################
#  CREATE quarter DUMMIES  #
############################
df2$QUARTER2018_1<-0
df2 <- df2 %>%
  mutate(QUARTER2018_1  = ifelse(Y_M == "2018_1"|Y_M == "2018_2"|Y_M == "2018_3", 1, QUARTER2018_1))

df2$QUARTER2018_2<-0
df2 <- df2 %>%
  mutate(QUARTER2018_2  = ifelse(Y_M == "2018_4"|Y_M == "2018_5"|Y_M == "2018_6", 1, QUARTER2018_2))

df2$QUARTER2018_3<-0
df2 <- df2 %>%
  mutate(QUARTER2018_3  = ifelse(Y_M == "2018_7"|Y_M == "2018_8"|Y_M == "2018_9", 1, QUARTER2018_3))

df2$QUARTER2018_4<-0
df2 <- df2 %>%
  mutate(QUARTER2018_4  = ifelse(Y_M == "2018_10"|Y_M == "2018_11"|Y_M == "2018_12", 1, QUARTER2018_4))

#2019

df2$QUARTER2019_1<-0
df2 <- df2 %>%
  mutate(QUARTER2019_1  = ifelse(Y_M == "2019_1"|Y_M == "2019_2"|Y_M == "2019_3", 1, QUARTER2019_1))

df2$QUARTER2019_2<-0
df2 <- df2 %>%
  mutate(QUARTER2019_2  = ifelse(Y_M == "2019_4"|Y_M == "2019_5"|Y_M == "2019_6", 1, QUARTER2019_2))

df2$QUARTER2019_3<-0
df2 <- df2 %>%
  mutate(QUARTER2019_3  = ifelse(Y_M == "2019_7"|Y_M == "2019_8"|Y_M == "2019_9", 1, QUARTER2019_3))

df2$QUARTER2019_4<-0
df2 <- df2 %>%
  mutate(QUARTER2019_4  = ifelse(Y_M == "2019_10"|Y_M == "2019_11"|Y_M == "2019_12", 1, QUARTER2019_4))

#2020

df2$QUARTER2020_1<-0
df2 <- df2 %>%
  mutate(QUARTER2020_1  = ifelse(Y_M == "2020_1"|Y_M == "2020_2"|Y_M == "2020_3", 1, QUARTER2020_1))

df2$QUARTER2020_2<-0
df2 <- df2 %>%
  mutate(QUARTER2020_2  = ifelse(Y_M == "2020_4"|Y_M == "2020_5"|Y_M == "2020_6", 1, QUARTER2020_2))

df2$QUARTER2020_3<-0
df2 <- df2 %>%
  mutate(QUARTER2020_3  = ifelse(Y_M == "2020_7"|Y_M == "2020_8"|Y_M == "2020_9", 1, QUARTER2020_3))

df2$QUARTER2020_4<-0
df2 <- df2 %>%
  mutate(QUARTER2020_4  = ifelse(Y_M == "2020_10"|Y_M == "2020_11"|Y_M == "2020_12", 1, QUARTER2020_4))

#2021

df2$QUARTER_ZERO<-0
#df2 <- df2 %>%
#  mutate(QUARTER2021_1  = ifelse(Y_M == "2021_1"|Y_M == "2021_2"|Y_M == "2021_3", 1, QUARTER2021_1))

df2$QUARTER2021_2<-0
df2 <- df2 %>%
  mutate(QUARTER2021_2  = ifelse(Y_M == "2021_4"|Y_M == "2021_5"|Y_M == "2021_6", 1, QUARTER2021_2))

df2$QUARTER2021_3<-0
df2 <- df2 %>%
  mutate(QUARTER2021_3  = ifelse(Y_M == "2021_7"|Y_M == "2021_8"|Y_M == "2021_9", 1, QUARTER2021_3))

df2$QUARTER2021_4<-0
df2 <- df2 %>%
  mutate(QUARTER2021_4  = ifelse(Y_M == "2021_10"|Y_M == "2021_11"|Y_M == "2021_12", 1, QUARTER2021_4))

#2022
df2$QUARTER2022_1<-0
df2 <- df2 %>%
  mutate(QUARTER2022_1  = ifelse(Y_M == "2022_1"|Y_M == "2022_2"|Y_M == "2022_3", 1, QUARTER2022_1))

df2$QUARTER2022_2<-0
df2 <- df2 %>%
  mutate(QUARTER2022_2  = ifelse(Y_M == "2022_4"|Y_M == "2022_5"|Y_M == "2022_6", 1, QUARTER2022_2))

df2$QUARTER2022_3<-0
df2 <- df2 %>%
  mutate(QUARTER2022_3  = ifelse(Y_M == "2022_7"|Y_M == "2022_8"|Y_M == "2022_9", 1, QUARTER2022_3))

df2$QUARTER2022_4<-0
df2 <- df2 %>%
  mutate(QUARTER2022_4  = ifelse(Y_M == "2022_10"|Y_M == "2022_11"|Y_M == "2022_12", 1, QUARTER2022_4))

#2023
df2$QUARTER2023_1<-0
df2 <- df2 %>%
  mutate(QUARTER2023_1  = ifelse(Y_M == "2023_1"|Y_M == "2023_2"|Y_M == "2023_3", 1, QUARTER2023_1))

df2$QUARTER2023_2<-0
df2 <- df2 %>%
  mutate(QUARTER2023_2  = ifelse(Y_M == "2023_4"|Y_M == "2023_5"|Y_M == "2023_6", 1, QUARTER2023_2))

df2$QUARTER2023_3<-0
df2 <- df2 %>%
  mutate(QUARTER2023_3  = ifelse(Y_M == "2023_7"|Y_M == "2023_8"|Y_M == "2023_9", 1, QUARTER2023_3))

df2$QUARTER2023_4<-0
df2 <- df2 %>%
  mutate(QUARTER2023_4  = ifelse(Y_M == "2023_10"|Y_M == "2023_11"|Y_M == "2023_12", 1, QUARTER2023_4))

######################################
#  CREATE QUARTER TREATMENT DUMMIES  #
######################################

# Identify columns that start with "QUARTER"
quarter_columns <- grep("^QUARTER", colnames(df2), value = TRUE)

for (col in quarter_columns) {
  interaction_term <- paste("DPTO", col, sep = "_")
  df2[[interaction_term]] <- df2$DPTO * df2[[col]]
}
dpto_quarter_columns <- grep("^DPTO_QUARTER", colnames(df2), value = TRUE)

# Print the names of the identified columns
print(dpto_quarter_columns)

df2_formal<- subset(df2, df2$FULL_FORMAL == 1)
df2_informal<- subset(df2, df2$FULL_INFORMAL == 1)

params<-c("DPTO_QUARTER2018_1", "DPTO_QUARTER2018_2", "DPTO_QUARTER2018_3", "DPTO_QUARTER2018_4", "DPTO_QUARTER2019_1", "DPTO_QUARTER2019_2", "DPTO_QUARTER2019_3", "DPTO_QUARTER2019_4",
          "DPTO_QUARTER2020_1", "DPTO_QUARTER2020_2", "DPTO_QUARTER2020_3", "DPTO_QUARTER2020_4", "DPTO_QUARTER2021_2", "DPTO_QUARTER2021_3", "DPTO_QUARTER2021_4",
            "DPTO_QUARTER2022_1","DPTO_QUARTER2022_2", "DPTO_QUARTER2022_3", "DPTO_QUARTER2022_4", "DPTO_QUARTER2023_1", "DPTO_QUARTER2023_2", "DPTO_QUARTER2023_3", "DPTO_QUARTER2023_4")

##########################################################
#                   FULL LABORFORCE                      #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel11 = lm(THO_MONTHLY_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2)
REGRESSION<-parrallel11

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(111)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(111)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FULL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel21 = lm(HRS_WRKD~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2)
REGRESSION<-parrallel21

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(121)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(121)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FULL_HRS_WRKD_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel31 = lm(LOG_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2)
REGRESSION<-parrallel31

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(131)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(131)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FULL_LOG_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")



##########################################################
#                  FORMAL LABORFORCE                     #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel1 = lm(THO_MONTHLY_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_formal)
REGRESSION<-parrallel1



###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(10)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(10)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FORMAL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel2 = lm(HRS_WRKD~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_formal)
REGRESSION<-parrallel2

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(101)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(101)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FORMAL_HRS_WRKD_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel3 = lm(LOG_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_formal)
REGRESSION<-parrallel3

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(102)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(102)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "FORMAL_LOG_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


############################################################
#                  INFORMAL LABORFORCE                     #
############################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel111 = lm(THO_MONTHLY_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_informal)
REGRESSION<-parrallel111

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(122)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(122)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "INFORMAL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel222 = lm(HRS_WRKD~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_informal)
REGRESSION<-parrallel222

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(133)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(133)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "INFORMAL_HRS_WRKD_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel333 = lm(LOG_WAGE~  DPTO_QUARTER2018_1+DPTO_QUARTER2018_2+DPTO_QUARTER2018_3+DPTO_QUARTER2018_4+DPTO_QUARTER2019_1+DPTO_QUARTER2019_2+DPTO_QUARTER2019_3+DPTO_QUARTER2019_4+
                  DPTO_QUARTER2020_1+DPTO_QUARTER2020_2+DPTO_QUARTER2020_3+DPTO_QUARTER2020_4+DPTO_QUARTER_ZERO+DPTO_QUARTER2021_2+DPTO_QUARTER2021_3+DPTO_QUARTER2021_4+
                  DPTO_QUARTER2022_1+DPTO_QUARTER2022_2+DPTO_QUARTER2022_3+DPTO_QUARTER2022_4+DPTO_QUARTER2023_1+DPTO_QUARTER2023_2+DPTO_QUARTER2023_3+DPTO_QUARTER2023_4+
                  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                  DPTO_AGR_ANO_2018+DPTO_AGR_ANO_2019+DPTO_AGR_ANO_2020+DPTO_AGR_ANO_2021+DPTO_AGR_ANO_2022+DPTO_AGR_ANO_2023+
                  DPTO_SERV_ANO_2018+DPTO_SERV_ANO_2019+DPTO_SERV_ANO_2020+DPTO_SERV_ANO_2021+DPTO_SERV_ANO_2022+DPTO_SERV_ANO_2023+
                  DPTO_IND_ANO_2018+DPTO_IND_ANO_2019+DPTO_IND_ANO_2020+DPTO_IND_ANO_2021+DPTO_IND_ANO_2022+DPTO_IND_ANO_2023+
                  DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                  DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                  DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                  DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+
                  as.factor(Time_FE)+as.factor(DPTO), data = df2_informal)
REGRESSION<-parrallel333
summary(REGRESSION)

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(144)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(144)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "INFORMAL_LOG_WAGE_DPTO_QUART_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")






