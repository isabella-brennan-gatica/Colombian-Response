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
df$MES <- sprintf("%02d", df$MES)
##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df <- df %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

########################
#  CREATE ANO DUMMIES  #
########################
df$MES <- as.numeric(df$MES)
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
df2$YearMonth2021_3<-0

######################################
#  CREATE ANO_MES TREATMENT DUMMIES  #
######################################

# Identify columns that start with "QUARTER"
quarter_columns <- grep("^YearMonth", colnames(df2), value = TRUE)

for (col in quarter_columns) {
  interaction_term <- paste("PTP", col, sep = "_")
  df2[[interaction_term]] <- df2$LOG_PTP* df2[[col]]
}
PTP_YearMonth_columns <- grep("^PTP_YearMonth", colnames(df2), value = TRUE)

# Print the names of the identified columns
print(PTP_YearMonth_columns)

df2_formal<- subset(df2, df2$FULL_FORMAL == 1)
df2_informal<- subset(df2, df2$FULL_INFORMAL == 1)

params<-c(  "PTP_YearMonth2018_1",  "PTP_YearMonth2018_10", "PTP_YearMonth2018_11","PTP_YearMonth2018_12", "PTP_YearMonth2018_2", 
            "PTP_YearMonth2018_3",  "PTP_YearMonth2018_4","PTP_YearMonth2018_5","PTP_YearMonth2018_6" , "PTP_YearMonth2018_7", 
            "PTP_YearMonth2018_8" , "PTP_YearMonth2018_9",  "PTP_YearMonth2019_1","PTP_YearMonth2019_10", "PTP_YearMonth2019_11",
            "PTP_YearMonth2019_12", "PTP_YearMonth2019_2",  "PTP_YearMonth2019_3","PTP_YearMonth2019_4",  "PTP_YearMonth2019_5", 
            "PTP_YearMonth2019_6" , "PTP_YearMonth2019_7",  "PTP_YearMonth2019_8","PTP_YearMonth2019_9",  "PTP_YearMonth2020_1", 
            "PTP_YearMonth2020_10", "PTP_YearMonth2020_11", "PTP_YearMonth2020_12", "PTP_YearMonth2020_2",  "PTP_YearMonth2020_3", 
            "PTP_YearMonth2020_4" , "PTP_YearMonth2020_5",  "PTP_YearMonth2020_6","PTP_YearMonth2020_7",  "PTP_YearMonth2020_8", 
            "PTP_YearMonth2020_9",  "PTP_YearMonth2021_1",  "PTP_YearMonth2021_10","PTP_YearMonth2021_11", "PTP_YearMonth2021_12",
            "PTP_YearMonth2021_2",   "PTP_YearMonth2021_4","PTP_YearMonth2021_5",  "PTP_YearMonth2021_6", 
            "PTP_YearMonth2021_7",  "PTP_YearMonth2021_8" , "PTP_YearMonth2021_9", "PTP_YearMonth2022_1",  "PTP_YearMonth2022_10",
            "PTP_YearMonth2022_11", "PTP_YearMonth2022_12", "PTP_YearMonth2022_2","PTP_YearMonth2022_3",  "PTP_YearMonth2022_4",
            "PTP_YearMonth2022_5",  "PTP_YearMonth2022_6",  "PTP_YearMonth2022_7","PTP_YearMonth2022_8",  "PTP_YearMonth2022_9")

##########################################################
#                   full/ LABORFORCE                      #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel11 = lm(THO_MONTHLY_WAGE~PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                   PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                   PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                   PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                   PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                   PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                   PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                   PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                   PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                   PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                   PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                   PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                   PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                   PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                   PTP_YearMonth2023_8+PTP_YearMonth2023_9+  
                   AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                   as.factor(Time_FE)+as.factor(DPTO), data = df2)
REGRESSION<-parrallel11
summary(REGRESSION)
boot<- boottest(REGRESSION, clustid = "DPTO", param = "PTP_YearMonth2020_2", B = 999, type = "rademacher")
print(boot)
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
dqrng::dqset.seed(888)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(888)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "full/_THO_MONTHLY_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel21 = lm(HRS_WRKD~ PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                   PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                   PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                   PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                   PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                   PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                   PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                   PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                   PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                   PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                   PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                   PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                   PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                   PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                   PTP_YearMonth2023_8+PTP_YearMonth2023_9+ 
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
dqrng::dqset.seed(887)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(887)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "full/_HRS_WRKD_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel31 = lm(LOG_WAGE~ PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                   PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                   PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                   PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                   PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                   PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                   PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                   PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                   PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                   PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                   PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                   PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                   PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                   PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                   PTP_YearMonth2023_8+PTP_YearMonth2023_9+ 
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
dqrng::dqset.seed(886)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(886)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "full/_LOG_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")



##########################################################
#                  FORMAL LABORFORCE                     #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel1 = lm(THO_MONTHLY_WAGE~  PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                  PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                  PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                  PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                  PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                  PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                  PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                  PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                  PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                  PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                  PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                  PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                  PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                  PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                  PTP_YearMonth2023_8+PTP_YearMonth2023_9+
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
dqrng::dqset.seed(885)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(885)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "FORMAL_THO_MONTHLY_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel2 = lm(HRS_WRKD~  PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                  PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                  PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                  PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                  PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                  PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                  PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                  PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                  PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                  PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                  PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                  PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                  PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                  PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                  PTP_YearMonth2023_8+PTP_YearMonth2023_9+
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
dqrng::dqset.seed(884)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(884)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "FORMAL_HRS_WRKD_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel3 = lm(LOG_WAGE~  PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                  PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                  PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                  PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                  PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                  PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                  PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                  PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                  PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                  PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                  PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                  PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                  PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                  PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                  PTP_YearMonth2023_8+PTP_YearMonth2023_9+
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
dqrng::dqset.seed(883)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(883)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "FORMAL_LOG_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


############################################################
#                  INFORMAL LABORFORCE                     #
############################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################


parrallel111 = lm(THO_MONTHLY_WAGE~ PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                    PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                    PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                    PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                    PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                    PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                    PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                    PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                    PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                    PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                    PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                    PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                    PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                    PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                    PTP_YearMonth2023_8+PTP_YearMonth2023_9+ 
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
dqrng::dqset.seed(882)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(882)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]



setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "INFORMAL_THO_MONTHLY_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

##############
#  HRS_WRKD  #
##############


parrallel222 = lm(HRS_WRKD~  PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                    PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                    PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                    PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                    PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                    PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                    PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                    PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                    PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                    PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                    PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                    PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                    PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                    PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                    PTP_YearMonth2023_8+PTP_YearMonth2023_9+
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
dqrng::dqset.seed(881)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(881)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "INFORMAL_HRS_WRKD_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#######################
#  LOG_WAGE DATASET   #
#######################


parrallel333 = lm(LOG_WAGE~  PTP_YearMonth2018_1+  PTP_YearMonth2018_10+ PTP_YearMonth2018_11+PTP_YearMonth2018_12+ PTP_YearMonth2018_2+ 
                    PTP_YearMonth2018_3+  PTP_YearMonth2018_4+PTP_YearMonth2018_5+PTP_YearMonth2018_6 + PTP_YearMonth2018_7+ 
                    PTP_YearMonth2018_8 + PTP_YearMonth2018_9+  PTP_YearMonth2019_1+PTP_YearMonth2019_10+ PTP_YearMonth2019_11+
                    PTP_YearMonth2019_12+ PTP_YearMonth2019_2+  PTP_YearMonth2019_3+PTP_YearMonth2019_4+  PTP_YearMonth2019_5+ 
                    PTP_YearMonth2019_6 + PTP_YearMonth2019_7+  PTP_YearMonth2019_8+PTP_YearMonth2019_9+  PTP_YearMonth2020_1+ 
                    PTP_YearMonth2020_10+ PTP_YearMonth2020_11+ PTP_YearMonth2020_12+ PTP_YearMonth2020_2+  PTP_YearMonth2020_3+ 
                    PTP_YearMonth2020_4 + PTP_YearMonth2020_5+  PTP_YearMonth2020_6+PTP_YearMonth2020_7+  PTP_YearMonth2020_8+ 
                    PTP_YearMonth2020_9+  PTP_YearMonth2021_1+  PTP_YearMonth2021_10+PTP_YearMonth2021_11+ PTP_YearMonth2021_12+
                    PTP_YearMonth2021_2+   PTP_YearMonth2021_4+PTP_YearMonth2021_5+  PTP_YearMonth2021_6+ 
                    PTP_YearMonth2021_7+  PTP_YearMonth2021_8 + PTP_YearMonth2021_9+ PTP_YearMonth2022_1+  PTP_YearMonth2022_10+
                    PTP_YearMonth2022_11+ PTP_YearMonth2022_12+ PTP_YearMonth2022_2+PTP_YearMonth2022_3+  PTP_YearMonth2022_4+
                    PTP_YearMonth2022_5+  PTP_YearMonth2022_6+  PTP_YearMonth2022_7+PTP_YearMonth2022_8+  PTP_YearMonth2022_9+ 
                    PTP_YearMonth2023_1+  PTP_YearMonth2023_10+ PTP_YearMonth2023_11+ PTP_YearMonth2023_12+ PTP_YearMonth2023_2+ 
                    PTP_YearMonth2023_3+  PTP_YearMonth2023_4+  PTP_YearMonth2023_5+  PTP_YearMonth2023_6+  PTP_YearMonth2023_7+ 
                    PTP_YearMonth2023_8+PTP_YearMonth2023_9+
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
dqrng::dqset.seed(880)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(880)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/TG/full/")
write_dta(merged_df, "INFORMAL_LOG_WAGE_PTP_QUART_FULL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")