#install.packages("purrr")
#install.packages("openxlsx")
#install.packages("Hmisc")
#install.packages("fastDummies")

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
library(fixest)

df_in_col <- read_dta("Colombian_informal.dta")

##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df_in_col <- df_in_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

params<-c("DID")


#######################
# DATASET:  EDUCATION #
#######################

#Primary 6
df_prim <- subset(df_in_col, EDUC<11)
#Highschool 11<14
df_high <- subset(df_in_col, EDUC==11|EDUC==12|EDUC==13)
#Highereducation 14,15
df_uni <- subset(df_in_col, EDUC==14|EDUC==15)
#elite 17
df_mas <- subset(df_in_col, EDUC==17)

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_in_col, GENDER==0)

#####################
# DATASET:  PRIMARY #
#####################

####################
# THO_MONTHLY_WAGE #
####################

didreg_prim = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_prim)

REGRESSION<-didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_prim = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_prim)
summary(feols_didreg_prim)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.1371371 -34.06975 4.347664 -11.30699 -1.574033
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df1 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df1$type <- "NATIVE PRIMARY EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df1, "PRIMARY_Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg4_didreg_prim = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_prim)

REGRESSION<-didreg4_didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_prim = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_prim)
summary(feols_didreg4_didreg_prim)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.04004004 -0.8208301 -0.03224884 -0.3455764 -2.31005
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df2 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df2$type <- "NATIVE PRIMARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df2, "PRIMARY_Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

########################
# DATASET:  HIGHSCHOOL #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_HIGH = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_high)

REGRESSION<-didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                            DPTO + Time_FE, data = df_high)
summary(feols_didreg_HIGH)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.5295295 -21.8105 55.97042 10.09034 0.6413865
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df3 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df3$type <- "NATIVE SECONDARY EDUCATION : THO_MONTHLY_WAGES"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df3, "HIGHSCHOOL_Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_HIGH = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_high)

REGRESSION<-didreg4_didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HIGH = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_high)
summary(feols_didreg4_didreg_HIGH)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.04804805 -0.6907438 -0.01655458 -0.454393 -3.86975
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df4 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df4$type <- "NATIVE SECONDARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df4, "HIGHSCHOOL_Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

########################
# DATASET: HIGHER EDUC #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_HIGHER = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +  
                     as.factor(Time_FE)+ as.factor(DPTO), data = df_uni)

REGRESSION<-didreg_HIGHER

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGHER = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                              DPTO + Time_FE, data = df_uni)
summary(feols_didreg_HIGHER)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.6486486 -72.72786 115.8156 17.89822 0.5289241
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df5 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df5$type <- "NATIVE HIGHER EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df5, "HIGHER_EDUC_Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_HIGHER = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                             as.factor(Time_FE)+ as.factor(DPTO), data = df_uni)

REGRESSION<-didreg4_didreg_HIGHER

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HIGHER = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                                      DPTO + Time_FE, data = df_uni)
summary(feols_didreg4_didreg_HIGHER)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.1881882 -0.7871413 0.1631122 -0.3473952 -1.767953
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df6 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df6$type <- "NATIVE HIGHER EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df6, "HIGHER_EDUC_Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

########################
# DATASET: ELITE EDUC  #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_ELITE = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                    as.factor(Time_FE)+ as.factor(DPTO), data = df_mas)

REGRESSION<-didreg_ELITE

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_ELITE = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                             DPTO + Time_FE, data = df_mas)
summary(feols_didreg_ELITE)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.1411411 -53.83961 362.8531 117.4756 1.568375
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df7 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df7$type <- "NATIVE MASTER/DOC EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df7, "ELITE_Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_ELITE = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+ covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_mas)

REGRESSION<-didreg4_didreg_ELITE

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_ELITE = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                                     DPTO + Time_FE, data = df_mas)
summary(feols_didreg4_didreg_ELITE)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.4014014 -0.4961219 0.885094 0.2119189 0.9387333
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df8 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df8$type <- "NATIVE MASTER/DOC EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df8, "ELITE_Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

###########################
#    MERGE ALL RESULTS    #
###########################

combined_df <- rbind(merged_df1, merged_df2)
combined_df <- rbind(combined_df, merged_df3)
combined_df <- rbind(combined_df, merged_df4)
combined_df <- rbind(combined_df, merged_df5)
combined_df <- rbind(combined_df, merged_df6)
combined_df <- rbind(combined_df, merged_df7)
combined_df <- rbind(combined_df, merged_df8)

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Colombia_INFORMAL_EDUC.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


##################################
#              WOMEN             #       
##################################


############
# THO_MONTHLY_WAGE #
############

didreg_WOMEN = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                    as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                             DPTO + Time_FE, data = df_female)
summary(feols_didreg_WOMEN)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.8618619 -45.08659 42.9953 -3.047134 -0.1796682
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df9 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df9$type <- "NATIVE WOMEN : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_WOMEN = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg4_didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                     DPTO + Time_FE, data = df_female)
summary(feols_didreg4_didreg_WOMEN)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.01201201 -0.6230846 -0.1613715 -0.4011041 -4.199711
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df10 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df10$type <- "NATIVE WOMEN : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

combined_df2 <- rbind(merged_df9, merged_df10)
combined_df <- rbind(combined_df, combined_df2)

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Colombia_INFORMAL_HETER_CHECKS.dta") 

