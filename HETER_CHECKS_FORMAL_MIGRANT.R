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

df_fo_ven <- read_dta("Migrant_formal.dta")


df_fo_ven <- df_fo_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))
params<-c("DID") 

#######################
# DATASET:  EDUCATION #
#######################

#HIGH OR LESS
df_highschool <- subset(df_fo_ven, EDUC<14)
#University or more
df_university_or_more <- subset(df_fo_ven, EDUC>=14)

AVG <- mean(df_university_or_more$THO_MONTHLY_WAGE, na.rm = TRUE) #AVERAGE
1754.7950

AVG2 <- mean(df_university_or_more$HRS_WRKD, na.rm = TRUE)
49.19324

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_fo_ven, GENDER==0)


########################
# DATASET:  HIGHSCHOOL #
########################

####################
# THO_MONTHLY_WAGE #
####################

didreg_prim = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_highschool)

REGRESSION<-didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_prim = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + | 
                            DPTO + Time_FE, data = df_highschool)
summary(feols_didreg_prim)
REGRESSION<-feols_didreg_prim
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
#0.1901902 -75.18407 16.34266 -33.84387 -1.884844
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df1 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df1$type <- "MIGRANT HIGHSCHOOL DIPLOMA EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df1, "HS_Dip_Migrant_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_prim = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_highschool)

REGRESSION<-didreg4_didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_prim = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                                    DPTO + Time_FE, data = df_highschool)
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
#DID 0.5535536 -2.96323 1.040136 -0.9454096 -1.090787
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df2 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df2$type <- "MIGRANT PRIMARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df2, "PRIMARY_Migrant_FORMAL_MONTHLY_WAGE_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

########################
# DATASET:  University #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_HIGH = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_university_or_more)

REGRESSION<-didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                            DPTO + Time_FE, data = df_university_or_more)
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
#DID 0.4574575 -758.5391 250.3348 -194.5972 -1.005224
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df3 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df3$type <- "MIGRANT SECONDARY EDUCATION : THO_MONTHLY_WAGES"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df3, "University_Diploma_Migrant_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_HIGH = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_university_or_more)

REGRESSION<-didreg4_didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HIGH = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                                    DPTO + Time_FE, data = df_university_or_more)
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
#DID 0.8198198 -1.430628 1.527606 0.1637703 0.299845
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df4 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df4$type <- "MIGRANT SECONDARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df4, "University_diploma_Migrant_FORMAL_HRS_WRK_regression.dta") 
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
write_dta(combined_df, "MERGED_Migrant_FORMAL_EDUC.dta") 
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
feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
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
#DID 0.5315315 -293.6711 163.3434 -64.40721 -0.626796
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df9 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df9$type <- "MIGRANT WOMEN : THO_MONTHLY_WAGE"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Migrant_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_WOMEN = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+ 
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg4_didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
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
#DID 0.4474474 -1.767551 0.7322976 -0.3901244 -0.8092726
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df10 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df10$type <- "MIGRANT WOMEN : WEEKLY_HOURS_WORKED"

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Migrant_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

combined_df2 <- rbind(merged_df9, merged_df10)
combined_df <- rbind(combined_df, combined_df2)

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Migrant_FORMAL_HETER_CHECKS.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


