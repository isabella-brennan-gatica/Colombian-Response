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

df_fo_col <- read_dta("Colombian_formal.dta")

##############################
#  CREATE TIME FIXED EFFECT  #
##############################

df_fo_col <- df_fo_col %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

params<-c("DID")


#######################
# DATASET:  EDUCATION #
#######################

#Primary 6
df_prim <- subset(df_fo_col, EDUC<11)
#Highschool 11<14
df_high <- subset(df_fo_col, EDUC>=11)
df_high <- subset(df_high, EDUC<14)
#Highereducation 14,15
df_uni <- subset(df_fo_col, EDUC==14 |EDUC==15)
#elite 17
df_mas <- subset(df_fo_col, EDUC==17)

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_fo_col, GENDER==0)

#####################
# DATASET:  PRIMARY #
#####################

####################
# THO_MONTHLY_WAGE #
####################

didreg_prim = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                         as.factor(Time_FE)+ as.factor(DPTO), data = df_prim)

REGRESSION<-didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_prim = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                  DPTO + Time_FE, data = df_prim)
summary(feols_didreg_prim)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.06106106  -26.19586  0.919625  -13.70459  -2.57569
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df1 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df1$type <- "NATIVE PRIMARY EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df1, "PRIMARY_Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg4_didreg_prim = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                          as.factor(Time_FE)+ as.factor(DPTO), data = df_prim)

REGRESSION<-didreg4_didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_prim = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|
                                   DPTO + Time_FE, data = df_prim)
summary(feols_didreg4_didreg_prim)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.009009009 0.08869715 1.134182 0.4974194 2.478013
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df2 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df2$type <- "NATIVE PRIMARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df2, "PRIMARY_Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

########################
# DATASET:  HIGHSCHOOL #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_HIGH = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_high)

REGRESSION<-didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                            DPTO + Time_FE, data = df_high)
summary(feols_didreg_HIGH)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.1641642 -27.84575 8.150996 -13.22952 -1.718223
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df3 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df3$type <- "NATIVE SECONDARY EDUCATION : THO_MONTHLY_WAGES"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df3, "HIGHSCHOOL_Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_HIGH = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_high)

REGRESSION<-didreg4_didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HIGH = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  | 
                                    DPTO + Time_FE, data = df_high)
summary(feols_didreg4_didreg_HIGH)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#1 DID 0.6106106 -0.884592 0.5733335 -0.2291528 -0.7642992
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df4 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df4$type <- "NATIVE SECONDARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df4, "HIGHSCHOOL_Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

########################
# DATASET: HIGHER EDUC #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_HIGHER = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_uni)

REGRESSION<-didreg_HIGHER

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGHER = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_uni)
summary(feols_didreg_HIGHER)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.4514515 -44.14173 16.57865 -10.30944 -0.8568212

#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df5 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df5$type <- "NATIVE HIGHER EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df5, "HIGHER_EDUC_Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_HIGHER = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_uni)

REGRESSION<-didreg4_didreg_HIGHER

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HIGHER = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                                    DPTO + Time_FE, data = df_uni)
summary(feols_didreg4_didreg_HIGHER)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.8358358 -0.3470149 0.4715482 0.03759685 0.2172996
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df6 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df6$type <- "NATIVE HIGHER EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df6, "HIGHER_EDUC_Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

########################
# DATASET: ELITE EDUC  #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_ELITE = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                     as.factor(Time_FE)+ as.factor(DPTO), data = df_mas)

REGRESSION<-didreg_ELITE

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_ELITE = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                              DPTO + Time_FE, data = df_mas)
summary(feols_didreg_ELITE)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.4264264 -73.7892 308.4639 65.45879 0.916037
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df7 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df7$type <- "NATIVE MASTER/DOC EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df7, "ELITE_Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_ELITE = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+
                             as.factor(Time_FE)+ as.factor(DPTO), data = df_mas)

REGRESSION<-didreg4_didreg_ELITE

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_ELITE = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                      DPTO + Time_FE, data = df_mas)
summary(feols_didreg4_didreg_ELITE)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#	 0.1461461 -0.1322288 0.5335061 0.1900781 1.662379
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df8 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df8$type <- "NATIVE MASTER/DOC EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df8, "ELITE_Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

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

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Colombia_FORMAL_EDUC.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")


##################################
#              WOMEN             #       
##################################


############
# THO_MONTHLY_WAGE #
############

didreg_WOMEN = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                    as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                             DPTO + Time_FE, data = df_female)
summary(feols_didreg_WOMEN)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(219)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(219)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.1301301 -46.80542 6.4231 -18.10731 -1.614478
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df9 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df9$type <- "NATIVE WOMEN : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_WOMEN = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+ 
                            as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg4_didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                     DPTO + Time_FE, data = df_female)
summary(feols_didreg4_didreg_WOMEN)

source("C:/Users/.../Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(419)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(419)

source("C:/Users/.../Colombia/GEIH R CODE/WildBootstrap_code.R")
#DID 0.7567568 -0.2378889 0.3356957 0.04004264 0.3237947
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df10 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df10$type <- "NATIVE WOMEN : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

combined_df2 <- rbind(merged_df9, merged_df10)
combined_df <- rbind(combined_df, combined_df2)

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Colombia_FORMAL_HETER_CHECKS.dta") 

df_MIGRANT <- read_dta("MERGED_Migrant_FORMAL_HETER_CHECKS.dta")

HETERO_DF <- rbind(combined_df, df_MIGRANT)

write_dta(HETERO_DF, "ALL_MERGED_FORMAL_HETER_CHECKS.dta") 
