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

df_in_ven <- read_dta("Migrant_informal.dta")

df_in_ven <- df_in_ven %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))
params<-c("DID") 

#######################
# DATASET:  EDUCATION #
#######################

#HIGH OR LESS
df_highschool <- subset(df_in_ven, EDUC<14)
#University or more
df_university_or_more <- subset(df_in_ven, EDUC>=14)

#######################
# DATASET:  GENDER    #
#######################
df_female <- subset(df_in_ven, GENDER==0)


#####################
# DATASET:  HIGHSCHOOL #
#####################

####################
# THO_MONTHLY_WAGE #
####################

didreg_HIGH = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 +
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_highschool)

REGRESSION<-didreg_HIGH

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_HIGH = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                            DPTO + Time_FE, data = df_highschool)
summary(feols_didreg_HIGH)
REGRESSION<-feols_didreg_HIGH
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
#DID 0.6026026 -36.33117 76.7239 19.13378 0.7695056
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df1 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df1$type <- "MIGRANT PRIMARY EDUCATION : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df1, "PRIMARY_Migrant_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
#df_highschool HRS_WEEK #
################

didreg4_didreg_prim = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_highschool)

REGRESSION<-didreg4_didreg_prim

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_HI = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
                                    DPTO + Time_FE, data = df_highschool)
summary(feols_didreg4_didreg_HI)
REGRESSION<-feols_didreg4_didreg_HI
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
#DID 0.4814815 -1.089105 0.5005481 -0.2791505 -0.828983
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df2 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df2$type <- "MIGRANT PRIMARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df2, "PRIMARY_Migrant_INFORMAL_MONTHLY_WAGE_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

########################
# DATASET:  uNIVERSITY #
########################

############
# THO_MONTHLY_WAGE #
############

didreg_UN= lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                   as.factor(Time_FE)+ as.factor(DPTO), data = df_university_or_more)

REGRESSION<-didreg_UN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_UN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                            DPTO + Time_FE, data = df_university_or_more)
summary(feols_didreg_UN)
REGRESSION<-feols_didreg_UN

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
#DID 0.08208208 -136.4514 10.26251 -65.59471 -2.114514
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df3 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df3$type <- "MIGRANT SECONDARY EDUCATION : THO_MONTHLY_WAGES"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df3, "HIGHSCHOOL_Migrant_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

################
# LOG_HRS_WEEK #
################

didreg4_didreg_UN = lm(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 + 
                           as.factor(Time_FE)+ as.factor(DPTO), data = df_university_or_more)

REGRESSION<-didreg4_didreg_UN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg4_didreg_UN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
                                    DPTO + Time_FE, data = df_university_or_more)
summary(feols_didreg4_didreg_UN)
REGRESSION<-feols_didreg4_didreg_UN

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
#DID 0.0590590 -1.627594 0.01956159 -0.8635271 -2.378674
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df4 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df4$type <- "MIGRANT SECONDARY EDUCATION : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df4, "HIGHSCHOOL_Migrant_INFORMAL_HRS_WRK_regression.dta") 
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
write_dta(combined_df, "MERGED_Migrant_INFORMAL_EDUC.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")


##################################
#              WOMEN             #       
##################################


############
# THO_MONTHLY_WAGE #
############

didreg_WOMEN = lm(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023+ 
                    as.factor(Time_FE)+ as.factor(DPTO), data = df_female)

REGRESSION<-didreg_WOMEN

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
feols_didreg_WOMEN = feols(THO_MONTHLY_WAGE ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 | 
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
#DID 0.7097097 -67.70624 90.5524 12.12415 0.3496696
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df9 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df9$type <- "MIGRANT WOMEN : THO_MONTHLY_WAGE"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Migrant_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

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
feols_didreg4_didreg_WOMEN = feols(HRS_WRKD ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023| 
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
#DID 0.3913914 -1.202498 0.4150408 -0.3673657 -1.04453
#######################
#    MERGE RESULTS    #
#######################

merged_df <- merge(result_table, results_df, by = "term", all = TRUE)

rows_to_move <- merged_df$term %in% params

# Move these rows to the top
merged_df10 <- merged_df[c(which(rows_to_move), which(!rows_to_move)), ]
merged_df10$type <- "MIGRANT WOMEN : WEEKLY_HOURS_WORKED"

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(merged_df9, "WOMEN_Migrant_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")

combined_df2 <- rbind(merged_df9, merged_df10)
combined_df <- rbind(combined_df, combined_df2)

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/HET_EDUC/")
write_dta(combined_df, "MERGED_Migrant_INFORMAL_HETER_CHECKS.dta") 

df_infor_col <- read_dta("MERGED_Colombia_INFORMAL_HETER_CHECKS.dta")

HETERO_DF <- rbind(combined_df, df_infor_col)

write_dta(HETERO_DF, "ALL_MERGED_INFORMAL_HETER_CHECKS.dta") 
