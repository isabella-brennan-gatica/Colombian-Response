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

df <- read_dta("GEIH_REDUCED_2018_2023.dta")

########################
#  CREATE ANO DUMMIES  #
########################


data_with_ANO_dummies <- dummy_cols(df, select_columns = "ANO",remove_first_dummy = FALSE)

variables <- c("DPTO_GDP", "DPTO_GINI", "DPTO_HOMIC", "DPTO_UNSAT", "DPTO_TERROR", "DPTO_INFO")

# Extract year dummy columns
DUMMY <- select(data_with_ANO_dummies, starts_with("ANO_"))

interaction_terms <- list() 
for (col in variables) {
  for (dummy in names(DUMMY)) {
    interaction_col_name <- paste(col, dummy, sep = "_")
    data_with_ANO_dummies[[interaction_col_name]] <- data_with_ANO_dummies[[col]] * data_with_ANO_dummies[[dummy]]
    interaction_terms <- c(interaction_terms, interaction_col_name)
    }
}

print("List of interaction terms:")
print(interaction_terms)

df<-data_with_ANO_dummies

###########################
#  CREATE SUBSET DATASET  #
###########################
df_in <- subset(df, df$FULL_INFORMAL == 1)

#dataset informal :Colombian
df_in_col <- subset(df_in, df_in$COLOMBIAN == 1)
#dataset informal :Migrant
df_in_ven <- subset(df_in, df_in$MIGRANT == 1)

df_fo <- subset(df, df$FULL_FORMAL == 1)

#dataset formal :Colombian
df_fo_col <- subset(df_fo, df_fo$COLOMBIAN == 1)
#dataset formal :Migrant
df_fo_ven <- subset(df_fo, df_fo$MIGRANT == 1)


####################
#   FULL DATASET   #
####################
############
# LOG_WAGE #
############
didreg = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
              GENDER + COHABITATE + EDUC + JEFE + MIGRANT + 
              FULL_INFORMAL+
              DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
              DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
              DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
              DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
              DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
              DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
              DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
              DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
              DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
              DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df)
summary(didreg)

REGRESSION<-didreg
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(123456)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(123)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_Full_regression.dta")
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# THO_WAGE #
############
didreg2 = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
              GENDER + COHABITATE + EDUC + JEFE + MIGRANT + 
              FULL_INFORMAL+
              DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
              DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
              DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
              DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
              DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
              DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
              DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
              DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
              DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
              DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df)
summary(didreg2)

REGRESSION<-didreg2
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(134567)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(134)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_Full_MONTHLY_WAGE_regression.dta")
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# HRS_WEEK #
############
didreg3 = lm(HRS_WRKD ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
              GENDER + COHABITATE + EDUC + JEFE + MIGRANT + 
              FULL_INFORMAL+
              DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
              DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
              DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
              DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
              DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
              DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
              DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
              DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
              DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
              DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df)
summary(didreg3)

REGRESSION<-didreg3
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(145678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(145)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_Full_HRS_WRK_regression.dta")
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


#####################
# DATASET: INFORMAL #
#####################

############
# LOG_WAGE #
############

didreg_informal = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                       GENDER + COHABITATE + EDUC + JEFE +
                       DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                       DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                       DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                       DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                       DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                       DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                       DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                       DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                       DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                       DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_in)
summary(didreg_informal)


REGRESSION<-didreg_informal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(2)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_INFORMAL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_informal = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                       GENDER + COHABITATE + EDUC + JEFE + MIGRANT +
                       DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                       DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                       DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                       DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                       DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                       DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                       DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                       DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                       DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                       DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_in)
summary(didreg2_informal)


REGRESSION<-didreg2_informal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(245678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(245)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_INFORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg3_informal = lm(HRS_WRKD ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                        GENDER + COHABITATE + EDUC + JEFE + MIGRANT +
                        DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                        DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                        DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                        DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                        DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                        DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                        DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                        DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                        DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                        DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_in)
summary(didreg3_informal)


REGRESSION<-didreg3_informal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(256789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(256)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_INFORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")


#####################
# DATASET:   FORMAL #
#####################

############
# LOG_WAGE #
############

didreg_formal = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                       GENDER + COHABITATE + EDUC + JEFE + MIGRANT +
                       DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                       DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                       DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                       DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                       DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                       DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                       DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                       DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                       DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                       DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_fo)
summary(didreg_formal)


REGRESSION<-didreg_formal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(345678)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(345)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_FORMAL_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# THO_WAGE #
############

didreg2_formal = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                     GENDER + COHABITATE + EDUC + JEFE + MIGRANT +
                     DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                     DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                     DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                     DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                     DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                     DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                     DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                     DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                     DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                     DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_fo)
summary(didreg2_formal)


REGRESSION<-didreg2_formal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(356789)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(356)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_FORMAL_MONTHLY_WAGE_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

############
# HRS_WEEK #
############

didreg3_formal = lm(HRS_WRKD ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
                      GENDER + COHABITATE + EDUC + JEFE + MIGRANT +
                      DPTO_GDP_ANO_2018+DPTO_GDP_ANO_2019+DPTO_GDP_ANO_2020+DPTO_GDP_ANO_2021+
                      DPTO_GDP_ANO_2022+DPTO_GDP_ANO_2023+DPTO_GINI_ANO_2018+DPTO_GINI_ANO_2019+
                      DPTO_GINI_ANO_2020+DPTO_GINI_ANO_2021+DPTO_GINI_ANO_2022+DPTO_GINI_ANO_2023+
                      DPTO_HOMIC_ANO_2018+DPTO_HOMIC_ANO_2019+
                      DPTO_HOMIC_ANO_2020+DPTO_HOMIC_ANO_2021+DPTO_HOMIC_ANO_2022+DPTO_HOMIC_ANO_2023+
                      DPTO_UNSAT_ANO_2018+DPTO_UNSAT_ANO_2019+
                      DPTO_UNSAT_ANO_2020+DPTO_UNSAT_ANO_2021+DPTO_UNSAT_ANO_2022+DPTO_UNSAT_ANO_2023+
                      DPTO_TERROR_ANO_2018+DPTO_TERROR_ANO_2019+
                      DPTO_TERROR_ANO_2020+DPTO_TERROR_ANO_2021+DPTO_TERROR_ANO_2022+DPTO_TERROR_ANO_2023+DPTO_INFO_ANO_2018+
                      DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df_fo)
summary(didreg3_formal)


REGRESSION<-didreg3_formal
# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(367891)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(367)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/")
write_dta(results_df, "Colombia_FORMAL_HRS_WRK_regression.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")
