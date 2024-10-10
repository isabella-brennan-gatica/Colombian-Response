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

##################################
#  CREATE ANO TREATMENT DUMMIES  #
##################################

variables <- c("DID")

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

df2<-data_with_ANO_dummies
df2<- subset(df2, df2$FULL_FORMAL == 1)






######################
#   Formal DATASET   #
######################

didreg_res = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
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
               DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023, data = df2)
summary(didreg_res)

REGRESSION<-didreg_res

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")
results_restricted <- result_table

######################
#   Formal DATASET   #
######################



didreg_un = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + 
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
              DPTO_INFO_ANO_2019+DPTO_INFO_ANO_2020+DPTO_INFO_ANO_2021+DPTO_INFO_ANO_2022+DPTO_INFO_ANO_2023+
              DID_ANO_2018+DID_ANO_2019+DID_ANO_2020+DID_ANO_2021+DID_ANO_2022+DID_ANO_2023, data = df2)
summary(didreg_un)


REGRESSION<-didreg_un

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/Clustered_Standard_Error_code.R")

results_unrestricted <- result_table

########
# test #
########

library(datasets)
library(car)

anova_result <- anova(didreg_res, didreg_un)
print(anova_result)

####
hypothesis <- c("DID_ANO_2018=0","DID_ANO_2019=0","DID_ANO_2020=0","DID_ANO_2021=0","DID_ANO_2022=0","DID_ANO_2023=0")

# Perform the F-test for the compound null hypothesis
f_test_result <- linearHypothesis(didreg_un, hypothesis, vcov = vcovCL(REGRESSION, cluster = ~ DPTO))

# Print the F-test results
print(f_test_result)




#####
Analysis of Variance Table

Model 1: THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + 
  COHABITATE + EDUC + JEFE + DPTO_GDP_ANO_2018 + DPTO_GDP_ANO_2019 + 
  DPTO_GDP_ANO_2020 + DPTO_GDP_ANO_2021 + DPTO_GDP_ANO_2022 + 
  DPTO_GDP_ANO_2023 + DPTO_GINI_ANO_2018 + DPTO_GINI_ANO_2019 + 
  DPTO_GINI_ANO_2020 + DPTO_GINI_ANO_2021 + DPTO_GINI_ANO_2022 + 
  DPTO_GINI_ANO_2023 + DPTO_HOMIC_ANO_2018 + DPTO_HOMIC_ANO_2019 + 
  DPTO_HOMIC_ANO_2020 + DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + 
  DPTO_HOMIC_ANO_2023 + DPTO_UNSAT_ANO_2018 + DPTO_UNSAT_ANO_2019 + 
  DPTO_UNSAT_ANO_2020 + DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + 
  DPTO_UNSAT_ANO_2023 + DPTO_TERROR_ANO_2018 + DPTO_TERROR_ANO_2019 + 
  DPTO_TERROR_ANO_2020 + DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + 
  DPTO_TERROR_ANO_2023 + DPTO_INFO_ANO_2018 + DPTO_INFO_ANO_2019 + 
  DPTO_INFO_ANO_2020 + DPTO_INFO_ANO_2021 + DPTO_INFO_ANO_2022 + 
  DPTO_INFO_ANO_2023
Model 2: THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + 
  COHABITATE + EDUC + JEFE + DPTO_GDP_ANO_2018 + DPTO_GDP_ANO_2019 + 
  DPTO_GDP_ANO_2020 + DPTO_GDP_ANO_2021 + DPTO_GDP_ANO_2022 + 
  DPTO_GDP_ANO_2023 + DPTO_GINI_ANO_2018 + DPTO_GINI_ANO_2019 + 
  DPTO_GINI_ANO_2020 + DPTO_GINI_ANO_2021 + DPTO_GINI_ANO_2022 + 
  DPTO_GINI_ANO_2023 + DPTO_HOMIC_ANO_2018 + DPTO_HOMIC_ANO_2019 + 
  DPTO_HOMIC_ANO_2020 + DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + 
  DPTO_HOMIC_ANO_2023 + DPTO_UNSAT_ANO_2018 + DPTO_UNSAT_ANO_2019 + 
  DPTO_UNSAT_ANO_2020 + DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + 
  DPTO_UNSAT_ANO_2023 + DPTO_TERROR_ANO_2018 + DPTO_TERROR_ANO_2019 + 
  DPTO_TERROR_ANO_2020 + DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + 
  DPTO_TERROR_ANO_2023 + DPTO_INFO_ANO_2018 + DPTO_INFO_ANO_2019 + 
  DPTO_INFO_ANO_2020 + DPTO_INFO_ANO_2021 + DPTO_INFO_ANO_2022 + 
  DPTO_INFO_ANO_2023 + DID_ANO_2018 + DID_ANO_2019 + DID_ANO_2020 + 
  DID_ANO_2021 + DID_ANO_2022 + DID_ANO_2023
Res.Df        RSS Df Sum of Sq      F Pr(>F)
1 545928 1.6478e+12                           
2 545926 1.6478e+12  2   5605960 0.9287 0.3951

#Since the p-value from the ANOVA table is not less than 0.05, we fail to reject the null hypothesis.

#This means we don’t have sufficient evidence to say that there is a statistically significant difference between the mean WAGES of the 2 groups.
# Print the result

