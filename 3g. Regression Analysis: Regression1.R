#install.packages("purrr")
#install.packages("openxlsx")
#install.packages("Hmisc")
rm(list=ls()) # clear workspace
setwd("C:/Users/.../Colombia/GEIH DATA/")

library(haven) #read dataframes
library(dplyr) #modify columns
library(purrr) #merge dataframes by columns
library(Hmisc)
library(stargazer)
library(sandwich)
library(lmtest)


data <- read_dta("GEIH_REDUCED_2018_2023.dta")

data_informal <- subset(data, data$FULL_INFORMAL == 1)

data_formal <- subset(data, data$FULL_FORMAL == 1)


reg = lm(THO_MONTHLY_WAGE ~  AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + MIGRANT + FULL_INFORMAL+DPTO_GDP+DPTO_GINI+DPTO_HOMIC+DPTO_UNSAT+DPTO_TERROR, data = data)
summary(reg)
#######################
#        RUN          #
#      LOG_WAGE       #
#     REGRESSION      #
#######################

####################
#   FULL DATASET   #
####################

didreg = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + MIGRANT + FULL_INFORMAL+DPTO_GDP+DPTO_GINI+DPTO_HOMIC+DPTO_UNSAT+DPTO_TERROR, data = data)
summary(didreg)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID         -9.0684e-04  2.9938e-02  -0.0303   0.97584  

###################
#     CLUSTER     #
# STANDARD ERRORS #
###################
cluster_se <- vcovCL(didreg, cluster = ~ DPTO)

didreg_cluster_se<-coeftest(didreg,cluster_se)
print(didreg_cluster_se)


if (!requireNamespace("clubSandwich", quietly = TRUE)) {
  install.packages("clubSandwich")
}
library(clubSandwich)

wald_test <- Wald_test(
  didreg,
  constraints = constrain_zero(3),  # Test the coefficient of DID (3RD coefficient)
  vcov = "CR2",
  cluster = data$DPTO,
  test = "boot",
  R = 100  # Number of bootstrap replications
)
print(wald_test)

library(fwildclusterboot)
lm_boot <- boottest(didreg, clustid = c("DID"), B = 500, param = "TREATED")
summary(lm_boot)

library(Rfast2)

wild.boot(y, x, cluster, ind = NULL, R = 500, parallel = FALSE)

#####################
# DATASET: INFORMAL #
#####################

didreg_informal = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + MIGRANT +DPTO_GDP+DPTO_GINI+DPTO_HOMIC+DPTO_UNSAT+DPTO_TERROR, data = data_informal)
summary(didreg_informal)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID          1.6717e-02  3.1524e-02   0.5303  0.595922   

###################
#     CLUSTER     #
# STANDARD ERRORS #
#    INFORMAL     #
###################
cluster_se_informal <- vcovCL(didreg_informal, cluster = ~ DPTO)

# Test hypotheses using the cluster-robust standard errors
didreg_informal_cluster_se<-coeftest(didreg_informal,cluster_se_informal)
print(didreg_informal_cluster_se)

#####################
#  DATASET: FORMAL  #
#####################

didreg_formal = lm(LOG_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + MIGRANT +DPTO_GDP+DPTO_GINI+DPTO_HOMIC+DPTO_UNSAT+DPTO_TERROR, data = data_formal)
summary(didreg_formal)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID         -0.08737525  0.04577564 -1.9088  0.056314 .  


###################
#     CLUSTER     #
# STANDARD ERRORS #
#     FORMAL      #
###################
cluster_se_formal <- vcovCL(didreg_formal, cluster = ~ DPTO)

# Test hypotheses using the cluster-robust standard errors
didreg_formal_cluster_se<-coeftest(didreg_formal,cluster_se_formal)
print(didreg_formal_cluster_se)

#############SAVE REGRESSION #######################
DID_LOG_WAGE_table <- stargazer(didreg, didreg_informal,didreg_formal,type="text",title = "DID COLOMBIA REGRESSION",
          label = "COL_REG_DID",column.labels = c("FULL DATASET", "INFORMAL DATASET", "FORMAL DATASET"),
          notes = "LOG WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + VENEZUELAN + URBAN")

# Define the file path to save the output
file_path <- "SAVED REGRESSION/DID_LOG_WAGE_table.tex" 

# Use sink() to redirect the output to the file
sink(file_path)
cat(DID_LOG_WAGE_table)
sink()

#The PPT program implementation caused a 8.738 drop in wages among the formally employed


#######################
#         RUN         #
#   THO_MONTHLY_WAGE  #
#     REGRESSION 2    #
#######################


didr = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + VENEZUELAN + URBAN + INFORMAL, data = data)
summary(didr)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID         -9.068e-04  1.476e-02  -0.061   0.9510    
#######################INFORMAL#######################
didr_informal = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + VENEZUELAN + URBAN, data = data_informal)
summary(did_informal)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID          1.672e-02  1.599e-02   1.046  0.29572   
########################FORMAL########################
didr_formal = lm(THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + VENEZUELAN + URBAN, data = data_formal)
summary(didr_formal)
# THE VARIABLE OF INTEREST IS DID
# SO THE ESTIMATE OF INTEREST IS : DID         -8.738e-02  3.724e-02  -2.346 0.018985 *  

#############SAVE REGRESSION #######################

DID_THO_WAGE_table <-stargazer(didr, didr_informal,didr_formal,type="latex",title = "DID COLOMBIA REGRESSION",
          label = "COL_REG_DID",column.labels = c("FULL DATASET", "INFORMAL DATASET", "FORMAL DATASET"),
          notes = "THO_MONTHLY_WAGE ~ TREATED + PPT + DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + VENEZUELAN + URBAN")

# Define the file path to save the output
file_path <- "SAVED REGRESSION/DID_THO_WAGE_table.tex" 

# Use sink() to redirect the output to the file
sink(file_path)
cat(DID_THO_WAGE_table)
sink()


#######################
#     alternate       #
#     REGRESSION      #
#######################




#install.packages("did")
#The coefficient for ‘did’ is the differences-in-differences estimator. The effect is significant at .1% with the treatment having a negative effect.
library(did)
example_attgt <- att_gt(yname = "LOG_WAGE",
                        tname = "ANO",
                        idname = "ID",
                        gname = "PPT",
                        # xformla = NULL,
                        control_group = "notyettreated",
                        data = data
)

coef( lm(LOG_WAGE~TREATED, data=data, subset=(ANO<=2021)) )
#(Intercept)      TREATED 
#13.424942070  0.003791196 

coef( lm(LOG_WAGE~TREATED, data=data, subset=(ANO>=2021)) )
#(Intercept)     TREATED 
#13.7749484  -0.1105268 

# Joint regression including an interaction term
library(lmtest)
coeftest( lm(LOG_WAGE~TREATED*PPT, data=data) )

#   t test of coefficients:

#                Estimate Std. Error   t value  Pr(>|t|)    
#  (Intercept) 13.3984315  0.0033412 4010.1188 < 2.2e-16 ***
#  TREATED      0.0153160  0.0034402    4.4521 8.504e-06 ***
#  PPT          0.3765169  0.0046575   80.8413 < 2.2e-16 ***
#  TREATED:PPT -0.1258428  0.0048435  -25.9815 < 2.2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

DiD <- lm(LOG_WAGE~TREATED*PPT , data=data)

DiDcontr <- lm(LOG_WAGE~TREATED*PPT+AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  VENEZUELAN  + URBAN + INFORMAL, data=data)

library(stargazer)

OLS <- lm(LOG_WAGE~AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +  VENEZUELAN  + URBAN + INFORMAL, data=data)

stargazer(OLS, DiD,DiDcontr,type="text")


