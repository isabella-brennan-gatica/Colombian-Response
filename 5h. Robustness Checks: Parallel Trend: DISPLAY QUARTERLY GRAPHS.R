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
library(stringr)
library(ggplot2)

#######################
#       EDIT          #
#     DATAFRAME       #
#######################

df<- read_dta("GEIH_REDUCED_2018_2023.dta")
df$COVID<-ifelse(df$ANO=="2020",1,0)

####################
#  CREATE TIME FE  #
####################

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
  interaction_term <- paste("PTP", col, sep = "_")
  df2[[interaction_term]] <- df2$LOG_PTP * df2[[col]]
}
PTP_QUARTER_columns <- grep("^PTP_QUARTER", colnames(df2), value = TRUE)

df3<-df2
#df2<-df3

#df2 <- subset(df2, TREATED == 1|UNTREATED ==1)
#df2 <- subset(df2, ANO>="2019")
df2_formal<- subset(df2, df2$FULL_FORMAL == 1)
df2_informal<- subset(df2, df2$FULL_INFORMAL == 1)

##########################################################
#                   FULL LABORFORCE                      #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test<-feols(THO_MONTHLY_WAGE~  PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
              PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
              PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
              COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2)

coef_df <- as.data.frame(confint(test))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Coeff Plot for Colombian Monthly Wages",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical

###############################
#      LOG_WAGE DATASET       #
###############################

test12<-feols(LOG_WAGE~  PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
              PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
              PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
              COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2)

coef_df <- as.data.frame(confint(test12))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test12)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Coeff Plot for Colombian Log(Wage) ",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###############################
#      HRS_WRKD DATASET       #
###############################

test13<-feols(HRS_WRKD~  PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE|DPTO + Time_FE,data=df2)

coef_df <- as.data.frame(confint(test13))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test13)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Coeff Plot for Colombian Weekly Hours Worked ",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###################################
#      LOG_HRS_WRKD DATASET       #
###################################
df2 <- df2 %>% mutate(LOG_HRS_WRKD=log(HRS_WRKD))

test13<-feols(LOG_HRS_WRKD ~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2)

coef_df <- as.data.frame(confint(test13))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test13)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Coeff Plot for Colombian Log(Weekly Hours Worked) ",
       x = "Quarter",
      y = "P x Quarter")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical

##########################################################
#                   FORMAL LABORFORCE                    #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test2<-feols(THO_MONTHLY_WAGE~  PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
               PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
               PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
               COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE|DPTO +  Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test2))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test2)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Colombian Formal Sector: Monthly Wage",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical



###############################
#      LOG_WAGE DATASET       #
###############################

test21<-feols(LOG_WAGE~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
               PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
               PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE|DPTO + Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test21))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test21)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Colombian Formal Sector: Log (Monthly Wages)",
       x = "Quarter",
      y = "P x Quarter") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical

###############################
#      HRS_WRKD DATASET       #
###############################

test21<-feols(HRS_WRKD~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test21))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test21)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Colombian Formal Sector: Weekly Hours Worked",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###################################
#      LOG_HRS_WRKD DATASET       #
###################################
df2_formal <- df2_formal %>% mutate(LOG_HRS_WRKD=log(HRS_WRKD))

test13<-feols(LOG_HRS_WRKD ~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test13))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test13)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Colombian Formal Sector: Log (Weekly Hours Worked)",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###########################################################
#                   INFORMAL LABORFORCE                   #
###########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test26<-feols(THO_MONTHLY_WAGE~  PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
               PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
               PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO +  Time_FE,data=df2_informal)

coef_df <- as.data.frame(confint(test26))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test26)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Colombian Informal Sector: Monthly Wage",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical



###############################
#      LOG_WAGE DATASET       #
###############################

test25<-feols(LOG_WAGE~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE|DPTO + Time_FE,data=df2_informal)

coef_df <- as.data.frame(confint(test25))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test25)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +  
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Colombian Informal Sector: Log( Monthly Wages)",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###############################
#      HRS_WRKD DATASET       #
###############################

test24<-feols(HRS_WRKD~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2_informal)

coef_df <- as.data.frame(confint(test24))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test24)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Colombian Informal Sector: Weekly Hours Worked",
       x = "Quarter",
      y = "P x Quarter")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical


###################################
#      LOG_HRS_WRKD DATASET       #
###################################
df2_informal <- df2_informal %>% mutate(LOG_HRS_WRKD=log(HRS_WRKD))

test23<-feols(LOG_HRS_WRKD ~ PTP_QUARTER2018_1+PTP_QUARTER2018_2+PTP_QUARTER2018_3+PTP_QUARTER2018_4+PTP_QUARTER2019_1+PTP_QUARTER2019_2+PTP_QUARTER2019_3+PTP_QUARTER2019_4+
                PTP_QUARTER2020_1+PTP_QUARTER2020_2+PTP_QUARTER2020_3+PTP_QUARTER2020_4+PTP_QUARTER_ZERO+PTP_QUARTER2021_2+PTP_QUARTER2021_3+PTP_QUARTER2021_4+
                PTP_QUARTER2022_1+PTP_QUARTER2022_2+PTP_QUARTER2022_3+PTP_QUARTER2022_4+PTP_QUARTER2023_1+PTP_QUARTER2023_2+PTP_QUARTER2023_3+PTP_QUARTER2023_4+
                COVID:DPTO+ AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2_informal)

boot1<-boottest(test23,B=999,param = "PTP_QUARTER2018_1",clustid = "DPTO")
print(boot1)
boot2<-boottest(test23,B=999,param = "PTP_QUARTER2018_2",clustid = "DPTO")
print(boot2)
boot3<-boottest(test23,B=999,param = "PTP_QUARTER2018_3",clustid = "DPTO")
print(boot3)
boot4<-boottest(test23,B=999,param = "PTP_QUARTER2018_4",clustid = "DPTO")
print(boot4)
boot5<-boottest(test23,B=999,param = "PTP_QUARTER2019_1",clustid = "DPTO")
print(boot5)

coef_df <- as.data.frame(confint(test23))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^PTP_QUARTER", term)) %>%
  mutate(estimate = coef(test23)[term],
         term_clean = str_replace(term, "^PTP_QUARTER", ""))
coef_df$term_clean <- gsub("_", "-Q", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

# Identify the positions of DPTO_QUARTER_2 and DPTO_QUARTER_3
position_2 <- which(coef_df$term_clean == "2020-Q4")
position_3 <- which(coef_df$term_clean == "2021-Q2")

# Calculate the middle point between these two positions
PERMIT_position <- (position_2 + position_3) / 2

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() +
  labs(title = "Colombian Informal Sector Log (Weekly Hours Worked) ",
       x = "Quarter",
      y = "P x Quarter")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Make variable names vertical

#
