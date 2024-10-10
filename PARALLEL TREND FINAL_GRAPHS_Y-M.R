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
library(stringr)
library(ggplot2)

#######################
#       EDIT          #
#     DATAFRAME       #
#######################

df<- read_dta("GEIH_REDUCED_2018_2023.dta")


#setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/DPTO CHAR/")
#df_covid<- read_dta( "COVID_2020.dta") 
#df_covid<- read_dta( "COVID_2020-23.dta")
#df_covid <- df_covid[df_covid$ANO != 2024, ]

#COVID_summary <- df_covid %>%
#  group_by(DPTO) %>%
#  summarise(Total_COVID_COUNT = sum(COVID_CASE, na.rm = TRUE))

#df_covid <- COVID_summary

#df_covid$ANO<-as.numeric(df_covid$ANO)
#df_covid$MES<-as.numeric(df_covid$MES)
#Left Join: Keeps all rows from the left data frame (df1) and adds matching rows from the right data frame (df2).
#merged_df <- left_join(df, df_covid, by = c("DPTO","ANO","MES"))
#merged_df$COVID <- ifelse(is.na(merged_df$COVID_CASE),0,merged_df$COVID_CASE)
#df<-merged_df


####################
#  CREATE TIME FE  #
####################
df$MES <- sprintf("%02d", df$MES)
df2 <- df %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))
#df2$COVID<-ifelse(df2$ANO=="2020" & df2$MES<="06",1,0)

df2_formal<- subset(df2, df2$FULL_FORMAL == 1)
df2_informal<- subset(df2, df2$FULL_INFORMAL == 1)

#df2_formal<- subset(df2_formal, df2_formal$ANO >="2019")
#df2_informal<- subset(df2_informal, df2_informal$ANO >="2019")
#df2<-subset(df2, df2$ANO>="2019")

#df2_formal$COVID<-ifelse(df2_formal$ANO=="2020" & df2_formal$MES<="06",1,0)
#df2_informal$COVID<-ifelse(df2_informal$ANO=="2020" & df2_informal$MES<="06",1,0)

#df2_formal <- df2_formal %>%
#  filter(ANO >= 2019)
#df2_informal <- df2_informal %>%
#  filter(ANO >= 2019)
##########################################################
#                   FULL LABORFORCE                      #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test_0<-feols(THO_MONTHLY_WAGE~ i(Time_FE, LOG_PTP, ref = "2021_03")+
              AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE + 
                covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  |DPTO + Time_FE,data=df2)
summary(test_0)
boot<-boottest(test_0, param="Time_FE::2021_01:LOG_PTP", B=999, clustid="DPTO")
print(boot)

boot<-boottest(test_0, param="Time_FE::2020_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.0531 
#confidence interval:-4e-04 0.0707 

boot<-boottest(test_0, param="Time_FE::2020_05:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.1271  
#confidence interval:-0.0053 0.0523 

coef_df <- as.data.frame(confint(test_0))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_0)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)

# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")


PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)


# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title = "Colombian FULL FORMAL + covid",
       x = "Monthly Time Period",
       y = "PPT x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PPT"))



##################################
#      HRS_WRKD DATASET       #
###################################

test_33_1<-feols(FULL_INFORMAL ~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +
                   DPTO_AGR_ANO_2021 + DPTO_AGR_ANO_2022 + DPTO_AGR_ANO_2023 +
                   DPTO_IND_ANO_2021 + DPTO_IND_ANO_2022 + DPTO_IND_ANO_2023 + 
                   DPTO_SERV_ANO_2021 + DPTO_SERV_ANO_2022 + DPTO_SERV_ANO_2023 + 
                   DPTO_HOMIC_ANO_2021 + DPTO_HOMIC_ANO_2022 + DPTO_HOMIC_ANO_2023 +
                   DPTO_UNSAT_ANO_2021 + DPTO_UNSAT_ANO_2022 + DPTO_UNSAT_ANO_2023 + 
                   DPTO_TERROR_ANO_2021 + DPTO_TERROR_ANO_2022 + DPTO_TERROR_ANO_2023 +
                   covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 |DPTO + Time_FE,data=df2)

boot<-boottest(test_33_1, param="Time_FE::2020_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.0591
#confidence interval:  -0.0751 0.0016  
#test statistic -1.7786

boot<-boottest(test_33_1, param="Time_FE::2020_05:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.1321
#confidence interval: -0.0531 0.0037 
#test statistic -1.976

boot<-boottest(test_33_1, param="Time_FE::2020_06:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.3413
#confidence interval: -0.0779 0.0529
#test statistic -1.69 

boot<-boottest(test_33_1, param="Time_FE::2021_01:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.3413
#confidence interval: -0.0779 0.0529
#test statistic -1.69 

coef_df <- as.data.frame(confint(test_33_1))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_33_1)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)


# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")


PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)


# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title = "Colombian Weekly FULL INFOMRAL + covid",
       x = "Monthly Time Period",
       y = "PTP x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PTP"))



##########################################################
#                   FORMAL LABORFORCE                    #
##########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test_10<-feols(THO_MONTHLY_WAGE~  i(Time_FE, LOG_PTP, ref = "2021_03")+
               AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023 
               |DPTO + Time_FE,data=df2_formal)

boot<-boottest(test_10, param="Time_FE::2018_06:LOG_PTP", B=999, clustid="DPTO")
print(boot)


coef_df <- as.data.frame(confint(test_10))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_10)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")


PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)


# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title = "Colombian Formal Sector: Monthly Wage",
       x = "Monthly Time Period",
       y = "PPT x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PPT"))



###############################
#      HRS_WRKD DATASET       #
###############################

test_12<-feols(HRS_WRKD~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE+
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023  |
                 DPTO + Time_FE,data=df2_formal)
summary(test_12)

boot<-boottest(test_12, param="Time_FE::2021_02:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.1491 
#confidence interval: -0.7847 0.0778 
#test statistic -1.393 
boot<-boottest(test_12, param="Time_FE::2020_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.5435  
#confidence interval: -1.5159 3.6386 
#test statistic 2.087 
boot<-boottest(test_12, param="Time_FE::2020_03:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.3123 
#confidence interval: -1.378 3.7806 
#test statistic 1.2643
boot<-boottest(test_12, param="Time_FE::2020_05:LOG_PTP", B=999, clustid="DPTO")
print(boot)
# value: 0.3864 
#confidence interval: -1.3225 3.567 
#test statistic 0.9541 
boot<-boottest(test_12, param="Time_FE::2019_07:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.0731  
#confidence interval:-1.028 0.0317  
#test statistic -1.6782  
boot<-boottest(test_12, param="Time_FE::2018_08:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.032
#confidence interval: -1.2943  -0.0833 
#test statistic -2.4983 

coef_df <- as.data.frame(confint(test_12))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_12)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)

# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)



# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title = "Colombian Formal Sector: Weekly Hours Worked",
       x = "Monthly Time Period",
       y = "PPT x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PPT"))



###########################################################
#                   INFORMAL LABORFORCE                   #
###########################################################

###############################
#  THO_MONTHLY_WAGE DATASET   #
###############################

test_26<-feols(THO_MONTHLY_WAGE~   i(Time_FE, LOG_PTP, ref = "2021_03")+
                AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE  +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023
               |DPTO + Time_FE,data=df2_informal)
boot<-boottest(test_26, param="Time_FE::2020_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.2292 
#confidence interval: -128.2827 230.1235

boot<-boottest(test_26, param="Time_FE::2020_03:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.1732 
#confidence interval:  -36.6976 152.7794 
#test statistic 1.6964  

boot<-boottest(test_26, param="Time_FE::2020_05:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.2332
#confidence interval: -20.1349 153.0224 
#test statistic 1.1746 

boot<-boottest(test_26, param="Time_FE::2019_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value: 0.0661 
#confidence interval:  -4.7164 74.2102  
#test statistic 2.5253 
boot<-boottest(test_26, param="Time_FE::2019_08:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value:  0.0901 
#confidence interval:  -10.2445 72.1084 
#test statistic 2.0714 
 
boot<-boottest(test_26, param="Time_FE::2018_04:LOG_PTP", B=999, clustid="DPTO")
print(boot)
#p value:  0.0621  
#confidence interval: -6.1172 98.122 
#test statistic 2.5365 

coef_df <- as.data.frame(confint(test_26))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_26)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")


PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)


# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title = "Colombian Informal Sector: Monthly Wage",
       x = "Monthly Time Period",
       y = "PPT x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PPT"))




###############################
#      HRS_WRKD DATASET       #
###############################

test_24<-feols(HRS_WRKD~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023
               |DPTO + Time_FE,data=df2_informal)


coef_df <- as.data.frame(confint(test_24))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_24)[term],
         term_clean = str_replace(term, "^Time_FE::", ""),
  )

# Remove ":TREATED" from the term column
coef_df$term_clean <- sub(":LOG_PTP", "", coef_df$term_clean)
# Rename columns for ggplot2 compatibility
colnames(coef_df) <- c("conf.low", "conf.high", "term", "estimate", "term_clean")

PTP_row <- data.frame(
  conf.low = 0,
  conf.high = 0,
  term = "2021_03",
  estimate = 0,
  term_clean = "2021_03"
)
coef_df <- rbind(
  coef_df[1:38, ],        # First part of the data frame (before the new row)
  PTP_row,                 # The new row to insert
    coef_df[39:47, ]         # Second part of the data frame (after the new row)
#  coef_df[39:70, ] 
)



# Calculate the middle point between these two positions
PERMIT_position <- which(coef_df$term_clean == "2021_03")

# Plot the coefficients
ggplot(coef_df, aes(x = term_clean, y = estimate)) +
  geom_point() +
  geom_line(aes(group = 1), color = "black") +
  geom_vline(xintercept = PERMIT_position, linetype = "solid", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+  
  labs(title = "Colombian Informal Sector: Weekly Hours Worked",
       x = "Monthly Time Period",
       y = "PPT x Monthly Time Period")+  theme(axis.text.x = element_text(angle = 90, hjust = 1))+# Make variable names vertical+
  theme(
    panel.grid = element_blank(),           # Remove all grid lines
    axis.line = element_line(),             # Add axis lines
    axis.line.x = element_line(color = "black"), # X-axis line
    axis.line.y = element_line(color = "black"), # Y-axis line
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text.x = element_text(color ="black", hjust = 0.5, vjust = .5),  # Set x-axis labels to black and center them
    axis.text.y = element_text(color = "black"),  # Optional: Set y-axis labels to black
    axis.ticks.x = element_line(color = "black", size = 0.5),  # Customize tick marks
  ) +
  geom_hline(yintercept = 0, color = "black")+  # Add a horizontal line at y = 0
  scale_x_discrete(labels = c("2021_03" = "PPT"))


