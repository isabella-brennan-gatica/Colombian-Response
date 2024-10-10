setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")

df_col_in_log <- read_dta ("Colombia_INFORMAL_log_regression.dta")
df1 <- df_col_in_log %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Colombian Log Wage")

df_col_in_wag <- read_dta ("Colombia_INFORMAL_MONTHLY_WAGE_regression.dta")
df2 <- df_col_in_wag %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Colombian Monthly Wage")

df_col_in_hrs <- read_dta ("Colombia_INFORMAL_HRS_WRK_regression.dta")
df3 <- df_col_in_hrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Colombian Hours per week")

df_col_in_loghrs <- read_dta ("Colombia_INFORMAL_LOG_HRS_WRK_regression.dta")
df13 <- df_col_in_loghrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Colombian Log(Hours per week)")


df_col_fo_log <- read_dta ("Colombia_FORMAL_log_regression.dta")
df4 <- df_col_fo_log %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Colombian log wage")


df_col_fo_wag <- read_dta ("Colombia_FORMAL_MONTHLY_WAGE_regression.dta")
df5 <- df_col_fo_wag %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Colombian monthly wage")

df_col_fo_hrs <-read_dta("Colombia_FORMAL_HRS_WRK_regression.dta")
df6 <- df_col_fo_hrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Colombian hrs per week")

df_col_fo_loghrs <- read_dta ("Colombia_FORMAL_LOG_HRS_WRK_regression.dta")
df14 <- df_col_fo_loghrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Colombian Log(Hours per week)")

df_mig_in_log <- read_dta ("Migrant_INFORMAL_log_regression.dta")
df7 <- df_mig_in_log %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Migrant log wage")

df_mig_in_wag <- read_dta ("Migrant_INFORMAL_MONTHLY_WAGE_regression.dta")
df8 <- df_mig_in_wag %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Migrant monthly wage")

df_mig_in_hrs <- read_dta ("Migrant_INFORMAL_HRS_WRK_regression.dta")
df9 <- df_mig_in_hrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Migrant Hrs worked")

df_mig_in_loghrs <- read_dta ("Migrant_INFORMAL_LOG_HRS_WRK_regression.dta")
df15 <- df_mig_in_loghrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Informal Migrant Log(Hours per week)")

df_mig_fo_log <- read_dta ("Migrant_FORMAL_log_regression.dta")
df10 <- df_mig_fo_log %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Migrant log wage")


df_mig_fo_wag <- read_dta ("Migrant_FORMAL_MONTHLY_WAGE_regression.dta")
df11 <- df_mig_fo_wag %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Migrant wage")

df_mig_fo_hrs <- read_dta ("Migrant_FORMAL_HRS_WRK_regression.dta")
df12 <- df_mig_fo_hrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Hrs Worked")

df_mig_fo_loghrs <- read_dta ("Migrant_FORMAL_LOG_HRS_WRK_regression.dta")
df16 <- df_mig_fo_loghrs %>%
  filter(term == "DID") %>%
  mutate(original_df = "Formal Migrant Log(Hours per week)")

# Combine the filtered rows into a new dataframe
new_df <- bind_rows(df1,df2,df3,df13,df4,df5,df6,df14,df7,df8,df9,df15,df10,df11,df12, df16)


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
write_dta(new_df, "Continuous_DID_Results_Merged.dta") 
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/")
#df_T_U <- read_dta ("TREATED_UNTREATED_DID_Results_Merged.dta")

#setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/CONTINUOUS/ANO2019/")
#df_T_U_19 <- read_dta ("TREATED_UNTREATED_2019_DID_Results_Merged.dta")

#setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/")
#df_T_DPTO <- read_dta ("ALL_DPTO_DID_Results_Merged.dta")

#setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/REGRESSION RESULTS/ALL_DPTO/ANO2019/")
#df_T_DPTO_19 <- read_dta ("ALL_DPTO_2019_DID_Results_Merged.dta")



