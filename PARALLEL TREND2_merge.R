rm(list=ls()) # clear workspace

setwd("C:/Users/.../Colombia/GEIH DATA/REGRESSION RESULTS/")
did_col<-read_dta("DID_Results_Merged.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")

params<-c("DPTO_QUARTER2018_1", "DPTO_QUARTER2018_2", "DPTO_QUARTER2018_3", "DPTO_QUARTER2018_4", "DPTO_QUARTER2019_1", "DPTO_QUARTER2019_2", "DPTO_QUARTER2019_3", "DPTO_QUARTER2019_4",
          "DPTO_QUARTER2020_1", "DPTO_QUARTER2020_2", "DPTO_QUARTER2020_3", "DPTO_QUARTER2020_4")

df_in_log <- read_dta ("INFORMAL_LOG_WAGE_DPTO_QUART_regression.dta")
df1 <- df_in_log %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Informal_Log_Wage"= "pvalue")
 

df_in_wag <- read_dta ("INFORMAL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta")
df2 <- df_in_wag %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Informal_Monthly_Wage"= "pvalue")

df_in_hrs <- read_dta ("INFORMAL_HRS_WRKD_DPTO_QUART_regression.dta")
df3 <- df_in_hrs %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Informal_Hours_week"= "pvalue")


df_fo_log <- read_dta ("FORMAL_LOG_WAGE_DPTO_QUART_regression.dta")
df4 <- df_fo_log %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Formal_log_wage"= "pvalue")


df_fo_wag <- read_dta ("FORMAL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta")
df5 <- df_fo_wag %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Formal_monthly_wage"= "pvalue")

df_fo_hrs <-read_dta("FORMAL_HRS_WRKD_DPTO_QUART_regression.dta")
df6 <- df_fo_hrs %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Formal_hrs_week"= "pvalue")

df_full_log <- read_dta ("FULL_LOG_WAGE_DPTO_QUART_regression.dta")
df7 <- df_fo_log %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Full_log_wage"= "pvalue")


df_full_wag <- read_dta ("FULL_THO_MONTHLY_WAGE_DPTO_QUART_regression.dta")
df8 <- df_fo_wag %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Full_monthly_wage"= "pvalue")

df_full_hrs <-read_dta("FULL_HRS_WRKD_DPTO_QUART_regression.dta")
df9 <- df_fo_hrs %>%
  filter(term %in% params) %>%   
  select(-estimate,-statistic,-Estimate,-StdERROR,-tVALUE)%>%
  rename("Full_hrs_week"= "pvalue")



# Combine the filtered rows into a new dataframe
merged_df <- merge(df1, df2, by = "term")
merged_df <- merge(merged_df, df3, by = "term")
merged_df <- merge(merged_df, df4, by = "term")
merged_df <- merge(merged_df, df5, by = "term")
merged_df <- merge(merged_df, df6, by = "term")
merged_df <- merge(merged_df, df7, by = "term")
merged_df <- merge(merged_df, df8, by = "term")
merged_df <- merge(merged_df, df9, by = "term")



setwd("C:/Users/.../Colombia/GEIH DATA/PARALLEL TREND ASSUMPTION/")
write_dta(merged_df, "parallel_Results_Merged.dta") 
setwd("C:/Users/.../Colombia/GEIH DATA/")
