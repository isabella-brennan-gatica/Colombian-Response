
library(dplyr)
library(haven)

pop_total_1993_depcod <- GEIH_Rep %>%
  select(pop_total_1993_depcod, depcod) %>%
  distinct()
pop_vene_1993_depcod <- GEIH_Rep %>%
  select(pop_vene_1993_depcod, depcod) %>%
  distinct()
share_vene_93_1993_depcod<- GEIH_Rep %>%
  select(share_vene_93_1993_depcod, depcod) %>%
  distinct()

pop_vene_2005 <- GEIH_Rep %>%
  select(pop_vene_2005, depcod) %>%
  distinct()

early_ven05 <- GEIH_Rep %>%
  select(early_ven05, depcod) %>%
  distinct()

early_ven93<- GEIH_Rep %>%
  select(early_ven93, depcod) %>%
  distinct()

it2<- GEIH_Rep %>%
  select(it2, depcod) %>%
  filter(it2 != 0.00000000)%>%
  distinct()

it3<- GEIH_Rep %>%
  select(it3, depcod) %>%
  filter(it3 != 0.00000000)%>%
  distinct()

idist<- GEIH_Rep %>%
  select(idist, depcod) %>%
  distinct()


merged_df <- merge(pop_total_1993_depcod,pop_vene_1993_depcod,by = "depcod")
merged_df <- merge(merged_df,share_vene_93_1993_depcod,by = "depcod")
merged_df <- merge(merged_df,early_ven93,by = "depcod")
merged_df <- merge(merged_df,it2,by = "depcod")
merged_df <- merge(merged_df,early_ven05,by = "depcod")
merged_df <- merge(merged_df,pop_vene_2005,by = "depcod")
merged_df <- merge(merged_df,it3,by = "depcod")
merged_df <- merge(merged_df,idist,by = "depcod")

setwd("C:/Users/.../Colombia/DPTO CHAR/")

write_dta(merged_df, "Robustness.dta")

#######################################
#       ADD ROBUSTNESS CHECK          #
#           TO DATAFRAME              #
#######################################

data_robustness <- read_dta("Robustness.dta")
data_covid <- read_dta("COVID_2020-23.dta")

data_covid <- data_covid %>%
  filter(ANO == "2020") %>%  
  group_by(DPTO) %>%              
  summarise(COVID_CASE = sum(COVID_CASE)) 

data_robustness <- data_robustness %>% rename(DPTO = depcod)

#it2=ven.settlements 1993 x I (post august 2018)
#it3=ven.settlements 2005 x I (post august 2018)
#it1=Average registration days x I (post august 2018)

data_robustness$DPTO<-as.numeric(data_robustness$DPTO)

setwd("C:/Users/.../Colombia/DPTO CHAR/")

write_dta(data_robustness, "Robustness.dta")
#######################
#       EDIT          #
#     DATAFRAME       #
#######################

setwd("C:/Users/.../Colombia/GEIH DATA/")
df <- read_dta("GEIH_REDUCED_2018_2023.dta")

df <- merge(df, data_robustness, by = "DPTO", all = TRUE)
df <- merge(df, data_covid, by = "DPTO", all = TRUE)

df$covid_ANO_2021 <- df$COVID_CASE * df$ANO_2021
df$covid_ANO_2022 <- df$COVID_CASE * df$ANO_2022
df$covid_ANO_2023 <- df$COVID_CASE * df$ANO_2023

write_dta(df, "GEIH_REDUCED_2018_2023.dta")

df2 <- read_dta("Colombian_informal.dta")
df2 <- merge(df2, data_robustness, by = "DPTO", all = TRUE)
df2 <- merge(df2, data_covid, by = "DPTO", all = TRUE)
df2$covid_ANO_2021 <- df2$COVID_CASE * df2$ANO_2021
df2$covid_ANO_2022 <- df2$COVID_CASE * df2$ANO_2022
df2$covid_ANO_2023 <- df2$COVID_CASE * df2$ANO_2023

write_dta(df2, "Colombian_informal.dta")

df3 <- read_dta("Migrant_informal.dta")
df3 <- merge(df3, data_robustness, by = "DPTO", all = TRUE)
df3 <- merge(df3, data_covid, by = "DPTO", all = TRUE)
df3$covid_ANO_2021 <- df3$COVID_CASE * df3$ANO_2021
df3$covid_ANO_2022 <- df3$COVID_CASE * df3$ANO_2022
df3$covid_ANO_2023 <- df3$COVID_CASE * df3$ANO_2023
write_dta(df3, "Migrant_informal.dta")

df4 <- read_dta("Colombian_formal.dta")
df4 <- merge(df4, data_robustness, by = "DPTO", all = TRUE)
df4 <- merge(df4, data_covid, by = "DPTO", all = TRUE)
df4$covid_ANO_2021 <- df4$COVID_CASE * df4$ANO_2021
df4$covid_ANO_2022 <- df4$COVID_CASE * df4$ANO_2022
df4$covid_ANO_2023 <- df4$COVID_CASE * df4$ANO_2023
write_dta(df4, "Colombian_formal.dta")

df5 <- read_dta("Migrant_formal.dta")
df5 <- merge(df5, data_robustness, by = "DPTO", all = TRUE)
df5 <- merge(df5, data_covid, by = "DPTO", all = TRUE)
df5$covid_ANO_2021 <- df5$COVID_CASE * df5$ANO_2021
df5$covid_ANO_2022 <- df5$COVID_CASE * df5$ANO_2022
df5$covid_ANO_2023 <- df5$COVID_CASE * df5$ANO_2023
write_dta(df5, "Migrant_formal.dta")

df$pop_vene_2005_ANO_2021 <- df$pop_vene_2005 * df$ANO_2021
df$pop_vene_2005_ANO_2022 <- df$pop_vene_2005 * df$ANO_2022
df$pop_vene_2005_ANO_2023 <- df$pop_vene_2005 * df$ANO_2023

df$pop_vene_1993_ANO_2021 <- df$pop_vene_1993_depcod * df$ANO_2021
df$pop_vene_1993_ANO_2022 <- df$pop_vene_1993_depcod * df$ANO_2022
df$pop_vene_1993_ANO_2023 <- df$pop_vene_1993_depcod * df$ANO_2023

write_dta(df, "GEIH_REDUCED_2018_2023.dta")


df2$pop_vene_2005_ANO_2021 <- df2$pop_vene_2005 * df2$ANO_2021
df2$pop_vene_2005_ANO_2022 <- df2$pop_vene_2005 * df2$ANO_2022
df2$pop_vene_2005_ANO_2023 <- df2$pop_vene_2005 * df2$ANO_2023

df2$pop_vene_1993_ANO_2021 <- df2$pop_vene_1993_depcod * df2$ANO_2021
df2$pop_vene_1993_ANO_2022 <- df2$pop_vene_1993_depcod * df2$ANO_2022
df2$pop_vene_1993_ANO_2023 <- df2$pop_vene_1993_depcod * df2$ANO_2023

write_dta(df2, "Colombian_informal.dta")


df3$pop_vene_2005_ANO_2021 <- df3$pop_vene_2005 * df3$ANO_2021
df3$pop_vene_2005_ANO_2022 <- df3$pop_vene_2005 * df3$ANO_2022
df3$pop_vene_2005_ANO_2023 <- df3$pop_vene_2005 * df3$ANO_2023

df3$pop_vene_1993_ANO_2021 <- df3$pop_vene_1993_depcod * df3$ANO_2021
df3$pop_vene_1993_ANO_2022 <- df3$pop_vene_1993_depcod * df3$ANO_2022
df3$pop_vene_1993_ANO_2023 <- df3$pop_vene_1993_depcod * df3$ANO_2023

write_dta(df3, "Migrant_informal.dta")


df4$pop_vene_2005_ANO_2021 <- df4$pop_vene_2005 * df4$ANO_2021
df4$pop_vene_2005_ANO_2022 <- df4$pop_vene_2005 * df4$ANO_2022
df4$pop_vene_2005_ANO_2023 <- df4$pop_vene_2005 * df4$ANO_2023

df4$pop_vene_1993_ANO_2021 <- df4$pop_vene_1993_depcod * df4$ANO_2021
df4$pop_vene_1993_ANO_2022 <- df4$pop_vene_1993_depcod * df4$ANO_2022
df4$pop_vene_1993_ANO_2023 <- df4$pop_vene_1993_depcod * df4$ANO_2023

write_dta(df4, "Colombian_formal.dta")


df5$pop_vene_2005_ANO_2021 <- df5$pop_vene_2005 * df5$ANO_2021
df5$pop_vene_2005_ANO_2022 <- df5$pop_vene_2005 * df5$ANO_2022
df5$pop_vene_2005_ANO_2023 <- df5$pop_vene_2005 * df5$ANO_2023

df5$pop_vene_1993_ANO_2021 <- df5$pop_vene_1993_depcod * df5$ANO_2021
df5$pop_vene_1993_ANO_2022 <- df5$pop_vene_1993_depcod * df5$ANO_2022
df5$pop_vene_1993_ANO_2023 <- df5$pop_vene_1993_depcod * df5$ANO_2023

write_dta(df5, "Migrant_formal.dta")
