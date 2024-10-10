#install.packages("purrr")
#install.packages("openxlsx")
#install.packages("Hmisc")
rm(list=ls()) # clear workspace
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

library(haven) #read dataframes
library(dplyr) #modify columns
library(purrr) #merge dataframes by columns
library(openxlsx) #save dataframe as excel 
library(Hmisc)

df <- read_dta("GEIH_2018_2023.dta")

###################################
#       LABOR FORCE VARIABLES     #
###################################
#LABOR FORCE
df$LABORFORCE<- 1


###########################################
#       DROP INCOMPLETE DEPARTMENTS       #
###########################################
# 81	ARAUCA
# 85	CASANARE
# 86	PUTUMAYO
# 88	SAN ANDRES
# 91	AMAZONAS
# 94	GUAINIA
# 95	GUAVIARE
# 97	VAUPES
# 99	VICHADA
df$DPTO<-as.numeric(df$DPTO)

df <- subset(df, DPTO != 81)
df <- subset(df, DPTO != 85)
df <- subset(df, DPTO != 86)
df <- subset(df, DPTO != 88)
df <- subset(df, DPTO != 91)
df <- subset(df, DPTO != 94)
df <- subset(df, DPTO != 95)
df <- subset(df, DPTO != 97)
df <- subset(df, DPTO != 99)

#######################################
#       DROP ANYONE LESS THAN 15      #
#######################################
# Filter out rows where age is greater than or equal to 15
df <- df %>%
  filter(df$P6040 >= 15)
# Filter out rows where age is greater than or equal to 64
df <- df %>%
  filter(df$P6040 <= 64)

#######################
#    Descriptive      #
#    STATISTICS       #
#######################

#How many Venezuelans
count_Venezuelans <- sum(df$P756S3 == 3, na.rm = TRUE)
print(count_Venezuelans)
# 108453

#How many Venezuelan Migrants
#P755 <- LIVED IN ANOTHER COUNTRY 5 YRS AGO IF = 4
#P3373S3A1 <- IN WHAT YEAR DID YOU ARRIVE IN cOLOMBIA <- VALID FOR 2022 &2023 QUESTIONS

count_Venezuelans_migrants <- sum(df$P756S3 == 3 & (df$P755==4|df$P3373S3A1>=2013), na.rm = TRUE)
print(count_Venezuelans_migrants)
#100796

#How many Colombians
count_Colombians <- sum(df$P756 == 1,df$P756 == 2, na.rm = TRUE)
print(count_Colombians)
#[1] 2623520

#"Would U take a salaried Job?"
count_Salaried <- sum(df$P1805 == 1, na.rm = TRUE)
print(count_Salaried)
#[1] 122520



###############
##INFORMALITY##
###############

#P6920<- "Contribute to PENSION?" 1. yes 2. no 3. im a pensioner
#P6930<- "wHICH PENSION FUND" 1. Fondo privado? 2. ISS, Cajanal? 3.Regímenes especiales (FFMM, Ecopetrol etc)? 4.Fondo Subsidiado (Prosperar,etc.)?
#P6940<- "who pays?" 1. part employer 2. full me 3. total employer 4. dont pay

#"COUNT PENSION CONTRIBUTORS ?"
count_PENSION <- sum(df$P6920 == 1, na.rm = TRUE)
print(count_PENSION)
# 742318
#"HOW MANY GET PENSION FULLY OR PARTIALLY PAID BY WORK?"
count_PENSION_WRK <- sum(df$P6920 == 1& (df$P6940==1|df$P6100==3), na.rm = TRUE)
print(count_PENSION_WRK)
# 588897

#P6090 <-"Contribute to health system?" 1 SI 2 NO
#P6100 <- 1.Contributivo (EPS)? 2. Especial ? (Fuerzas Armadas, Ecopetrol,universidades públicas) 3. Subsidiado (EPS-S) 4. No sabe, no informa
#P6110 <- "who pays?" 1. part employer 2. discount from pension 3. full me 4. total employer 5. dont pay 6. idk

#"HOW MANY GET HEALTHCARE FULLY OR PARTIALLY PAID BY WORK?"
count_HealthCare_WRK <- sum(df$P6090==1 & (df$P6100==2|df$P6110==1|df$P6110==4), na.rm = TRUE)
print(count_HealthCare_WRK)
# 690385

#HOW MANY GET BOTH HEALTHCARE AND PENSION PARTIALLY OR FULLY PAID BY EMPLOYER
count_formal <- sum((df$P6090==1 & (df$P6100==2|df$P6110==1|df$P6110==2|df$P6110==4)) & (df$P6920 == 1 & (df$P6940==1|df$P6100==3)), na.rm = TRUE)
print(count_formal)
#576913


#HOW MANY GET DON'T GET BOTH HEALTHCARE AND PENSION PARTIALLY OR FULLY PAID BY EMPLOYER
count_informal <- sum((df$P6090==2 |df$P6110==3|df$P6110==5)  |df$P6090==9 | (df$P6920 == 2|df$P6940==2|df$P6940==4), na.rm = TRUE)
print(count_informal)
# 1523036

#######################
#    Descriptive      #
#    Variables        #
#######################

df$GENDER = ifelse(df$P6020==1,1,0)
# 1 == MALE

df$VENEZUELAN <- ifelse(df$P756S3 == 3, 1,0)
df$MIGRANT <- ifelse(df$P756S3 == 3 & (df$P755==4|df$P3373S3A1>=2013),1,0)
df$COLOMBIAN=ifelse(df$P756 == 1 | df$P756 == 2, 1, 0)
df$AGE = df$P6040
df$COHABITATE = ifelse(df$P6070 == 3 | df$P6070 == 2| df$P6070 == 1, 1, 0)
#3 == MARRIED, 2 & 1 == LIVING WITH PARTNER
df$EDUC = df$EDUC
#YEARS OF EDUCATION
df$JEFE = ifelse(df$P6050==1,1,0)
#JEFE = HEAD OF HOUSEHOLD

df$URBAN = ifelse(df$CLASE==1,1,0)
#Urban == 1 and Rural == 2

#P6920<- "Contribute to PENSION?" 1. yes 2. no 3. im a pensioner
#P6930<- "WHICH PENSION FUND?" 1. Fondo privado? 2. ISS, Cajanal? 3.Regímenes especiales (FFMM, Ecopetrol etc)? 4.Fondo Subsidiado (Prosperar,etc.)?
#P6940<- "who pays?" 1. part employer 2. full me 3. total employer 4. dont pay

#P6090 <-"Contribute to HEALTH SYSTEM?" 1 SI 2 NO
#P6100 <- 1.Contributivo (EPS)? 2. Especial ? (Fuerzas Armadas, Ecopetrol,universidades públicas) 3. Subsidiado (EPS-S) 4. No sabe, no informa
#P6110 <- "who pays?" 1. part employer 2. discount from pension 3. full me 4. total employer 5. dont pay 6. idk

df$FULL_FORMAL <- ifelse((df$P6090==1 & (df$P6100==2|df$P6110==1|df$P6110==2|df$P6110==4)) & (df$P6920 == 1 & (df$P6940==1|df$P6100==3)),1,0)
count_FORMAL_WORKERS <- sum(df$FULL_FORMAL == 1, na.rm = TRUE)
print(count_FORMAL_WORKERS) 
#576913

#HEALTHCARE BUT NO PENSION PROVIDED
df$H_PARTIAL_FORMAL <- ifelse((df$P6090==1 & (df$P6100==2|df$P6110==1|df$P6110==4))& (df$P6920 == 2 | df$P6110 == 3 | df$P6110 == 5),1,0)
count_H_PARTIAL_FORMAL <- sum(df$H_PARTIAL_FORMAL == 1, na.rm = TRUE)
print(count_H_PARTIAL_FORMAL) 
#48640

#PENSION BUT NO HEALTHCARE PROVIDED
df$P_PARTIAL_FORMAL <- ifelse((df$P6920 == 1 & (df$P6940==1|df$P6100==3)) & (df$P6090==2 | df$P6110 ==3 | df$P6110 ==5),1,0)
count_P_PARTIAL_FORMAL <- sum(df$P_PARTIAL_FORMAL == 1, na.rm = TRUE)
print(count_P_PARTIAL_FORMAL) 
#7294

#EITHER PENSION HOLDERS OR HEALTHCARE HOLDERS FROM EMPLOYER
df$PARTIAL_FORMAL <- ifelse(df$H_PARTIAL_FORMAL==1 | df$P_PARTIAL_FORMAL == 1, 1,0)
count_PARTIAL_FORMAL <- sum(df$PARTIAL_FORMAL == 1, na.rm = TRUE)
print(count_PARTIAL_FORMAL) 


#IF NOT FULLY FORMAL THEN MARK AS INFORMAL
df$FULL_INFORMAL <- ifelse((df$P6090==1 & (df$P6100==2|df$P6110==1|df$P6110==4)) & (df$P6920 == 1 & (df$P6940==1|df$P6100==3)),0,1)
                  

count_INFORMAL_WORKERS <- sum(df$FULL_INFORMAL == 1, na.rm = TRUE)
print(count_INFORMAL_WORKERS)
#1604038


######################################
#       ORGANIZE INCOME & WORK       #
######################################

#(df$P6500) <- "Pre-tax Wage"
#   Si no recibió salario en dinero, escriba 00; 
#   si recibió pero no sabe el monto, 
#   escriba 98; 
#   si no sabe si recibió, escriba 99.
#(df$P7070) <- "Wage"
#(df$P6750) <- "Net Profit"

df$P6500= ifelse(df$P6500==98|df$P6500==99,NA,df$P6500)

#CLEANING UP WAGE SITUATION
df$MONTHLY_WAGE= df$INGLABO 

df <- df %>%
  mutate(
    MONTHLY_WAGE = if_else(is.na(P6500) & is.na(P7070) & is.na(P6750), NA_real_, 
                   rowSums(select(., P6500,P7070,P6750), na.rm = TRUE))
  )


df$LOG_WAGE = log(df$MONTHLY_WAGE)

df$THO_MONTHLY_WAGE=df$MONTHLY_WAGE/1000

df$HRS_WRKD = df$P6800
df <- df %>%
  mutate(
    HRS_WRKD = if_else(HRS_WRKD==98,NA,HRS_WRKD),
    HRS_WRKD = if_else(HRS_WRKD==99,0,HRS_WRKD)
  )
df <- df %>% mutate(LOG_HRS_WRKD=log(HRS_WRKD))
df$LOG_HRS_WRKD <- ifelse(df$LOG_HRS_WRKD=="-Inf",NA,df$LOG_HRS_WRKD)
######################################
#        MINI COUNT DATAFRAMES       #
######################################

#COUNT VENEZUELANS BY DEPARTMENT AND YEAR
Venezuelans_by_DPTO_YR <- df %>%
  filter(!is.na(MIGRANT)) %>%  # Remove NA values in 'value'
  group_by(ANO, DPTO) %>%
  summarise(count_of_V = sum(MIGRANT == 1))

#COUNT VENEZUELANS BY DEPARTMENT
Venezuelans_by_DPTO <- df %>%
  filter(!is.na(MIGRANT)) %>%  # Remove NA values in 'value'
  group_by(DPTO) %>%
  summarise(count_of_V = sum(MIGRANT == 1))

print(Venezuelans_by_DPTO)

#COUNT VENEZUELANS BY YEAR
Venezuelans_by_YEAR <- df %>%
  filter(!is.na(MIGRANT)) %>%  # Remove NA values in 'value'
  group_by(ANO) %>%
  summarise(count_of_V = sum(MIGRANT== 1))


#COUNT FORMALITY BY DEPARTMENT
FORMAL_by_DPTO <- df %>%
  filter(!is.na(FULL_FORMAL)) %>%  # Remove NA values in 'value'
  group_by(DPTO) %>%
  summarise(count_of_FORMAL = sum(FULL_FORMAL == 1))

print(FORMAL_by_DPTO)
###############################
#       TREATMENT DUMMY       #
###############################

# Create a dummy variable to identify the group exposed to the treatment. 
# Lets assumed that departments with code 5,8,11,13,20,44,47,54,68,76,25(=1). 
# Departments 15,17,18,19,23,25,27,41,52,70,73 were not treated (=0).

#df$TREATED = ifelse(df$DPTO == 5 | df$DPTO ==  8 | df$DPTO == 11 | df$DPTO == 13 |  df$DPTO == 44 | 
#                      df$DPTO == 47  | df$DPTO == 54 | df$DPTO == 68 | df$DPTO == 76| df$DPTO == 25|df$DPTO == 20, 1, 0)

#df$UNTREATED = ifelse(df$DPTO == 15 | df$DPTO == 17 | df$DPTO == 19 | df$DPTO == 23 | df$DPTO == 50 | 
#                     df$DPTO == 41 | df$DPTO == 52 | df$DPTO == 63 | df$DPTO == 73 | df$DPTO == 66 | df$DPTO == 70, 1, 0)

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/DPTO CHAR/")

data_ptp <- read_dta("Col_DPTO_ptp_pop.dta")

df <- merge(df, data_ptp, by = "DPTO", all = TRUE)


##################################
#       INTERVENTION DUMMY       #
##################################

#Create a dummy variable to indicate the time when the treatment started. Lets assume that treatment started in 2020.
#In this case, years before MARCH 2021 will have a value of 0 and MARCH 2021 a 1. 
df$MES = as.numeric(df$MES)
df$ANO<-as.numeric(df$ANO)
df$Policy=ifelse((df$ANO==2021 & df$MES>= 3)|(df$ANO>=2022),1,0)

#####################################
#       INTERACTION TERM: DID       #
#####################################

#Create an interaction between time and treated
df$DID = df$Policy * df$LOG_PTP
#######################################
#       ADD DPTO DESCRIPTORS          #
#           TO DATAFRAME              #
#######################################

#GINI INDEX 2015

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/DPTO CHAR/")

data_agr_gdp <- read.xlsx("GDP_AGR_2017_COL.xlsx")
data_serv_gdp <- read.xlsx("GDP_SERV_2017_COL.xlsx")
data_ind_gdp <- read.xlsx("GDP_IND_2017_COL.xlsx")
data_gini <- read.xlsx("GINI_2015_COL.xlsx") 
data_homic <- read.xlsx("HOMIC_2017_COL.xlsx")
data_unsat <- read.xlsx("UNSAT_2005_COL.xlsx") 
data_terror <- read.xlsx("TERROR_2015_COL.xlsx") 
data_info <- read_dta("INFO_2005_COL.dta") 

data_info$dpto<-as.numeric(data_info$dpto)
data_terror$dpto<-as.numeric(data_terror$dpto)
data_unsat$dpto<-as.numeric(data_unsat$dpto)
data_homic$dpto<-as.numeric(data_homic$dpto)
data_gini$dpto<-as.numeric(data_gini$dpto)
data_agr_gdp$dpto<-as.numeric(data_agr_gdp$dpto)
data_serv_gdp$dpto<-as.numeric(data_serv_gdp$dpto)
data_ind_gdp$dpto<-as.numeric(data_ind_gdp$dpto)

# Merge the dataframes by the 'dpto' column
DPTO_df <- merge(data_agr_gdp, data_serv_gdp, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_ind_gdp, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_gini, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_homic, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_unsat, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_terror, by = "dpto", all = TRUE)
DPTO_df <- merge(DPTO_df, data_info,  by = "dpto", all = TRUE)


DPTO_df$terror<-ifelse(is.na(DPTO_df$terror),0,DPTO_df$terror)



DPTO_df <- subset(DPTO_df, dpto != 81)
DPTO_df <- subset(DPTO_df, dpto != 85)
DPTO_df <- subset(DPTO_df, dpto != 86)
DPTO_df <- subset(DPTO_df, dpto != 88)
DPTO_df <- subset(DPTO_df, dpto != 91)
DPTO_df <- subset(DPTO_df, dpto != 94) 
DPTO_df <- subset(DPTO_df, dpto != 95)
DPTO_df <- subset(DPTO_df, dpto != 97)
DPTO_df <- subset(DPTO_df, dpto != 99) 


DPTO_df <- DPTO_df %>% rename(DPTO = dpto)
DPTO_df <- DPTO_df %>% rename(DPTO_AGR = gdp_agr)
DPTO_df <- DPTO_df %>% rename(DPTO_IND = gdp_ind)
DPTO_df <- DPTO_df %>% rename(DPTO_SERV = gdp_serv)
DPTO_df <- DPTO_df %>% rename(DPTO_GINI = gini)
DPTO_df <- DPTO_df %>% rename(DPTO_HOMIC = homic)
DPTO_df <- DPTO_df %>% rename(DPTO_UNSAT = unsat)
DPTO_df <- DPTO_df %>% rename(DPTO_TERROR = terror)

df <- merge(df, DPTO_df, by = "DPTO", all = TRUE)
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/DESCRIPTIVE STATS/")
#SAVE AS DTA FILE
write_dta(DPTO_df, "Col_DPTO_DF.dta")

#######################
#    REORGANIZED      #
#     DATAFRAME       #
#######################

df <- df %>% select(ID,DIRECTORIO,SECUENCIA_P,ORDEN,MES,ANO,DPTO,URBAN, LOG_WAGE, THO_MONTHLY_WAGE,INGLABO,MONTHLY_WAGE, HRS_WRKD, GENDER,VENEZUELAN,MIGRANT,AGE,COHABITATE,EDUC,
                    JEFE, FULL_FORMAL,PARTIAL_FORMAL,H_PARTIAL_FORMAL,P_PARTIAL_FORMAL,FULL_INFORMAL,COLOMBIAN, PTP2021,PTP2022,PTP,POP,DPTO,PTP70,PTP70_POP,LOG_PTP,LOG_HRS_WRKD,Policy, DID,LABORFORCE, DPTO_AGR,DPTO_IND,DPTO_SERV, DPTO_GINI, DPTO_HOMIC, DPTO_UNSAT, DPTO_TERROR, DPTO_INFO,
                    P6500,P7070,P6750, P6020, P6030S3,P6050, P6080, P6210S1,P6220,P756,P6070,P6090, P6100,   
                    P756S3,P6440,P6450,P6460,P6460S1,P6424S1,P6424S2,P6424S3,
                    P6426,P6510,P6510S1,P6510S2,P6765,P6760,
                    P550,P6780,P1879,P1805,P6790,P6800,P6850,P6880,P7040,
                    P7045,P7050,P7130,P514)



#data<- df[!is.na(df$LOG_WAGE), ]

data <- df %>% select(ID,DIRECTORIO,SECUENCIA_P,ORDEN,MES,ANO,DPTO,URBAN,INGLABO,LOG_WAGE,MONTHLY_WAGE,THO_MONTHLY_WAGE, HRS_WRKD, 
                          GENDER,VENEZUELAN, MIGRANT,COLOMBIAN, AGE,COHABITATE,EDUC,JEFE, COHABITATE,FULL_FORMAL,PARTIAL_FORMAL,H_PARTIAL_FORMAL,P_PARTIAL_FORMAL,FULL_INFORMAL,
                      PTP2021,PTP2022,PTP,POP,DPTO,PTP70,PTP70_POP,LOG_PTP,LOG_HRS_WRKD,Policy,
                      DID,LABORFORCE, DPTO_AGR,DPTO_IND,DPTO_SERV, DPTO_GINI, DPTO_HOMIC, DPTO_UNSAT, DPTO_TERROR,DPTO_INFO)



#######################
#       SAVE          #
#     DATAFRAME       #
#######################

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#SAVE AS DTA FILE
write_dta(data, "GEIH_REDUCED_2018_2023.dta")


#SAVE AS DTA FILE
write_dta(df, "GEIH_LONG_2018_2023.dta")


#######################
#       EDIT          #
#     DATAFRAME       #
#######################


df <- read_dta("GEIH_REDUCED_2018_2023.dta")


##############################
#  CREATE TIME FIXED EFFECT  #
##############################
df <- df %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df <- df %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

########################
#  CREATE ANO DUMMIES  #
########################

library(fastDummies)
data_with_ANO_dummies <- dummy_cols(df, select_columns = "ANO",remove_first_dummy = FALSE)

variables <- c("DPTO_AGR","DPTO_IND","DPTO_SERV","DPTO_GINI", "DPTO_HOMIC", "DPTO_UNSAT", "DPTO_TERROR")

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

#DATA informal :Colombian
df_in_col <- subset(df_in, df_in$COLOMBIAN == 1)
#DATA informal :Migrant
df_in_ven <- subset(df_in, df_in$MIGRANT == 1)

setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#SAVE AS DTA FILE
write_dta(df_in_col, "Colombian_informal.dta")
write_dta(df_in_ven, "Migrant_informal.dta")

df_fo <- subset(df, df$FULL_FORMAL == 1)

#DATA formal :Colombian
df_fo_col <- subset(df_fo, df_fo$COLOMBIAN == 1)
#DATA formal :Migrant
df_fo_ven <- subset(df_fo, df_fo$MIGRANT == 1)

#SAVE AS DTA FILE
write_dta(df_fo_col, "Colombian_formal.dta")
write_dta(df_fo_ven, "Migrant_formal.dta")

write_dta(df, "GEIH_REDUCED_2018_2023.dta")


