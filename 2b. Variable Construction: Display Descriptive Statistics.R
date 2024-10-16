rm(list=ls()) # clear workspace
setwd("C:/Users/.../Colombia/GEIH DATA/")

library(haven) #read dataframes
library(dplyr) #modify columns


data <- read_dta("GEIH_REDUCED_2016_2023.dta")

data$EMPLOYED= ifelse(data$MONTHLY_WAGE!=0,1,0)
data$HRS_WRKD<-as.numeric(data$HRS_WRKD)

#############################################################
#                           COLOMBIAN                       #
#############################################################

#######################################
#               FORMAL                #
#######################################
F_COL_data <- data %>%
  filter(COLOMBIAN == 1 & AGE <= 64 & FORMAL == 1)

############
# HRS_WRKD #
############

# Filter data where column 'COLOMBIAN' is 1 AND 'FORMAL' is 1 and then summarize column 'HRS_WRKD'

F_C_HRS_WRKD <- F_COL_data %>%
  summarise(
    variable = "COLOMBIAN : FORMAL HRS WORKED",
    mean = mean(HRS_WRKD, na.rm = TRUE),
    sd = sd(HRS_WRKD, na.rm = TRUE),
    observations = sum(HRS_WRKD!=0, na.rm = TRUE)
  )


####################
# THO_MONTHLY_WAGE #
####################

F_C_WAGE <- F_COL_data %>%
  summarise(
    variable = "COLOMBIAN : FORMAL MONTHLY WAGE (THOUSAND)",
    mean = mean(THO_MONTHLY_WAGE, na.rm = TRUE),
    sd = sd(THO_MONTHLY_WAGE, na.rm = TRUE),
    observations= sum(THO_MONTHLY_WAGE!=0, na.rm = TRUE)
  )


####################
#     EMPLOYED     #
####################

F_COL_data <- data %>%
  filter(COLOMBIAN == 1 & AGE <= 64 & FORMAL == 1 & MONTHLY_WAGE>0)

  
F_C_EMPLYD <- F_COL_data %>%
  summarise(
    variable = "COLOMBIAN : FORMAL EMPLOYED [FORMAL SECTOR]",
    mean = mean(EMPLOYED, na.rm = TRUE),
    sd = sd(EMPLOYED, na.rm = TRUE),
    observations= sum(EMPLOYED!=0, na.rm = TRUE)
  )


#######################################
#               INFORMAL              #
#######################################

I_COL_data <- data %>%
  filter(COLOMBIAN == 1 & AGE <= 64 & INFORMAL == 1)

############
# HRS_WRKD #
############

# Filter data where column 'COLOMBIAN' is 1 AND 'INFORMAL' is 1 and then summarize column 'HRS_WRKD'

I_C_HRS_WRKD <- I_COL_data %>%
  summarise(
    variable = "COLOMBIAN : INFORMAL HRS WRKD",
    mean = mean(HRS_WRKD, na.rm = TRUE),
    sd = sd(HRS_WRKD, na.rm = TRUE),
    observations= sum(HRS_WRKD!=0, na.rm = TRUE)
  )

####################
# THO_MONTHLY_WAGE #
####################

I_C_WAGE <- I_COL_data %>%
  summarise(
    variable = "COLOMBIAN : INFORMAL MONTHLY WAGE (THOUSAND)",
    mean = mean(THO_MONTHLY_WAGE, na.rm = TRUE),
    sd = sd(THO_MONTHLY_WAGE, na.rm = TRUE),
    observations= sum(THO_MONTHLY_WAGE!=0, na.rm = TRUE)
  )

####################
#     EMPLOYED     #
####################

I_COL_data <- data %>%
  filter(COLOMBIAN == 1 & AGE <= 64 & INFORMAL == 1 & MONTHLY_WAGE>0)

I_C_EMPLYD <- I_COL_data %>%
  summarise(
    variable = "COLOMBIAN : INFORMAL EMPLOYED [INFORMAL SECTOR]",
    mean = mean(EMPLOYED, na.rm = TRUE),
    sd = sd(EMPLOYED, na.rm = TRUE),
    observations= sum(EMPLOYED!=0, na.rm = TRUE)
  )


#########################################################
#                         LABOR FORCE                   #
#########################################################

COL_data <- data %>%
  filter(COLOMBIAN == 1 & AGE <= 64)

C_LBRFRC <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : Labor Force",
    mean = mean(COLOMBIAN, na.rm = TRUE),
    sd = sd(COLOMBIAN, na.rm = TRUE),
    observations= sum(!is.na(COLOMBIAN))
  )

#########################################################
#                         URBAN                         #
#########################################################

C_URBAN <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : URBAN",
    mean = mean(URBAN, na.rm = TRUE),
    sd = sd(URBAN, na.rm = TRUE),
    observations= sum(!is.na(URBAN))
  )

#########################################################
#                     GENDER [ 1= MALE]                 #
#########################################################

C_GENDER <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : GENDER [1 = MALE]",
    mean = mean(GENDER, na.rm = TRUE),
    sd = sd(GENDER, na.rm = TRUE),
    observations= sum(COL_data$GENDER==1,COL_data$GENDER==0, na.rm = TRUE)
  )

#########################################################
#                          AGE                          #
#########################################################

C_AGE <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : AGE",
    mean = mean(AGE, na.rm = TRUE),
    sd = sd(AGE, na.rm = TRUE),
    observations= sum(!is.na(AGE))
  )

#########################################################
#                         COHABITATE                    #
#########################################################


C_COH <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : Cohabitate",
    mean = mean(COHABITATE, na.rm = TRUE),
    sd = sd(COHABITATE, na.rm = TRUE),
    observations= sum(!is.na(COHABITATE))
  )

#########################################################
#                   HEAD OF HOUSEHOLD                   #
#########################################################


C_JEFE <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : HEAD OF HOUSEHOLD",
    mean = mean(JEFE, na.rm = TRUE),
    sd = sd(JEFE, na.rm = TRUE),
    observations= sum(!is.na(JEFE))
  )

#########################################################
#                     YRS EDUCATION                     #
#########################################################


C_EDUC <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : Years of Education",
    mean = mean(EDUC, na.rm = TRUE),
    sd = sd(EDUC, na.rm = TRUE),
    observations= sum(!is.na(EDUC))
  )

###############################################################
#   INFORMAL [=1 if does not contribute to health system]     #
###############################################################

 C_INFORMAL <- COL_data %>%
  summarise(
    variable = "COLOMBIAN : INFORMAL [=1 DONT contribute to health system]",
    mean = mean(INFORMAL, na.rm = TRUE),
    sd = sd(INFORMAL, na.rm = TRUE),
    observations= sum(!is.na(INFORMAL))
  )

#############################################################
#                           VENEZUELAN                       #
#############################################################

#######################################
#               FORMAL                #
#######################################
F_VEN_data <- data %>%
  filter(VENEZUELAN == 1 & AGE <= 64 & FORMAL == 1)

############
# HRS_WRKD #
############

# Filter data where VENumn 'VENEZUELAN' is 1 AND 'FORMAL' is 1 and then summarize VENumn 'HRS_WRKD'

F_V_HRS_WRKD <- F_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : FORMAL HRS WORKED",
    mean = mean(HRS_WRKD, na.rm = TRUE),
    sd = sd(HRS_WRKD, na.rm = TRUE),
    observations = sum(HRS_WRKD!=0, na.rm = TRUE)
  )


####################
# THO_MONTHLY_WAGE #
####################

F_V_WAGE <- F_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : FORMAL MONTHLY WAGE (THOUSAND)",
    mean = mean(THO_MONTHLY_WAGE, na.rm = TRUE),
    sd = sd(THO_MONTHLY_WAGE, na.rm = TRUE),
    observations= sum(THO_MONTHLY_WAGE!=0, na.rm = TRUE)
  )


####################
#     EMPLOYED     #
####################

F_VEN_data <- data %>%
  filter(VENEZUELAN == 1 & AGE <= 64 & FORMAL == 1 & MONTHLY_WAGE>0)


F_V_EMPLYD <- F_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : FORMAL EMPLOYED [FORMAL SECTOR]",
    mean = mean(EMPLOYED, na.rm = TRUE),
    sd = sd(EMPLOYED, na.rm = TRUE),
    observations= sum(EMPLOYED!=0, na.rm = TRUE)
  )


#######################################
#               INFORMAL              #
#######################################

I_VEN_data <- data %>%
  filter(VENEZUELAN == 1 & AGE <= 64 & INFORMAL == 1)

############
# HRS_WRKD #
############

# Filter data where VENumn 'VENEZUELAN' is 1 AND 'INFORMAL' is 1 and then summarize VENumn 'HRS_WRKD'

I_V_HRS_WRKD <- I_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : INFORMAL HRS WRKD",
    mean = mean(HRS_WRKD, na.rm = TRUE),
    sd = sd(HRS_WRKD, na.rm = TRUE),
    observations= sum(HRS_WRKD!=0, na.rm = TRUE)
  )

####################
# THO_MONTHLY_WAGE #
####################

I_V_WAGE <- I_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : INFORMAL MONTHLY WAGE (THOUSAND)",
    mean = mean(THO_MONTHLY_WAGE, na.rm = TRUE),
    sd = sd(THO_MONTHLY_WAGE, na.rm = TRUE),
    observations= sum(THO_MONTHLY_WAGE!=0, na.rm = TRUE)
  )

####################
#     EMPLOYED     #
####################

I_VEN_data <- data %>%
  filter(VENEZUELAN == 1 & AGE <= 64 & INFORMAL == 1 & MONTHLY_WAGE>0)

I_V_EMPLYD <- I_VEN_data %>%
  summarise(
    variable = "VENEZUELAN : INFORMAL EMPLOYED [INFORMAL SECTOR]",
    mean = mean(EMPLOYED, na.rm = TRUE),
    sd = sd(EMPLOYED, na.rm = TRUE),
    observations= sum(EMPLOYED!=0, na.rm = TRUE)
  )


#########################################################
#                         LABOR FORCE                   #
#########################################################

VEN_data <- data %>%
  filter(VENEZUELAN == 1 & AGE <= 64)

V_LBRFRC <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : Labor Force",
    mean = mean(VENEZUELAN, na.rm = TRUE),
    sd = sd(VENEZUELAN, na.rm = TRUE),
    observations= sum(!is.na(VENEZUELAN))
  )

#########################################################
#                         URBAN                         #
#########################################################

v_URBAN <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : URBAN",
    mean = mean(URBAN, na.rm = TRUE),
    sd = sd(URBAN, na.rm = TRUE),
    observations= sum(!is.na(URBAN))
  )

#########################################################
#                     GENDER [ 1= MALE]                 #
#########################################################



V_GENDER <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : GENDER [1 = MALE]",
    mean = mean(GENDER, na.rm = TRUE),
    sd = sd(GENDER, na.rm = TRUE),
    observations= sum(VEN_data$GENDER==1,VEN_data$GENDER==0, na.rm = TRUE)
  )

#########################################################
#                          AGE                          #
#########################################################


V_AGE <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : AGE",
    mean = mean(AGE, na.rm = TRUE),
    sd = sd(AGE, na.rm = TRUE),
    observations= sum(!is.na(AGE))
  )


#########################################################
#                         COHABITATE                    #
#########################################################


V_COH <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : Cohabitate",
    mean = mean(COHABITATE, na.rm = TRUE),
    sd = sd(COHABITATE, na.rm = TRUE),
    observations= sum(!is.na(COHABITATE))
  )

#########################################################
#                   HEAD OF HOUSEHOLD                   #
#########################################################


V_JEFE <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : HEAD OF HOUSEHOLD",
    mean = mean(JEFE, na.rm = TRUE),
    sd = sd(JEFE, na.rm = TRUE),
    observations= sum(!is.na(JEFE))
  )

#########################################################
#                     YRS EDUCATION                     #
#########################################################


V_EDUC <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : Years of Education",
    mean = mean(EDUC, na.rm = TRUE),
    sd = sd(EDUC, na.rm = TRUE),
    observations= sum(!is.na(EDUC))
  )

###############################################################
#   INFORMAL [=1 if does not contribute to health system]     #
###############################################################


V_INFORMAL <- VEN_data %>%
  summarise(
    variable = "VENEZUELAN : INFORMAL [=1 DONT contribute to health system]",
    mean = mean(INFORMAL, na.rm = TRUE),
    sd = sd(INFORMAL, na.rm = TRUE),
    observations= sum(!is.na(INFORMAL))
  )



LaborForce <-rbind(F_C_HRS_WRKD,F_C_WAGE,F_C_EMPLYD,I_C_HRS_WRKD,I_C_WAGE,I_C_EMPLYD, C_LBRFRC, C_URBAN, C_GENDER, C_AGE, C_COH, C_JEFE, C_EDUC, C_INFORMAL,
                   F_V_HRS_WRKD,F_V_WAGE,F_V_EMPLYD,I_V_HRS_WRKD,I_V_WAGE,I_V_EMPLYD, V_LBRFRC, v_URBAN, V_GENDER, V_AGE, V_COH, V_JEFE, V_EDUC, V_INFORMAL)

View(LaborForce)

#REMOVE USED DATA
rm(F_C_HRS_WRKD,F_C_WAGE,F_C_EMPLYD,I_C_HRS_WRKD,I_C_WAGE,I_C_EMPLYD, C_LBRFRC, C_URBAN, C_GENDER, C_AGE, C_COH, C_JEFE, C_EDUC, C_INFORMAL,
   F_V_HRS_WRKD,F_V_WAGE,F_V_EMPLYD,I_V_HRS_WRKD,I_V_WAGE,I_V_EMPLYD, V_LBRFRC, v_URBAN, V_GENDER, V_AGE, V_COH, V_JEFE, V_EDUC, V_INFORMAL, COL_data, F_COL_data,
   F_VEN_data, I_COL_data, I_VEN_data, VEN_data)

#SAVE AS DTA FILE
write_dta(LaborForce, "LaborForce_Descriptive Stats.dta")
