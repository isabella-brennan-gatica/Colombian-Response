#install.packages("purrr")
#install.packages("openxlsx")
rm(list=ls()) # clear workspace
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/")

library(haven) #read dataframes
library(dplyr) #modify columns
library(purrr) #merge dataframes by columns
#library(openxlsx) #save dataframe as excel 


  
data23 <- read_dta("GEIH - 2023 - Gran Encuesta Integrada de Hogares/GEIH_2023.dta")
data22 <- read_dta("GEIH - 2022 - Gran Encuesta Integrada de Hogares/GEIH_2022.dta")
data21 <- read_dta("GEIH - 2021 - Gran Encuesta Integrada de Hogares/GEIH_2021.dta")
data20 <- read_dta("GEIH - 2020 - Gran Encuesta Integrada de Hogares/GEIH_2020.dta")
data19 <- read_dta("GEIH - 2019 - Gran Encuesta Integrada de Hogares/GEIH_2019.dta")
data18 <- read_dta("GEIH - 2018 - Gran Encuesta Integrada de Hogares/GEIH_2018.dta")
#data17 <- read_dta("GEIH - 2017 - Gran Encuesta Integrada de Hogares/GEIH_2017.dta")
#data16 <- read_dta("GEIH - 2016 - Gran Encuesta Integrada de Hogares/GEIH_2016.dta")

# Get column names
#column_18 <- colnames(data18)

# Print column names
#print(column_18)

#data22 <- data22 %>%
#  rename(
#    ANO = PER,
#  )
#data23 <- data23 %>% rename(ANO = PER)

data23 <- data23 %>% select(-PERIODO)
data22 <- data22 %>% select(-PERIODO)
#data20 <- data20 %>% select(-PERIODO)

#data16 <- data16 %>% mutate(P6080 = NA)
#data17 <- data17 %>% mutate(P6080 = NA)
data18 <- data18 %>% mutate(P6080 = NA)
data19 <- data19 %>% mutate(P6080 = NA)

data22 <- data22 %>% mutate(P6210 = P3042)
data22$EDUC <-data22$P6210

#2022,2023
# 1	Ninguno
# 2	Preescolar 
# 3	Básica primaria (1o - 5o)
# 4	Básica secundaria (6o - 9o)
# 5	Media académica (Bachillerato clásico)
# 6	Media técnica (Bachillerato técnico)
# 7	Normalista
# 8	Técnica profesional
# 9	Tecnológica 
#10	Universitaria
#11	Especialización 
#12	Maestría 
#13	Doctorado 
#99 IDK

data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 1, 0, EDUC)) 
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 2, 0, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 3, 5, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 4, 9, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 5|P6210 == 6, 11, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 7|P6210 == 8|P6210 == 9, 14, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 10|P6210 == 11, 15, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 12|P6210 == 13, 17, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 99, 0, EDUC))

#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 6, 5, P6210)) 
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 7, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 8, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 9, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 10, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 11, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 12, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 13, 6, P6210))
#data22 <- data22 %>% mutate(EDUC = ifelse(P6210 == 99, 0, P6210))

data23 <- data23 %>% mutate(P6210 = P3042)
data23$EDUC <-data23$P6210

data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 1, 0, EDUC)) 
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 2, 0, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 3, 5, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 4, 9, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 5|P6210 == 6, 11, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 7|P6210 == 8|P6210 == 9, 14, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 10|P6210 == 11, 15, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 12|P6210 == 13, 17, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6210 == 99, 0, EDUC))

#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 6, 5, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 7, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 8, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 9, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 10, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 11, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 12, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 13, 6, P6210))
#data23 <- data23 %>% mutate(P6210 = ifelse(P6210 == 99, 9, P6210))

#2018-2021
#1	Ninguno
#2	Preescolar 
#3	Básica primaria (1o - 5o)
#4	Básica secundaria (6o - 9o)
#5	MEDIA
#6	HIGHER OR UNIVERSITY

####
#data22 <- data22 %>% mutate(P6210S1 = P3042S1)
#data23 <- data23 %>% mutate(P6210S1 = P3042S1)

#data22 <- data22 %>%
#  mutate(P6210  = ifelse(P6210 == 2, 1, P6210))
#data22 <- data22 %>%
#  mutate(P6210S1  = ifelse(P6210 == 1, 0, P6210S1))

#data22 <- data22 %>%
#  mutate(P6210S1  = ifelse(P6210 == 4, P6210S1+5, P6210S1))
#data22 <- data22 %>%
#  mutate(P6210S1  = ifelse(P6210 == 5, P6210S1+9, P6210S1))
#data22 <- data22 %>%
 # mutate(P6210S1  = ifelse(P6210 == 6, P6210S1+11, P6210S1))

#data23 <- data23 %>%
#  mutate(P6210  = ifelse(P6210 == 2, 1, P6210))
#data23 <- data23 %>%
#  mutate(P6210S1  = ifelse(P6210 == 1, 0, P6210S1))

#data23 <- data23 %>%
#  mutate(P6210S1  = ifelse(P6210 == 4, P6210S1+5, P6210S1))
#data23 <- data23 %>%
#  mutate(P6210S1  = ifelse(P6210 == 5, P6210S1+9, P6210S1))
#data23 <- data23 %>%
#  mutate(P6210S1  = ifelse(P6210 == 6, P6210S1+11, P6210S1))

#¿Cuál es el título o diploma de mayor nivel educativo que Usted ha recibido?
#2022-2023
#1	Ninguno
#2	Media académica (Bachillerato clásico)
#3	Media técnica (Bachillerato técnico)
#4	Normalista
#5	Técnica profesional
#6	Tecnológica 
#7	Universitaria
#8	Especialización 
#9	Maestría 
#10	Doctorado 
#99	No sabe, no informa

#2018
# 1. Ninguno
# 2. Bachiller
# 3. Técnico o tecnológico
# 4. Universitario
# 5. Postgrado
# 6. No sabe, no informa

data22 <- data22 %>% mutate(P6220 = P3043)
data22 <- data22 %>% mutate(EDUC = ifelse(P6220 == 2 |P6220 == 3, 11, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6220 == 4 |P6220 == 5|P6220 == 6, 14, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6220 == 7 |P6220 == 8, 15, EDUC))
data22 <- data22 %>% mutate(EDUC = ifelse(P6220 == 9 |P6220 == 10, 17, EDUC))


#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 4, 3, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 5, 3, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 6, 3, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 7, 4, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 8, 5, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 9, 5, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 10, 5, P6220))
#data22 <- data22 %>% mutate(P6220 = ifelse(P6220 == 99, 9, P6220))

data23 <- data23 %>% mutate(P6220 = P3043)

data23 <- data23 %>% mutate(EDUC = ifelse(P6220 == 2 |P6220 == 3, 11, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6220 == 4 |P6220 == 5|P6220 == 6, 14, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6220 == 7 |P6220 == 8, 15, EDUC))
data23 <- data23 %>% mutate(EDUC = ifelse(P6220 == 9 |P6220 == 10, 17, EDUC))


#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 3, 2, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 4, 3, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 5, 3, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 6, 3, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 7, 4, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 8, 5, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 9, 5, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 10, 5, P6220))
#data23 <- data23 %>% mutate(P6220 = ifelse(P6220 == 99, 9, P6220))


#data16 <- data16 %>% mutate(P3043 = NA)
#data17 <- data17 %>% mutate(P3043 = NA)
data18 <- data18 %>% mutate(P3043 = NA)
data19 <- data19 %>% mutate(P3043 = NA)
data20 <- data20 %>% mutate(P3043 = NA)
data21 <- data21 %>% mutate(P3043 = NA)

data18 <- data18 %>% mutate(P3373S3A1 = NA)
data19 <- data19 %>% mutate(P3373S3A1 = NA)
data20 <- data20 %>% mutate(P3373S3A1 = NA)
data21 <- data21 %>% mutate(P3373S3A1 = NA)

data22 <- data22 %>% mutate(P755 = NA)
data23 <- data23 %>% mutate(P755 = NA)
data22 <- data22 %>% mutate(P755S3 = NA)
data23 <- data23 %>% mutate(P755S3 = NA)

#P3373 Dónde nació ...: 
# 1	Aquí en este municipio
# 2	En otro Municipio
# 3	En otro país:
 
# Rename columns
data22 <- data22 %>% rename(
  P756 = P3373
)
data23 <- data23 %>% rename(
  P756 = P3373
)

# P3373S3 Dónde nació ...: En otro país:
# Rename columns
data22 <- data22 %>% rename(
  P756S3 = P3373S3
)
data23 <- data23 %>% rename(
  P756S3 = P3373S3
)

data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 8, 11, P756S3))#11. OTRO PAIS
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 4, 11, P756S3))
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 10, 11, P756S3))

data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 8, 11, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 4, 11, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 10, 11, P756S3))

data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 32, 8, P756S3)) #8. Argentina
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 188, 7, P756S3))#7. Costa Rica
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 218, 4, P756S3))#4. Ecuador
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 250, 9, P756S3))#9. Francia
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 380, 10, P756S3))#10. Italia
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 591, 5, P756S3))#5. Panamá
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 604, 6, P756S3)) #6. Perú
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 724, 2, P756S3)) #2. España
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 840, 1, P756S3)) #1. Estados Unidos
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 == 862, 3, P756S3))# 3. Venezuela


data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 32, 8, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 188, 7, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 218, 4, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 250, 9, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 380, 10, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 591, 5, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 604, 6, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 724, 2, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 840, 1, P756S3))
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 == 862, 3, P756S3))

score_counts <- table(data22$P756S3)

#IF NUMBER NOT ONE OF THESE (1, 2,3,4,5,6,7,8,9,10) THEN MARK 11 AS OTHER COUNTRY
data22 <- data22 %>% mutate(P756S3 = ifelse(P756S3 %in% c(1, 2,3,4,5,6,7,8,9,10), P756S3, 11)) 
data23 <- data23 %>% mutate(P756S3 = ifelse(P756S3 %in% c(1, 2,3,4,5,6,7,8,9,10), P756S3, 11)) 


#IF BORN IN COLOMBIA THEN FILL COLUMN P756S3 NA
data22 <- data22 %>% mutate(P756S3 = ifelse(P756 %in% c(1, 2), NA, P756S3))
data23 <- data23 %>% mutate(P756S3 = ifelse(P756 %in% c(1, 2), NA, P756S3))

data23 <- data23 %>% select(-P3374S1)
data22 <- data22 %>% select(-P3374S1)

data23 <- data23 %>% select(-P3375S1)
data22 <- data22 %>% select(-P3375S1)


data23 <- data23 %>% select(-P6424S5)
data22 <- data22 %>% select(-P6424S5)


data23 <- data23 %>% select(-P3046)
data22 <- data22 %>% select(-P3046)

data22 <- data22 %>% mutate(P6772 = NA)
data23 <- data23 %>% mutate(P6772 = NA)

data22 <- data22 %>% mutate(P6772 = ifelse(P3045S1 %in% c(1), 1, P6772))
data22 <- data22 %>% mutate(P6772 = ifelse(P3045S2 %in% c(1), 1, P6772))
data22 <- data22 %>% mutate(P6772 = ifelse(P3045S3 %in% c(1), 1, P6772))

data23 <- data23 %>% mutate(P6772 = ifelse(P3045S1 %in% c(1), 1, P6772)) 
data23 <- data23 %>% mutate(P6772 = ifelse(P3045S2 %in% c(1), 1, P6772))
data23 <- data23 %>% mutate(P6772 = ifelse(P3045S3 %in% c(1), 1, P6772))

#data16 <- data16 %>% rename(DIRECTORIO = DIRECTORIO)
#data17 <- data17 %>% rename(DIRECTORIO = DIRECTORIO)


######################################
#data16 <- data16 %>%
#  mutate(P6210  = ifelse(P6210 == 2, 1, P6210))
#data16 <- data16%>%
#  mutate(P6210S1  = ifelse(P6210 == 1, 0, P6210S1))
#data16 <- data16%>%
#  mutate(P6210S1  = ifelse(P6210 == 6, P6210S1+11, P6210S1))

#data17 <- data17 %>%
#  mutate(P6210  = ifelse(P6210 == 2, 1, P6210))
#data17 <- data17 %>%
#  mutate(P6210S1  = ifelse(P6210 == 1, 0, P6210S1))
#data17 <- data17 %>%
#  mutate(P6210S1  = ifelse(P6210 == 6, P6210S1+11, P6210S1))
###EDUCATION####
data18$EDUC <-data18$P6210
data19$EDUC <-data19$P6210
data20$EDUC <-data20$P6210
data21$EDUC <-data21$P6210

#2018-2021
#1	Ninguno
#2	Preescolar 
#3	Básica primaria (1o - 5o)
#4	Básica secundaria (6o - 9o)
#5	MEDIA
#6	HIGHER OR UNIVERSITY

data18 <- data18 %>%
  mutate(EDUC  = ifelse(P6210 == 1|P6210 == 2, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 9, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 3, 5, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 4, 9, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 5, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 3,14, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 4,15, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 5,17, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 2,11, EDUC))

  #2018
  # 1. Ninguno
  # 2. Bachiller
  # 3. Técnico o tecnológico
  # 4. Universitario
  # 5. Postgrado
  # 6. No sabe, no informa

  data19 <- data19 %>%
  mutate(EDUC  = ifelse(P6210 == 1|P6210 == 2, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 9, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 3, 5, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 4, 9, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 5, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 3,14, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 4,15, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 5,17, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 2,11, EDUC))
  

data20 <- data20 %>%
  mutate(EDUC  = ifelse(P6210 == 1|P6210 == 2, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 9, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 3, 5, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 4, 9, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 5, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 3,14, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 4,15, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 5,17, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 2,11, EDUC))


data21 <- data21 %>%
  mutate(EDUC  = ifelse(P6210 == 1|P6210 == 2, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 9, 0, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 3, 5, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 4, 9, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 5, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6, 11, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 3,14, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 4,15, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 5,17, EDUC))%>%
  mutate(EDUC  = ifelse(P6210 == 6 & P6220 == 2,11, EDUC))

  
#############################################



#common_cols1617 <- intersect(names(data16), names(data17))
#merged_16_17 <- merge(data16, data17, by = common_cols1617, all = TRUE)

#common_cols18 <- intersect(names(merged_16_17), names(data18))
#merged_16_18 <- merge(merged_16_17, data18, by = common_cols18, all = TRUE)

data19 <- data19 %>%
  mutate_if(is.labelled, as.numeric)

common_cols19 <- intersect(names(data18), names(data19))
merged_18_19 <- merge(data18, data19, by = common_cols19, all = TRUE)

data20 <- data20 %>%
  mutate_if(is.labelled, as.numeric)

common_cols20 <- intersect(names(merged_18_19), names(data20))
merged_18_20 <- merge(merged_18_19, data20, by = common_cols20, all = TRUE)

data21 <- data21 %>%
  mutate_if(is.labelled, as.numeric)

common_cols21 <- intersect(names(merged_18_20), names(data21))
merged_18_21 <- merge(merged_18_20, data21, by = common_cols21, all = TRUE)

common_cols22 <- intersect(names(merged_18_21), names(data22))
merged_18_22 <- merge(merged_18_21, data22, by = common_cols22, all = TRUE)

common_cols23 <- intersect(names(merged_18_22), names(data23))
merged_18_23 <- merge(merged_18_22, data23, by = common_cols23, all = TRUE)

remove_labels <- function(merged_18_23) {
  # Iterate over each column in the data frame
  merged_18_23 [] <- lapply(merged_18_23 , function(col) {
    # Remove all attributes from the column
    attributes(col) <- NULL
    return(col)
  })
  return(merged_18_23)
}
merged_18_23 <- remove_labels(merged_18_23)

columns_to_keep <-c("ID","DIRECTORIO","SECUENCIA_P","ORDEN","MES","ANO","DPTO","CLASE", "EDUC","P6020","P6030S3","P6040","P6050",
                    "P6080","P6070", "P6090", "P6100","P6110","P6210","P6210S1","P6220","P756",     
                    "P756S3","P755","P755S3","P3373S3A1","P6440","P6450","P6460","P6460S1","P6424S1","P6424S2","P6424S3",
                    "P6426","P6772","P6500","P6510","P6510S1","P6510S2","P6765","P6750","P6760",
                    "P550","P6780","P1879","P1805","P6790","P6800","P6850","P6880","P6920", "P6930","P6940","P7040",
                    "P7045","P7050","P7070","P7130","P514", "INGLABO", "P3051", "P3052", "P3365","P7422", "P7422S1",  "P1519", "P1883", "DSI")    


merged_18_23 <- merged_18_23[, columns_to_keep]


setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH DATA/")

#SAVE AS DTA FILE
write_dta(merged_18_23, "GEIH_2018_2023.dta")

