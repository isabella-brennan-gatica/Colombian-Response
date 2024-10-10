#install.packages("purrr")
install.packages("openxlsx")
install.packages("Hmisc")
rm(list=ls()) # clear workspace
setwd("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/")

library(haven) #read dataframes
library(dplyr) #modify columns
library(purrr) #merge dataframes by columns
library(openxlsx) #save dataframe as excel 
library(Hmisc)

df <- read_dta("GEIH_2016_2023.dta")

label(df$ID) <- "Unique ID"
label(df$P6030S3) <- "Birth Year"
label(df$P6040) <- "Age"
label(df$P6020) <- "Sex"
label(df$P6050) <- "Jefe"
label(df$P6080) <- "Race/Ethnicity"
label(df$P6070) <- "Marital Status"
label(df$P6090) <- "Contribute to health system?"
label(df$P6100) <- "Which social security health schemes"
label(df$P6210) <- "Education Level"
label(df$P6210S1) <- "Grade"
label(df$P6220) <- "Title/Diploma"
label(df$P756) <- "Birth Place"
label(df$P756S3) <- "Birth Country"
label(df$P6440) <- "Work Contract"
label(df$P6450) <- "Verbal/ Written Contract"
label(df$P6460) <- "Contract Definite? Indefinite?"
label(df$P6460S1) <- "Contract Length"
label(df$P6424S1) <- "WORK - Vacation"
label(df$P6424S2) <- "WORK - Christmas"
label(df$P6424S3) <- "WORK -  Unemployment"
label(df$P6426) <- "Length of Employment"
label(df$P6772) <- "Business registered?"
label(df$P6500) <- "Pre-tax Wage"
label(df$P6510) <- "Overtime? Y or N"
label(df$P6510S1) <- "Amount Overtime Paid?"
label(df$P6510S2) <- "Included in Pre-tax Wage"
label(df$P6765) <- "Type of Work"
label(df$P6750) <- "Net Profit"
label(df$P6760) <- "Months of Profit"
label(df$P550) <- "12 month Business Net Profit"
label(df$P6780) <- "Work Schedule"
label(df$P1879) <- "Work Independence Reasons"
label(df$P1805) <- "Would U take a salaried Job?"
label(df$P6790) <- "Months Employed"
label(df$P6800) <- "Avg Hours a Week working"
label(df$P6850) <- "Hours last week working"
label(df$P6880) <- "Work Location"
label(df$P7040) <- "2nd Job?"
label(df$P7045) <- "Hours last week working 2nd job"
label(df$P7050) <- "Type of 2nd Work"
label(df$P7070) <- "Wage"
label(df$P7130) <- "Unsatisfied with Job"
label(df$P514) <- "Is Job stable?"
label(df$INGLABO) <- "Total Wage"





