

df <- read_dta("GEIH_REDUCED_2018_2023.dta")

df_Colombian <- subset(df, COLOMBIAN==1)

df_Migrnat <- subset(df, MIGRANT==1)



df_Migrnat <- df_Migrnat %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))

df_Colombian <- df_Colombian %>%
  mutate(Time_FE = as.factor(paste(ANO, MES, sep = "_")))
df_Colombian$MES <- sprintf("%02d", df_Colombian$MES)
df_Migrnat$MES <- as.numeric(df_Migrnat$MES)
df_Migrnat$MES <- sprintf("%02d", df_Migrnat$MES)

params<-c("DID")


test_33<-feols(FULL_INFORMAL ~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   |DPTO + Time_FE,data=df_Colombian)

coef_df <- as.data.frame(confint(test_33))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_33)[term],
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
  labs(title = "migrnat Formal employment",
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






############
# colombian#
############

feols_COL_FORMAL = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_Colombian)
summary(feols_COL_FORMAL)

REGRESSION<-feols_COL_FORMAL
#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(2)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")



#######################
# DATASET:  EDUCATION #
#######################

#Primary 6
df_prim <- subset(df_Colombian, EDUC<11)
#Highschool 11<14
df_high <- subset(df_Colombian, EDUC>=11)
df_high <- subset(df_Colombian, EDUC<14)
#Highereducation 14,15
df_uni <- subset(df_Colombian, EDUC<=14)
df_uni <- subset(df_Colombian, EDUC<=15)
#elite 17
df_mas <- subset(df_Colombian, EDUC==17)

feols_col__prim = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_prim)
summary(feols_col__prim) #0.03503504
REGRESSION<-feols_col__prim

feols_col__prim = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_prim)
summary(feols_col__prim)
REGRESSION<-feols_col__prim #0.04304304

feols_col__high = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_high)
summary(feols_col__high)

feols_col__high = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_high)
summary(feols_col__high)


feols_col__un = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_uni)
summary(feols_col__un)

feols_col__un = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                          DPTO + Time_FE, data = df_uni)
summary(feols_col__un)

df_female <- subset(df_Colombian, GENDER==0)
feols_col__wom = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                        DPTO + Time_FE, data = df_female)
summary(feols_col__wom)

feols_col__wom = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                        DPTO + Time_FE, data = df_female)
summary(feols_col__wom)
############
# Migrant #
############

feols_MIGRANT_FORMAL = feols(FULL_FORMAL~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_Migrnat)
summary(feols_MIGRANT_FORMAL)

REGRESSION<-feols_MIGRANT_FORMAL
#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(3)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(3)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")

#######################
# DATASET:  EDUCATION #
#######################

#Primary 6
df_prim <- subset(df_Migrnat, EDUC<11)
#Highschool 11<14
df_high <- subset(df_Migrnat, EDUC>=11)
df_high <- subset(df_Migrnat, EDUC<14)
#Highereducation 14,15
df_uni <- subset(df_Migrnat, EDUC<=14)
df_uni <- subset(df_Migrnat, EDUC<=15)
#elite 17
df_mas <- subset(df_Migrnat, EDUC==17)
df_female <- subset(df_Migrnat, GENDER==0)

feols_mig_prim = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                         DPTO + Time_FE, data = df_prim)
summary(feols_mig_prim)

feols_mig_primIN = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                         DPTO + Time_FE, data = df_prim)
summary(feols_mig_primIN)

feols_mig_highIN = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                         DPTO + Time_FE, data = df_high)
summary(feols_mig_highIN)

feols_mig_un = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                         DPTO + Time_FE, data = df_uni)
summary(feols_mig_un)
feols_mig_unIN = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                       DPTO + Time_FE, data = df_uni)
summary(feols_mig_unIN)
REGRESSION<-feols_mig_unIN


feols_mig_mas = feols(FULL_FORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                       DPTO + Time_FE, data = df_mas)
summary(feols_mig_mas)


feols_mig_INmas = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE | 
                        DPTO + Time_FE, data = df_mas)
summary(feols_mig_INmas)
############

############
# infomral colombian#
############



feols_COL_INFORMAL = feols(FULL_INFORMAL ~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_Colombian)
summary(feols_COL_INFORMAL)

REGRESSION<-feols_COL_INFORMAL



#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(4)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(4)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")



############
# HRS #
############

feols_MIGRANT_INFORMAL = feols(FULL_INFORMAL~ DID + AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO+Time_FE, data = df_Migrnat)
summary(feols_MIGRANT_INFORMAL)

REGRESSION<-feols_MIGRANT_INFORMAL
#################################
# WILD CLUSTERED BOOTSTRAP      #
#       P-VALUES                #
#################################

# set seed via dqset.seed for engine = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(5)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(5)

source("C:/Users/isabe/OneDrive/Documents/Bella_Cloud_Documents/UCD_cloud/Summer 2024 - Thesis/Colombia/GEIH R CODE/WildBootstrap_code.R")




