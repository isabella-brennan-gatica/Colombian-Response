###############################
#      LOG_WAGE DATASET       #
###############################

test_35<-feols(LOG_WAGE~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE |DPTO + Time_FE,data=df2)

coef_df <- as.data.frame(confint(test_35))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_35)[term],
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
  labs(title = "Colombian Log (Monthly Wage)",
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

###################################
#      LOG_HRS_WRKD DATASET       #
###################################
df2 <- df2 %>% mutate(LOG_HRS_WRKD=log(HRS_WRKD))

test_33<-feols(LOG_HRS_WRKD ~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   |DPTO + Time_FE,data=df2)

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
  labs(title = "Colombian Log (Weekly Hours Worked)",
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

#formal
###############################
#      LOG_WAGE DATASET       #
###############################

test_11<-feols(LOG_WAGE~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023|DPTO + Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test_11))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_11)[term],
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
  labs(title = "Colombian Formal Sector: Log (Monthly Wages)",
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

###################################
#      LOG_HRS_WRKD DATASET       #
###################################

test_13<-feols(LOG_HRS_WRKD ~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE +
                 covid_ANO_2021 + covid_ANO_2022 + covid_ANO_2023
               |DPTO + Time_FE,data=df2_formal)

coef_df <- as.data.frame(confint(test_13))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_13)[term],
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
  labs(title = "Colombian Formal Sector: Log (Weekly Hours Worked)",
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


###############################
#      LOG_WAGE DATASET       #
###############################

test_25<-feols(LOG_WAGE~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   |DPTO + Time_FE,data=df2_informal)

coef_df <- as.data.frame(confint(test_25))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_25)[term],
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
  labs(title = "Colombian Informal Sector: Log (Monthly Wages)",
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

###################################
#      LOG_HRS_WRKD DATASET       #
###################################

test_23<-feols(LOG_HRS_WRKD ~  i(Time_FE, LOG_PTP, ref = "2021_03")+
                 AGE + I(AGE^2) + GENDER + COHABITATE + EDUC + JEFE   |DPTO + Time_FE,data=df2_informal)

coef_df <- as.data.frame(confint(test_23))
coef_df$term <- rownames(coef_df)
coef_df <- coef_df %>%
  filter(grepl("^Time_FE::", term)) %>%
  mutate(estimate = coef(test_23)[term],
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
  labs(title = "Colombian Informal Sector Log (Weekly Hours Worked)",
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


