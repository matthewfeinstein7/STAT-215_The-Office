## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
table(data$Look_Timing)
mean(data$Look_Timing)
sd(data$Look_Timing)
summary(data$Look_Timing)

# EXAMINE QUANT_VAR2
table(data$Number_of_Looks)
mean(data$Number_of_Looks)
sd(data$Number_of_Looks)
summary(data$Number_of_Looks)

# EXAMINE QUAL_VAR1
table(data$Character_Look)

# EXAMINE QUAL_VAR2
table(data$Emotion_from_Look)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Character_Look, data$Emotion_from_Look)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Character_Look, data$Emotion_from_Look))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Number_of_Looks ~ Emotion_from_Look, data = raw_data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Number_of_Looks, data$Emotion_from_Look)


##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################

linear_relationship <- lm(Look_Timing ~ Number_of_Looks, data = raw_data)
summary(linear_relationship)


##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
linear_plot <- plot(data$Number_of_Looks, data$Look_Timing)
print(linear_plot)
abline(linear_relationship, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################

plot(data$Number_of_Looks, residuals(linear_relationship))
abline(h = 0, col = "red")