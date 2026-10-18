setwd("D:/Project")

install.packages(c("tidyverse", "sqldf", "ggplot2"))
install.packages("shiny")

library(tidyverse)
library(sqldf)

insurance <- read.csv("insurance.csv")

glimpse(insurance)
summary(insurance)
colSums(is.na(insurance))
insurance_clean <- na.omit(insurance)
colSums(is.na(insurance_clean))

insurance <- read.csv("insurance.csv")

insurance_clean <- insurance %>%
  mutate(
    age = ifelse(is.na(age), mean(age, na.rm = TRUE), age),
    bmi = ifelse(is.na(bmi), mean(bmi, na.rm = TRUE), bmi),
    children = ifelse(is.na(children), mean(children, na.rm = TRUE), children)
  )


colSums(is.na(insurance_clean))

head(insurance_clean)

library(dplyr)
insurance_clean %>%
  group_by(smoker)  %>%
summarise(ave_charge = mean(charges))

insurance_clean %>%
  group_by(region) %>%
  summarise(avg_charge = mean(charges))

region_avg <- insurance_clean %>%
  group_by(region) %>%
  summarise(avg_charge = mean(charges))

ggplot(region_avg, aes(x= region, y = avg_charge, fill = region)) + 
  geom_col() + 
  labs(
    title = "Average Insurance charges by Region" ,
    x = "Region" ,
    y = "Average Charges ($)"
  )

ggplot(insurance_clean, aes(x = age, y = charges)) +
  geom_point(alpha = 0.5) +
  labs(
title = "Age vs insurance",
x = "Age",
y = "Charges ($)"
  )

ggplot(insurance_clean, aes(x = bmi, y = charges)) + 
  geom_point(alpha = 0.5) +
labs(
  title = "BMI Vs insurance charges",
  x = "BMI",
  y = "charges ($)"
)

ggplot(insurance_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() + 
  labs(
    title = "Chargers for Smoker vs Non-Smoker",
    x = "Smoker Status",
    y = "charges ($)"
  )
  
numeric_vars <- insurance_clean[,c("age", "bmi", "children", "charges")]
cor(numeric_vars)