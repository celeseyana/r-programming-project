# PFDA Group Assignment
# To determine the correlation between a person's credit score and other factors, such as annual income, number of credit cards,
# number of bank accounts, as well as the percentile of interest rate.



#=============================================

# Libraries and Packages

# un-comment install lines on first load, run and re-comment ( !! IMPORTANT READ !! )

# install.packages("ggplot2")
library(ggplot2)
# install.packages("dplyr")
library(dplyr)
# install.packages("stringr")
library(stringr)
# install.packages("tidyverse")
library(tidyverse)
# readr comes included in tidyverse package
library(readr)
#=============================================



#=============================================
# Importing Dataset

property_dataset <- read.csv("C:\\Github\\data analysis module\\r-programming-project\\PFDAdataset.csv", na.strings = c(" ", "NA")) # ( !! edit based off your dataset location !! )
for (col in names(property_dataset)) {
  property_dataset[property_dataset == ""] <- NA
}
property_dataset

#=============================================



#=============================================

# Data Cleaning

# duplicate row checking
property_dataset <- property_dataset %>%
  distinct()
property_dataset

# finding unique values in age column
unique_ages <- unique(property_dataset$Age)
unique_ages

# changing age values into logical values
property_dataset$Age <- as.numeric(as.character(property_dataset$Age)) # data type check

name_check <- property_dataset %>%
  group_by(Name) %>%
  summarise(logical_age = median(Age, na.rm = TRUE))

property_dataset <- property_dataset %>%
  left_join(name_check, by = "Name") %>%
    mutate(Age = ifelse(is.na(Age) | Age <= 0 | Age > 120, logical_age, Age))
property_dataset

# finding unique values in occupation column
unique_occu <- unique(property_dataset$Occupation)
unique_occu

# matching occupations with names
property_dataset$Occupation[property_dataset$Occupation == "_______"] <- NA # converts invalid values to NA

occu_count <- property_dataset %>%
  group_by(Name, Occupation) %>%
  summarise(count = n()) %>%
  ungroup()

occu_check <- occu_count %>%
  group_by(Name) %>%
  slice(which.max(count)) %>%
  select(-count)
occu_check # outputs most common value in occupation column for each name in name column 

property_dataset <- property_dataset %>%
  group_by(Name) %>%
  mutate(Occupation = ifelse(is.na(Occupation), occu_check$Occupation[match(Name, occu_check$Name)], Occupation)) %>%
  ungroup()
property_dataset # replaces empty values in occupation with a person's most common occupation (occu_check) (???)

#=============================================



#=============================================

# Data Exploration





#=============================================



#=============================================

# Ong Zi Yang TP065229
# Objective : To investigate the behaviour between credit score and annual income

#Analysis 1 : Is there a correlation between a person's credit score as well as their annual income?

#Analysis 2 : Does the occupation of an individual affect their annual income?

#Analysis 3 : Is there a correlation between an individual's payment behaviour and their credit score?

#Analysis 4 : Does an individual's credit utilization ratio have an effect on their credit score?


#=============================================



#=============================================

# Joshua Tioh Chee Yong TP065839
# Objective : 

#Analysis 1 :

#Analysis 2 :

#Analysis 3 :

#Analysis 4 :


#=============================================



#=============================================

# Trilester Movees Tirilos TP066460
# Objective : To investigate the behaviour between credit score and number of credit cards

#Analysis 1 :

#Analysis 2 :

#Analysis 3 :

#Analysis 4 :

#=============================================



#=============================================

# Wong Wei Hann TP065820
# Objective : 

#Analysis 1 :

#Analysis 2 :

#Analysis 3 :

#Analysis 4 :


#=============================================





