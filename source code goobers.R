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

property_dataset <- read.csv("D:\\Github\\data analysis module\\r-programming-project\\PFDAdataset.csv", na.strings = c(" ", "NA")) # ( !! edit based off your dataset location !! )
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

# property_dataset$Occupation[property_dataset$Occupation == "_______"] <- NA # converts invalid values to NA

# Function to fill blank occupations with the most frequent occupation in the 8-row block
fill_occupation <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Occupation = as.character(Occupation)) # treat as chara
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    block <- property_dataset[i:(i+7), ]
    # finds the most common occupation in the block that is not blank or '_______'
    valid_occupations <- block$Occupation[block$Occupation != '' & block$Occupation != '_______']
    if (length(valid_occupations) > 0) {
      mode_occupation <- names(sort(table(valid_occupations), decreasing = TRUE))[1]
      property_dataset$Occupation[i:(i+7)][property_dataset$Occupation[i:(i+7)] == '' | property_dataset$Occupation[i:(i+7)] == '_______'] <- mode_occupation
    }
  }
  return(property_dataset)
}

property_dataset <- fill_occupation(property_dataset)
property_dataset

# removes underscore from end of string
property_dataset$Annual_Income <- gsub("_", "", property_dataset$Annual_Income)
property_dataset

# changes inconsistent values to consistent values
incons_val <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Annual_Income = as.numeric(Annual_Income)) # treat as numeric
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    annl_inc <- property_dataset[i:(i+7), ]
    # Find the most common annual income within 8 rows // similar to last func
    cmn_income <- names(sort(table(annl_inc$Annual_Income), decreasing = TRUE))[1]
    # Replace all 'Annual_Income' values in the block with the mode // ALSO similar to last func
    property_dataset$Annual_Income[i:(i+7)] <- cmn_income
  }
  return(property_dataset)
}

# Apply the function to the dataframe
property_dataset <- incons_val(property_dataset)
print(property_dataset[50:60, ]) # where first inconsistency happened // to check dataset







# Monthly Inhand Salary sorting
monthly_sal <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Monthly_Inhand_Salary = as.numeric(Monthly_Inhand_Salary)) # treat as numerics
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    monthly_sal_table <- property_dataset[i:(i+7), ]
    valid_monthly_sal <- monthly_sal_table$Monthly_Inhand_Salary[monthly_sal_table$Monthly_Inhand_Salary != '' & monthly_sal_table$Monthly_Inhand_Salary >= 0]
    if (length(valid_monthly_sal) > 0) {
      mode_monthly_sal <- names(sort(table(valid_monthly_sal), decreasing = TRUE))[1]
      # Replace values within the table only
      property_dataset$Monthly_Inhand_Salary[i:(i+7)] <- ifelse(property_dataset$Monthly_Inhand_Salary[i:(i+7)] == '' | is.na(property_dataset$Monthly_Inhand_Salary[i:(i+7)]), mode_monthly_sal, property_dataset$Monthly_Inhand_Salary[i:(i+7)])
    }
  }
  return(property_dataset)
}

property_dataset <- monthly_sal(property_dataset)
property_dataset[10:30, ] # this is where the first irregularity appeared









# Checks for unique bank account amount values
bank_check <- unique(property_dataset$Num_Bank_Accounts) # used to find logical range of values
bank_check 

# Bank Account Value cleaning
bank_acc_val <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Num_Bank_Accounts = as.numeric(Num_Bank_Accounts)) # treat as numerics
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    bank_acc_table <- property_dataset[i:(i+7), ]
    valid_bank_acc <- bank_acc_table$Num_Bank_Accounts[bank_acc_table$Num_Bank_Accounts <= 10 & bank_acc_table$Num_Bank_Accounts >= 0]
    if (length(valid_bank_acc) > 0) {
      mode_bank_acc <- names(sort(table(valid_bank_acc), decreasing = TRUE))[1]
      # Replace values within the table only
      property_dataset$Num_Bank_Accounts[i:(i+7)] <- ifelse(property_dataset$Num_Bank_Accounts[i:(i+7)] > 10 | property_dataset$Num_Bank_Accounts[i:(i+7)] < 0, mode_bank_acc, property_dataset$Num_Bank_Accounts[i:(i+7)])
    }
  }
  return(property_dataset)
}

property_dataset <- bank_acc_val(property_dataset)
property_dataset[260:280, ] # this is where the first irregularity appeared

# checks for unqiue values in the number of credit cards column
cc_check <- unique(property_dataset$Num_Credit_Card) # used to find logical range of values
cc_check_sorted <- sort(cc_check, decreasing = FALSE)
cc_check_sorted

# Number of Credit Cards cleaning
cc_val <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Num_Credit_Card = as.numeric(Num_Credit_Card)) # treat as numerics
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    cc_table <- property_dataset[i:(i+7), ]
    valid_cc <- cc_table$Num_Credit_Card[cc_table$Num_Credit_Card <= 12 & cc_table$Num_Credit_Card >= 0]
    if (length(valid_cc) > 0) {
      mode_cc <- names(sort(table(valid_cc), decreasing = TRUE))[1]
      # Replace values within the table only
      property_dataset$Num_Credit_Card[i:(i+7)] <- ifelse(property_dataset$Num_Credit_Card[i:(i+7)] > 12 | property_dataset$Num_Credit_Card[i:(i+7)] < 0, mode_cc, property_dataset$Num_Credit_Card[i:(i+7)])
    }
  }
  return(property_dataset)
}

property_dataset <- cc_val(property_dataset)
property_dataset[10:20, ] # this is where the first irregularity appeared

# finds the unique values of the column to determine logical values
intr_check <- unique(property_dataset$Interest_Rate)
intr_check_sort <- sort(intr_check, decreasing = FALSE)
intr_check_sort

# Interest Rate Cleaning
intr_val <- function(property_dataset) {
  property_dataset <- property_dataset %>% mutate(Interest_Rate = as.numeric(Interest_Rate))
  for (i in seq(1, nrow(property_dataset), by = 8)) {
    intr_table <- property_dataset[i:(i+7), ]
    valid_intr <- intr_table$Interest_Rate[intr_table$Interest_Rate <= 34 & intr_table$Interest_Rate > 0]
    if (length(valid_intr) > 0) {
      mode_intr <- names(sort(table(valid_intr), decreasing = TRUE))[1]
      # Replace values within the table only
      property_dataset$Interest_Rate[i:(i+7)] <- ifelse(property_dataset$Interest_Rate[i:(i+7)] > 34 | property_dataset$Interest_Rate[i:(i+7)] < 0, mode_intr, property_dataset$Interest_Rate[i:(i+7)])
    }
  }
  return(property_dataset)
}

property_dataset <- intr_val(property_dataset)
property_dataset[40:50, ]

# check for number of loan values 
property_dataset$Num_of_Loan <- gsub("_", "", property_dataset$Num_of_Loan)
property_dataset <- property_dataset %>% mutate(Num_of_Loan = as.numeric(Num_of_Loan))
property_dataset

num_loan_vals <- unique(property_dataset$Num_of_Loan)
num_loan_vals_sort <- sort(num_loan_vals, decreasing = FALSE)
num_loan_vals_sort


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





