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

property_dataset <- read.csv("C:\\Github\\r-programming-project\\PFDAdataset.csv", na.strings = c(" ", "NA")) # ( !! edit based off your dataset location !! )
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

# name change rm na

property_dataset <- property_dataset %>% mutate(Name = as.character(Name))

replace_with_mode_name <- function(x) {
  mode_value_name <- as.character(names(which.max(table(x))))
  x[is.na(x)] <- mode_value_name
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Name = replace_with_mode_name(Name))



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
      property_dataset$Monthly_Inhand_Salary[i:(i+7)] <- ifelse(property_dataset$Monthly_Inhand_Salary[i:(i+7)] == '' | is.na(property_dataset$Monthly_Inhand_Salary[i:(i+7)]) | property_dataset$Monthly_Inhand_Salary[i:(i+7)] != mode_monthly_sal, mode_monthly_sal, property_dataset$Monthly_Inhand_Salary[i:(i+7)])
    }
  }
  return(property_dataset)
}

property_dataset <- monthly_sal(property_dataset)
property_dataset[45:55, ] # this is where the first irregularity appeared


# Checks for unique bank account amount values
bank_check <- unique(property_dataset$Num_Bank_Accounts) # used to find logical range of values
bank_check 

# Bank Account Value cleaning
property_dataset <- property_dataset %>% mutate(Num_Bank_Accounts = as.numeric(Num_Bank_Accounts))

replace_with_mode_bankaccs <- function(x) {
  mode_value_bankaccs <- as.numeric(names(which.max(table(x))))
  x[x < 0 | x > 10 ] <- mode_value_bankaccs
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_Bank_Accounts = replace_with_mode_bankaccs(Num_Bank_Accounts))



# checks for unique values in the number of credit cards column
cc_check <- unique(property_dataset$Num_Credit_Card) # used to find logical range of values
cc_check_sorted <- sort(cc_check, decreasing = FALSE)
cc_check_sorted

# Number of Credit Cards cleaning
property_dataset <- property_dataset %>% mutate(Num_Credit_Card = as.numeric(Num_Credit_Card))

replace_with_mode_cc <- function(x) {
  mode_value_cc <- as.numeric(names(which.max(table(x))))
  x[x < 0 | x > 12 ] <- mode_value_cc
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_Credit_Card = replace_with_mode_cc(Num_Credit_Card))


# finds the unique values of the column to determine logical values
intr_check <- unique(property_dataset$Interest_Rate)
intr_check_sort <- sort(intr_check, decreasing = FALSE)
intr_check_sort

# Interest Rate Cleaning
property_dataset <- property_dataset %>% mutate(Interest_Rate = as.numeric(Interest_Rate))

replace_with_mode_intrate <- function(x) {
  mode_value_intrate <- as.numeric(names(which.max(table(x))))
  x[x < 0 | x > 34 ] <- mode_value_intrate
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Interest_Rate = replace_with_mode_intrate(Interest_Rate))

# check for num_of_loan values 
property_dataset$Num_of_Loan <- gsub("_", "", property_dataset$Num_of_Loan)
property_dataset <- property_dataset %>% mutate(Num_of_Loan = as.numeric(Num_of_Loan))
property_dataset

num_loan_vals <- unique(property_dataset$Num_of_Loan)
num_loan_vals_sort <- sort(num_loan_vals, decreasing = FALSE)
num_loan_vals_sort

# number of loan cleaning
property_dataset <- property_dataset %>% mutate(Num_of_Loan = as.numeric(Num_of_Loan))

replace_with_mode_loannum <- function(x) {
  mode_value_loannum <- as.numeric(names(which.max(table(x))))
  x[x < 0 | x > 9 ] <- mode_value_loannum
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_of_Loan = replace_with_mode_loannum(Num_of_Loan)) %>%
  ungroup() %>%
  mutate(Num_of_Loan = ifelse(Num_of_Loan == 0, "null", Num_of_Loan))

# type of loan cleaning
loantype_check <- unique(property_dataset$Type_of_Loan)
loantype_check

loantype_clean <- function(property_dataset) {
  property_dataset <- property_dataset %>%
    mutate(Type_of_Loan = ifelse(is.na(Type_of_Loan), "Not Specified", Type_of_Loan))
  
  return(property_dataset)
}

property_dataset <- loantype_clean(property_dataset)

# Delay from due date
duedate_vals <- unique(property_dataset$Delay_from_due_date)
duedate_vals_sort <- sort(duedate_vals, decreasing = FALSE)
duedate_vals_sort # values don't seem illogical at all...? probably doesn't require cleaning

# Number of delayed payments
property_dataset$Num_of_Delayed_Payment <- gsub("_", "", property_dataset$Num_of_Delayed_Payment)
property_dataset <- property_dataset %>% mutate(Num_of_Delayed_Payment = as.numeric(Num_of_Delayed_Payment))

delayed_vals <- unique(property_dataset$Num_of_Delayed_Payment)
delayed_vals_sort <- sort(delayed_vals, decreasing = FALSE)
delayed_vals_sort

replace_with_mode_dp <- function(x) {
  mode_value_dp <- as.numeric(names(which.max(table(x))))
  x[is.na(x) | x < 0 | x > 28] <- mode_value_dp
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_of_Delayed_Payment = replace_with_mode_dp(Num_of_Delayed_Payment))

# Changed Credit Limit
cred_limit_vals <- unique(property_dataset$Changed_Credit_Limit)
cred_limit_vals_sort <- sort(cred_limit_vals, decreasing = FALSE)
cred_limit_vals_sort

property_dataset$Changed_Credit_Limit <- gsub("_", "", property_dataset$Changed_Credit_Limit)
property_dataset <- property_dataset %>% mutate(Changed_Credit_Limit = as.numeric(Changed_Credit_Limit))

replace_with_mode_credlimit <- function(x) {
  mode_value_credlimit <- as.numeric(names(which.max(table(x))))
  x[is.na(x) | x == ""] <- mode_value_credlimit
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Changed_Credit_Limit = replace_with_mode_dp(Changed_Credit_Limit))


# Number of Credit Inquiries cleaning
unique_cred_inq <- unique(property_dataset$Num_Credit_Inquiries)
unique_cred_inq_sort <- sort(unique_cred_inq, decreasing = FALSE)
unique_cred_inq_sort

property_dataset <- property_dataset %>% mutate(Num_Credit_Inquiries = as.numeric(Num_Credit_Inquiries))

replace_with_mode_cred_inq <- function(x) {
  mode_value_cred_inq <- as.numeric(names(which.max(table(x))))
  x[is.na(x) | x == ""] <- mode_value_cred_inq
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_Credit_Inquiries = replace_with_mode_cred_inq(Num_Credit_Inquiries))


# Credit Mix cleaning
unique_cred_mix <- unique(property_dataset$Credit_Mix)
unique_cred_mix

replace_with_mode_credmix <- function(x) {
  mode_value_credmix <- as.character(names(which.max(table(x))))
  x[x == "_"] <- mode_value_credmix
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Credit_Mix = replace_with_mode_credmix(Credit_Mix))


# Outstanding Debt Cleaning
property_dataset$Outstanding_Debt <- gsub("_", "", property_dataset$Outstanding_Debt)

# Credit Utilization Ratio
property_dataset$Credit_Utilization_Ratio <- round(property_dataset$Credit_Utilization_Ratio, 2)


# Credit History Age cleaning (idk)







# Payment of Min Amount cleaning
unique_paymin <- unique(property_dataset$Payment_of_Min_Amount)
unique_paymin_sort <- sort(unique_paymin, decreasing = FALSE)
unique_paymin_sort

property_dataset <- property_dataset %>% mutate(Payment_of_Min_Amount = as.character(Payment_of_Min_Amount))

replace_with_mode_paymin <- function(x) {
  mode_value_paymin <- as.character(names(which.max(table(x))))
  x[x == "NM"] <- mode_value_paymin
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Payment_of_Min_Amount = replace_with_mode_paymin(Payment_of_Min_Amount))




# Total EMI per Month
property_dataset <- property_dataset %>% mutate(Total_EMI_per_month = as.numeric(Total_EMI_per_month))

replace_with_mode_emi <- function(x) {
  mode_value_emi <- as.numeric(names(which.max(table(x))))
  x[x > 1000] <- mode_value_emi
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Total_EMI_per_month = replace_with_mode_emi(Total_EMI_per_month))

property_dataset$Total_EMI_per_month <- round(property_dataset$Total_EMI_per_month, 2)


# Amount Invested monthly
property_dataset$Amount_invested_monthly <- gsub("_", "", property_dataset$Amount_invested_monthly)
property_dataset <- property_dataset %>% mutate(Amount_invested_monthly = as.numeric(Amount_invested_monthly))


replace_with_mode_invested <- function(x) {
  mode_value_invested <- as.numeric(names(which.max(table(x))))
  x[is.na(x)] <- mode_value_invested
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Amount_invested_monthly = replace_with_mode_invested(Amount_invested_monthly))

property_dataset$Amount_invested_monthly <- round(property_dataset$Amount_invested_monthly, 2)

# Payment Behaviour cleaning
unique_paybhv <- unique(property_dataset$Payment_Behaviour)
unique_paybhv

property_dataset <- property_dataset %>% mutate(Payment_Behaviour = as.character(Payment_Behaviour))
property_dataset$Payment_Behaviour[property_dataset$Payment_Behaviour == "!@9#%8"] <- NA

replace_with_mode_paybhv <- function(x) {
  mode_value_paybhv <- as.character(names(which.max(table(x))))
  x[x == "!@9#%8" | is.na(x)] <- mode_value_paybhv
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Payment_Behaviour = replace_with_mode_paybhv(Payment_Behaviour))


# Monthly Balance
property_dataset <- property_dataset %>% mutate(Monthly_Balance = as.numeric(Monthly_Balance))

replace_with_mode_monthbal <- function(x) {
  mode_value_monthbal <- as.numeric(names(which.max(table(x))))
  x[is.na(x)] <- mode_value_monthbal
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Balance = replace_with_mode_monthbal(Monthly_Balance))

property_dataset$Monthly_Balance <- round(property_dataset$Monthly_Balance, 2)


# Credit Score
unique_credscore <- unique(property_dataset$Credit_Score)
unique_credscore



#=============================================





#=============================================

# Ong Zi Yang TP065229
# Objective : To investigate the behaviour between credit score and annual income

#Analysis 1 : Is there a correlation between a person's credit score as well as their annual income? // THIS TOOK AN HOUR AND A HALF TO DO WHY

# determine min max values to determine logical range for a chart
min_income_val <- min(property_dataset$Annual_Income)

max_income_val <- max(property_dataset$Annual_Income)

min_income_val
max_income_val

# 0k - 180k salary range
property_dataset$Annual_Income <- as.numeric(property_dataset$Annual_Income)

# Define breaks for the histogram bins
breaks <- c(0, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000)

# Filter the dataset by Credit_Score "Good"
good_data <- subset(property_dataset, Credit_Score == "Good")
standard_data <- subset(property_dataset, Credit_Score == "Standard")
poor_data <- subset(property_dataset, Credit_Score == "Poor")


# good credit scores
ggplot(good_data, aes(x = cut(Annual_Income, breaks = breaks))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + # Add text labels for the counts
  scale_x_discrete(labels = c("0-20k", "21k-40k", "41k-60k", "61k-80k", "81k-100k",
                              "101k-120k", "121k-140k", "141k-160k", "161k-180k")) +
  scale_y_continuous(labels = seq(0, 100000, by = 20000),
                     breaks = seq(0, 100000, by = 20000),
                     expand = c(0, 200)) +  # Remove space around the axis
  labs(x = "Annual Income Range", y = "Frequency", title = "Distribution of Good Credit Scores by Annual Income") +
  theme_minimal()

# standard credit scores
ggplot(standard_data, aes(x = cut(Annual_Income, breaks = breaks))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + # Add text labels for the counts
  scale_x_discrete(labels = c("0-20k", "21k-40k", "41k-60k", "61k-80k", "81k-100k",
                              "101k-120k", "121k-140k", "141k-160k", "161k-180k")) +
  scale_y_continuous(labels = seq(0, 100000, by = 20000),
                     breaks = seq(0, 100000, by = 20000),
                     expand = c(0, 600)) +  # Remove space around the axis
  labs(x = "Annual Income Range", y = "Frequency", title = "Distribution of Standard Credit Scores by Annual Income") +
  theme_minimal()

# bad credit scores
ggplot(poor_data, aes(x = cut(Annual_Income, breaks = breaks))) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + # Add text labels for the counts
  scale_x_discrete(labels = c("0-20k", "21k-40k", "41k-60k", "61k-80k", "81k-100k",
                              "101k-120k", "121k-140k", "141k-160k", "161k-180k")) +
  scale_y_continuous(labels = seq(0, 100000, by = 20000),
                     breaks = seq(0, 100000, by = 20000),
                     expand = c(0, 400)) +  # Remove space around the axis
  labs(x = "Annual Income Range", y = "Frequency", title = "Distribution of Good Credit Scores by Annual Income") +
  theme_minimal()

#Analysis 2 : Does the occupation of an individual affect their annual income?
unique_occu <- unique(property_dataset$Occupation)
unique_occu

# find da average annual income for the chart
avg_annl_income <- property_dataset %>%
  group_by(property_dataset$Occupation) %>%
  summarize(Avg_Annual_Income = round(mean(Annual_Income, na.rm = TRUE)))
avg_annl_income


ggplot(avg_annl_income, aes(x = unique_occu, y = Avg_Annual_Income)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Avg_Annual_Income), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Average Annual Income by Occupation", x = "Occupation", y = "Average Annual Income")


#Analysis 3 : Is there a correlation between an individual's payment behaviour and their credit score?
unique_paymentbhv <- unique(property_dataset$Payment_Behaviour)
unique_paymentbhv

# Good credit score
ggplot(good_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values", x = "Payment Behaviour", y = "Frequency")

# poor credit score
ggplot(poor_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values", x = "Payment Behaviour", y = "Frequency")

# standard credit score
ggplot(standard_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values", x = "Payment Behaviour", y = "Frequency")


#Analysis 4 : Does an individual's credit utilization ratio have an effect on their credit score?







#=============================================



#=============================================

# Joshua Tioh Chee Yong TP065839
# Objective : To investigate the behaviour between credit score and percentile of interest rate

#Analysis 1 : is there a relationship between a customer's credit score and their interest rate?

property_dataset$Interest_Rate <- as.numeric(property_dataset$Interest_Rate)


# Calculate summary statistics
good_min <- min(good_data$Interest_Rate)
good_max <- max(good_data$Interest_Rate)
good_avg <- mean(good_data$Interest_Rate)

poor_min <- min(poor_data$Interest_Rate)
poor_max <- max(poor_data$Interest_Rate)
poor_avg <- mean(poor_data$Interest_Rate)

standard_min <- min(standard_data$Interest_Rate)
standard_max <- max(standard_data$Interest_Rate)
standard_avg <- mean(standard_data$Interest_Rate)


# Create bar chart
bar_data <- matrix(c(good_min, poor_min, standard_min,
                     good_max, poor_max, standard_max,
                     good_avg, poor_avg, standard_avg),
                   nrow = 3, byrow = TRUE)
barplot(bar_data, beside = TRUE, col = c("blue", "red", "green"),
        ylim = c(0, max(bar_data) + 10),
        names.arg = c("Good Data", "Poor Data", "Standard Data"),
        main = "Summary Statistics Bar Chart",
        xlab = "Datasets", ylab = "Values")
legend("topright", legend = c("Min", "Max", "Avg"),
       fill = c("blue", "red", "green"))






#Analysis 2 : Does the number of loans a customer have affect their interest rate?

#Analysis 3 : is there a relationship between a customer's credit score and their payment behaviour?

#Analysis 4 : Does the number of delayed payment affect a customer's interest rate?


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
# Objective : To investigate the behaviour between credit score and number of bank accounts

#Analysis 1 :

#Analysis 2 :

#Analysis 3 :

#Analysis 4 :


#=============================================





