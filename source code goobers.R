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
property_dataset$Age <- gsub("_", "", property_dataset$Age)
property_dataset$Age <- gsub("-", "", property_dataset$Age)
property_dataset <- property_dataset %>% mutate(Age = as.numeric(Age))

replace_with_mode_age <- function(x) {
  mode_value_age <- as.numeric(names(which.max(table(x))))
  x[is.na(x) | x > 100] <- mode_value_age
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Age = replace_with_mode_age(Age))


# finding unique values in occupation column
unique_occu <- unique(property_dataset$Occupation)
unique_occu

# Function to fill blank occupations with the most frequent occupation 
property_dataset <- property_dataset %>% mutate(Occupation = as.character(Occupation))
property_dataset$Occupation[property_dataset$Occupation == "_______"] <- NA

replace_with_mode_occu <- function(x) {
  mode_value_occu <- as.character(names(which.max(table(x))))
  x[is.na(x) | x == '_______'] <- mode_value_occu
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Occupation = replace_with_mode_occu(Occupation))

# Annual Income cleaning
# removes underscore from end of string
property_dataset$Annual_Income <- gsub("_", "", property_dataset$Annual_Income)
property_dataset

property_dataset <- property_dataset %>% mutate(Annual_Income = as.numeric(Annual_Income))
property_dataset$Annual_Income <- round(property_dataset$Annual_Income, 2)

annlinc_check <- unique(property_dataset$Annual_Income) # used to find logical range of values
annlinc_check_sorted <- sort(annlinc_check, decreasing = TRUE)
annlinc_check_sorted

# changes inconsistent values to consistent values

replace_with_mode_annlinc <- function(x) {
  mode_value_annlinc <- as.numeric(names(which.max(table(x))))
  x[x > 180000] <- mode_value_annlinc
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Annual_Income = replace_with_mode_annlinc(Annual_Income))


# Monthly Inhand Salary sorting
property_dataset <- property_dataset %>% mutate(Monthly_Inhand_Salary = as.numeric(Monthly_Inhand_Salary))
property_dataset$Monthly_Inhand_Salary <- round(property_dataset$Monthly_Inhand_Salary, 2)

replace_with_mode_mis <- function(x) {
  mode_value_mis <- as.numeric(names(which.max(table(x))))
  x[is.na(x)] <- mode_value_mis
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Monthly_Inhand_Salary = replace_with_mode_mis(Monthly_Inhand_Salary))


# Checks for unique bank account amount values
bank_check <- unique(property_dataset$Num_Bank_Accounts) # used to find logical range of values
bank_check 

# Bank Account Value cleaning
property_dataset <- property_dataset %>% mutate(Num_Bank_Accounts = as.numeric(Num_Bank_Accounts))
property_dataset$Num_Bank_Accounts <- gsub("-", "", property_dataset$Num_Bank_Accounts)
property_dataset

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
  mutate(Num_of_Loan = replace_with_mode_loannum(Num_of_Loan)) 
  #ungroup() %>%
  #mutate(Num_of_Loan = ifelse(Num_of_Loan == 0, "null", Num_of_Loan))

# type of loan cleaning
loantype_check <- unique(property_dataset$Type_of_Loan)
loantype_check

loantype_clean <- function(property_dataset) {
  property_dataset <- property_dataset %>%
    mutate(Type_of_Loan = ifelse(is.na(Type_of_Loan), "No Loans Taken", Type_of_Loan))
  
  return(property_dataset)
}

property_dataset <- loantype_clean(property_dataset)

# Delay from due date
duedate_vals <- unique(property_dataset$Delay_from_due_date)
duedate_vals_sort <- sort(duedate_vals, decreasing = FALSE)
duedate_vals_sort # values don't seem illogical at all...

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
  x[is.na(x) | x == "" | x > 17] <- mode_value_cred_inq
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Num_Credit_Inquiries = replace_with_mode_cred_inq(Num_Credit_Inquiries))


# Credit Mix cleaning
unique_cred_mix <- unique(property_dataset$Credit_Mix)
unique_cred_mix

property_dataset$Credit_Mix[property_dataset$Credit_Mix == "_"] <- NA

replace_with_mode_credmix <- function(x) {
  mode_value_credmix <- as.character(names(which.max(table(x))))
  x[is.na(x) | x == "_"] <- mode_value_credmix
  return(x)
}

property_dataset <- property_dataset %>%
  group_by(Customer_ID) %>%
  mutate(Credit_Mix = replace_with_mode_credmix(Credit_Mix))


# Outstanding Debt Cleaning
unique_od <- unique(property_dataset$Outstanding_Debt)
unique_od_sort <- sort(unique_od, decreasing = FALSE)
unique_od_sort

property_dataset$Outstanding_Debt <- gsub("_", "", property_dataset$Outstanding_Debt)

# Credit Utilization Ratio
property_dataset$Credit_Utilization_Ratio <- round(property_dataset$Credit_Utilization_Ratio, 2)
unq_cur <- unique(property_dataset$Credit_Utilization_Ratio)
unq_cur_sorted <- sort(unq_cur, decreasing = FALSE)
unq_cur_sorted


# Credit History Age cleaning (idk SOMEONE DO THIS)







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

property_dataset$Amount_invested_monthly[property_dataset$Amount_invested_monthly == "10000"] <- NA

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

#Analysis 1 : Does a person's credit score have an impact on their annual income? // THIS TOOK AN HOUR AND A HALF TO DO WHY // Histogram

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
  labs(x = "Annual Income Range", y = "Frequency", title = "Distribution of Poor Credit Scores by Annual Income") +
  theme_minimal()

#Analysis 2 : Does the credit score of an individual affect their EMI? // Jitter Plot

ggplot(property_dataset, aes(x = Credit_Score, y = Total_EMI_per_month, color = Credit_Score)) +
  geom_point(position = position_jitter(width = 0.2, height = 0)) +
  labs(title = "Total EMI per Month vs. Credit Score",
       x = "Credit Score",
       y = "Total EMI/ Month") +
  theme_minimal()


#Analysis 3 : Does an individual's payment behaviour affect their credit score? // Histogram
unique_paymentbhv <- unique(property_dataset$Payment_Behaviour)
unique_paymentbhv

# Good credit score
ggplot(good_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values (Good credit score)", x = "Payment Behaviour", y = "Frequency")

# poor credit score
ggplot(poor_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values (Poor credit score)", x = "Payment Behaviour", y = "Frequency")

# standard credit score
ggplot(standard_data, aes(x = Payment_Behaviour)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Payment Behaviour Values (Standard credit score)", x = "Payment Behaviour", y = "Frequency")


#Analysis 4 : Does an individual's credit utilization ratio have an effect on their credit score? // Jitter  Plot
install.packages("caret")
library(caret)

property_dataset$Credit_Score <- factor(property_dataset$Credit_Score, levels = c("Poor", "Standard", "Good"))
property_dataset$Credit_Utilization_Ratio <- as.numeric(property_dataset$Credit_Utilization_Ratio)

clean_data <- na.omit(property_dataset[is.finite(property_dataset$Credit_Utilization_Ratio), ])

set.seed(123)
training_indices <- sample(seq_len(nrow(clean_data)), size = 0.7 * nrow(clean_data))
training_data <- clean_data[training_indices, ]
testing_data <- clean_data[-training_indices, ]

model <- lm(Credit_Utilization_Ratio ~ Credit_Score, data = training_data)

summary(model)

testing_data$Predicted_Credit_Utilization_Ratio <- predict(model, newdata = testing_data)

comparison <- data.frame(Actual = testing_data$Credit_Utilization_Ratio, 
                         Predicted = testing_data$Predicted_Credit_Utilization_Ratio, 
                         Credit_Score = testing_data$Credit_Score)
head(comparison)

ggplot(comparison, aes(x = Actual, y = Predicted, color = Credit_Score)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Credit Utilization Ratios",
       x = "Actual Credit Utilization Ratio",
       y = "Predicted Credit Utilization Ratio") +
  theme_minimal() +
  scale_color_manual(values = c("Poor" = "blue", "Standard" = "green", "Good" = "orange")) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

recommend_credit_score <- function(current_ratio, target_ratio, model) {
  coefficients <- coef(model)
  required_change <- (target_ratio - coefficients[1]) / coefficients[2] - current_ratio
  return(required_change)
}

current_ratio <- 0.3  # Example current credit utilization ratio
target_ratio <- 0.1  # Example target credit utilization ratio
required_change <- recommend_credit_score(current_ratio, target_ratio, model)

cat("To achieve a credit utilization ratio of", target_ratio, "consider adjusting your credit score factors accordingly.\n")


# Extra Analysis 1 : Relationship between Credit Score and Delay from due date of an individual // Violin Plot

ggplot(property_dataset, aes(x = Credit_Score, y = Delay_from_due_date)) +
  geom_violin(fill = "skyblue", color = "black") +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add box plot for comparison
  labs(x = "Credit Score", y = "Delay from due date") +
  ggtitle("Violin Plot of Delay from Due Date by Credit Score") +
  theme_minimal()

unique_delayvals <- unique(property_dataset$Delay_from_due_date)
unique_delayvals

# Extra Analysis 2 : Relationship between Annual Income and Amount Invested Monthly // Pearson's Correlation | r > 0.5 // pearson's here outputs a coefficient of 0.6 == strong positive correlation

correlation <- cor(property_dataset$Annual_Income, property_dataset$Amount_invested_monthly, method = "pearson")
correlation # this shows the coefficient 

ggplot(property_dataset, aes(x = Annual_Income, y = Amount_invested_monthly)) +
  geom_point(color = "steelblue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add trend line
  labs(x = "Annual Income", y = "Amount Invested Monthly") +  # Axis labels
  ggtitle("Scatter Plot of Annual Income vs. Amount Invested Monthly") +  # Plot title
  geom_text(x = 50000, y = 2000, label = paste("Correlation coefficient:", round(cor(property_dataset$Annual_Income, property_dataset$Amount_invested_monthly), 2))) +  # Display correlation coefficient
  theme_minimal()  # Apply minimal theme



#=============================================



#=============================================

# Joshua Tioh Chee Yong TP065839
# Objective : To investigate the behaviour between credit score and percentile of interest rate

#Analysis 1 : is there a relationship between a customer's credit score and their interest rate?
good_data <- subset(property_dataset, Credit_Score == "Good")
standard_data <- subset(property_dataset, Credit_Score == "Standard")
poor_data <- subset(property_dataset, Credit_Score == "Poor")


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
bp <- barplot(bar_data, beside = TRUE, col = c("blue", "red", "green"),
        ylim = c(0, max(bar_data) + 10),
        names.arg = c("Good Score", "Poor Score", "Standard Score"),
        main = "Interest Rate Bar Chart",
        xlab = "Type of Credit Score", ylab = "Interest Rate")
legend("topright", legend = c("Min", "Max", "Avg"),
       fill = c("blue", "red", "green"))
text(x = bp, y = bar_data, label = round(bar_data, 2), pos = 3, cex = 0.8, col = "black")


#Analysis 2 : Does the number of loans a customer have affect their interest rate?
property_dataset$Num_of_Loan <- as.numeric(property_dataset$Num_of_Loan)

library(ggplot2)


ggplot(data_clean, aes(x = factor(Loans), y = rate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Interest Rate by Number of Loans",
       x = "Number of Loans",
       y = "Interest Rate") +
  theme_minimal()

# Ensure there are no NA values when calculating the maximum
max_loans <- max(data_clean$Loans, na.rm = TRUE)
max_rates <- max(data_clean$rate, na.rm = TRUE)


# Print the result
print(max_loans)

# Check for NA values in the columns
sum(is.na(data$Loans))
sum(is.na(data$rate))

# Remove rows with NA values
data_clean <- na.omit(data)

#Analysis 3 : Does occupation affect interest rate?

# Extract the unique strings from the specified column
unique_strings <- unique(property_dataset[["Occupation"]])
unique_strings

# detailed histogram of each occupation's interest rate
ggplot(property_dataset, aes(x = Interest_Rate)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Interest Rate by Occupation",
       x = "Interest Rate",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~ Occupation, scales = "free_y")

# Performing ANOVA
anova_result <- aov(Interest_Rate ~ Occupation, data = property_dataset)
summary(anova_result)

# Performing Tukey's HSD test if ANOVA is significant
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

# Plotting Tukey HSD results
plot(TukeyHSD(anova_result), las = 1)


#Analysis 4 : Does the number of delayed payment affect a customer's interest rate?

ggplot(property_dataset, aes(x = factor(Num_of_Delayed_Payment), 
                             y = Interest_Rate)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Interest Rate by Number of Delayed Payments",
       x = "Number of Delayed Payments",
       y = "Interest Rate") +
  theme_minimal()

max_interest_rate <- property_dataset %>%
  filter(Num_of_Delayed_Payment == -2) %>%
  pull(Interest_Rate) %>%
  mean(na.rm = TRUE)

max_interest_rate

sum(is.na(property_dataset$Num_of_Delayed_Payment))
sum(is.na(property_dataset$Interest_Rate))

# Extra Feature 1 

# Calculate the correlation coefficient after removing NA values
correlation <- cor(data_clean$Loans, data_clean$rate)
print(paste("Pearson correlation coefficient: ", round(correlation, 2)))

#=============================================



#=============================================

# Trilester Movees Tirilos TP066460
# Objective : To investigate the behaviour between credit score and number of credit cards  
#Analysis 1 : Is there a relationship between the credit score and the number of credit cards they hold?
ggplot(property_dataset, aes(x = as.factor(Num_Credit_Card), fill = Credit_Score)) +
  geom_bar(position = "dodge") + 
  labs(title = "Bar Plot of Credit Scores by Number of Credit Cards",
       x = "Number of Credit Cards",
       y = "Count") +
  theme_minimal()

#Analysis 2 : Does the frequency of credit inquiries affect the credit score?
# Count plot for Frequency of Credit Inquiries
ggplot(property_dataset, aes(x = as.factor(Num_Credit_Inquiries))) +
  geom_bar(fill = "blue") +
  labs(title = "Count Plot of Credit Inquiries",
       x = "Number of Credit Inquiries",
       y = "Count") +
  theme_minimal()

# Count plot for Credit Score
ggplot(property_dataset, aes(x = Credit_Score)) +
  geom_bar(fill = "blue") +
  labs(title = "Count Plot of Credit Scores",
       x = "Credit Score",
       y = "Count") +
  theme_minimal()


# Analysis 3 : Does the number of credit cards affect the average utilization ratio?
library(reshape2)
library(ggplot2)

# Aggregate data to count occurrences of each combination
heatmap_data <- property_dataset %>%
  group_by(Num_Credit_Card, Credit_Utilization_Ratio) %>%
  summarise(count = n(), .groups = 'drop')

# Create heatmap
ggplot(heatmap_data, aes(x = factor(Num_Credit_Card), y = Credit_Utilization_Ratio, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Heatmap of Credit Utilization Ratio by Number of Credit Cards",
       x = "Number of Credit Cards",
       y = "Credit Utilization Ratio",
       fill = "Count") +
  theme_minimal()

#Analysis 4 : What changes can be made to improve credit scores?
credit_score_improvement <- property_dataset %>%
  group_by(Credit_Score) %>%
  summarise(
    avg_num_credit_cards = mean(Num_Credit_Card, na.rm = TRUE),
    avg_num_loans = mean(Num_of_Loan, na.rm = TRUE),
    avg_utilization_ratio = mean(Credit_Utilization_Ratio, na.rm = TRUE),
    avg_delay_payments = mean(Num_of_Delayed_Payment, na.rm = TRUE)
  )

# Generate plot to show the relationship between credit scores and improvement factors
# Factors: Num_Credit_Card, Num_of_Loan, Credit_Utilization_Ratio, Num_of_Delayed_Payment
ggplot(property_dataset, aes(x = Credit_Score)) +
  geom_boxplot(aes(y = Num_Credit_Card), fill = "blue", position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(y = Num_of_Loan), fill = "green", position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(y = Credit_Utilization_Ratio), fill = "red", position = position_dodge(width = 0.8)) +
  geom_boxplot(aes(y = Num_of_Delayed_Payment), fill = "yellow", position = position_dodge(width = 0.8)) +
  labs(title = "Factors Affecting Credit Scores",
       x = "Credit Score",
       y = "Value",
       fill = "Factors") +
  theme_minimal()

# Analysis 5: Predicting Credit Scores based on Annual Income and Credit Utilization Ratio
library(ggplot2)
library(dplyr)

# Ensure the data has no missing values 
property_dataset <- na.omit(property_dataset[c("Credit_Score", "Annual_Income", "Credit_Utilization_Ratio")])
property_dataset$Annual_Income <- as.numeric(property_dataset$Annual_Income)

# Plot
ggplot(property_dataset, aes(x = Annual_Income, y = Credit_Utilization_Ratio, color = Credit_Score)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  geom_rug(sides = "b", color = "black", alpha = 0.3) +  
  labs(x = "Annual Income", y = "Credit Utilization Ratio",
       title = "Distribution of Credit Scores with Annual Income and Credit Utilization Ratio",
       color = "Credit Score") +  
  theme_minimal()  
credit_score_improvement <- property_dataset %>%
  group_by(Credit_Utilization_Ratio, Num_Credit_Card) %>%
  summarise(avg_credit_score = mean(Credit_Score, na.rm = TRUE))

ggplot(credit_score_improvement, aes(x = Credit_Utilization_Ratio, y = Num_Credit_Card, fill = avg_credit_score)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Improving Credit Scores by Managing Credit Utilization Ratio and Number of Credit Cards",
       x = "Credit Utilization Ratio",
       y = "Number of Credit Cards",
       fill = "Average Credit Score") +
  theme_minimal()

# Extra feature 1
# Heatmap
library(reshape2)
library(ggplot2)

# Aggregate data to count occurrences of each combination
heatmap_data <- property_dataset %>%
  group_by(Num_Credit_Card, Credit_Utilization_Ratio) %>%
  summarise(count = n(), .groups = 'drop')

# Create the heatmap
ggplot(heatmap_data, aes(x = factor(Num_Credit_Card), y = Credit_Utilization_Ratio, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Heatmap of Credit Utilization Ratio by Number of Credit Cards",
       x = "Number of Credit Cards",
       y = "Credit Utilization Ratio",
       fill = "Count") +
  theme_minimal()

# Extra feature 2
# rug_plot
library(ggplot2)
library(dplyr)

# Ensure the data has no missing values for the required variables
property_dataset <- na.omit(property_dataset[c("Credit_Score", "Annual_Income", "Credit_Utilization_Ratio")])
property_dataset$Annual_Income <- as.numeric(cleaned_data$Annual_Income)

# Create the plot with geom_rug
ggplot(property_dataset, aes(x = Annual_Income, y = Credit_Utilization_Ratio, color = Credit_Score)) +
  geom_point(alpha = 0.6) +  # Add points with some transparency
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  geom_rug(sides = "b", color = "black", alpha = 0.3) +  # Add rug plot on x-axis (bottom)
  labs(x = "Annual Income", y = "Credit Utilization Ratio",
       title = "Distribution of Credit Scores with Annual Income and Credit Utilization Ratio",
       color = "Credit Score") +  # Axis labels and plot title
  theme_minimal()  # Apply minimal theme

#=============================================



#=============================================

# Wong Wei Hann TP065820
# Objective : To investigate the behaviour between credit score and number of bank accounts

#Analysis 1 : is there a relationship between the customers' credit score and the number of bank accounts they hold? / Stacked Bar Chart
# descriptive & Dianogstic
# Convert Credit_Score to factor to maintain the order
property_dataset$Credit_Score <- factor(property_dataset$Credit_Score, levels = c("Poor", "Standard", "Good"))

# Create a count table for the plot
count_table <- property_dataset %>%
  group_by(Credit_Score, Num_Bank_Accounts) %>%
  summarise(count = n()) %>%
  ungroup()

# Plotting the stacked bar chart

ggplot(count_table, aes(x = Credit_Score, y = count, fill = factor(Num_Bank_Accounts))) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Credit Score and Number of Bank Accounts",
       x = "Credit Score",
       y = "Frequency",
       fill = "Number of Bank Accounts") +
  theme_minimal()

#Analysis 2 : does the number of bank accounts correlate with the number of loans? 

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(car)

# Perform ANOVA
anova_model <- aov(Num_of_Loan ~ Num_Bank_Accounts, data = property_dataset)
summary(anova_model)

# Post-hoc analysis if ANOVA is significant
if (summary(anova_model)[[1]][["Pr(>F)"]][1] < 0.05) {
  tukey_results <- TukeyHSD(anova_model)
  print(tukey_results)
}

# Visualize the ANOVA results using boxplots
ggplot(property_dataset, aes(x = Num_Bank_Accounts, y = Num_of_Loan)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Number of Loans by Number of Bank Accounts",
       x = "Number of Bank Accounts",
       y = "Number of Loans") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

#Analysis 3 : is there a relationship between a customers' credit score and their account payment behaviour

# Create a contingency table
contingency_table <- table(property_dataset$Credit_Score, property_dataset$Payment_Behaviour)

# Perform Chi-Square Test of Independence
chi_square_test <- chisq.test(contingency_table)

# Create a facet grid plot
ggplot(property_dataset, aes(x = Payment_Behaviour, fill = Payment_Behaviour)) +
  geom_bar() +
  facet_grid(. ~ Credit_Score) +
  labs(title = "Facet Grid of Payment Behaviour by Credit Score",
       x = "Payment Behaviour",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Print the results
print(chi_square_test)
#Analysis 4 : does the number of bank accounts customers' affect interest rate

library(ggridges)

ggplot(property_dataset, aes(x = Interest_Rate, y = factor(Num_Bank_Accounts), fill = factor(Num_Bank_Accounts))) +
  geom_density_ridges(alpha = 0.7) +
  labs(title = "Ridgeline Plot of Interest Rate by Number of Bank Accounts",
       x = "Interest Rate",
       y = "Number of Bank Accounts") +
  theme_minimal()

# Extra Analysis 1 : is there a relationship between a customer's credit score and their change credit limit?
#Predictive Analysis

library(randomForest)
library(caTools)

# Split the data into training and test sets
set.seed(123)
split <- sample.split(property_dataset$Credit_Score, SplitRatio = 0.8)
training_set <- subset(property_dataset, split == TRUE)
test_set <- subset(property_dataset, split == FALSE)

# Build the random forest model
classifier_rf <- randomForest(Credit_Score ~ Changed_Credit_Limit, data = training_set)

# Summary of the model
print(classifier_rf)

# Make predictions
prediction_rf <- predict(classifier_rf, test_set)

# Prediction Accuracy
accuracy <- mean(prediction_rf == test_set$Credit_Score)
cat("Prediction Accuracy:", accuracy, "\n")

# Confusion Matrix
conf_matrix_rf <- confusionMatrix(prediction_rf, test_set$Credit_Score)
print(conf_matrix_rf)

# Extra Analysis 2 : is there a relationship between a customer's credit score and number of delayed payments?
# Create a density plot
ggplot(property_dataset, aes(x = Num_of_Delayed_Payment, fill = Credit_Score)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Number of Delayed Payments by Credit Score",
       x = "Number of Delayed Payments",
       y = "Density") +
  theme_minimal()
#=============================================





