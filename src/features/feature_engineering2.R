##################################################################
# feature engineering
##################################################################

library("dplyr")
library("lazyeval")

# Read the file
df <- read.csv(file="C://Users/g557428/Projects/seis736_ml_project/data/raw/cs-training.csv")

# Remove the row index
df <- df[,-1]

# Perform summarization on the dataframe - groupbed by the dependent variable
do_summary <- function(df, varname) {
  df %>% group_by(SeriousDlqin2yrs) %>% summarize_(obs = "n()",
                                                   mean = interp(~mean(x, na.rm=TRUE), x=as.name(varname)),
                                                   med = interp(~median(x, na.rm=TRUE), x=as.name(varname)),
                                                   min = interp(~min(x, na.rm=TRUE), x=as.name(varname)),
                                                   max = interp(~max(x, na.rm=TRUE), x=as.name(varname)),
                                                   sd = interp(~sd(x, na.rm=TRUE), x=as.name(varname)),
                                                   na = interp(~sum(is.na(x)), x=as.name(varname)))
}

# Function to find the outliers that are more than 1.5 x the IQR
find_outliers <- function(x) {
  q <- as.vector(quantile(x, c(0.25, 0.50, 0.75)))
  q1 <- q[1]
  q3 <- q[3]
  iqr <- q3-q1
  return(c(max(min(x), q1 - 1.5 * iqr), min(max(x), q3 + 1.5 * iqr)))
}

# Function to find the outliers that are less than 1.5 x the IQR
find_majority <- function(x) {
  q <- as.vector(quantile(x, c(0.25, 0.50, 0.75)))
  return(c(q[1],q[3]))
}

##################################################################
# Num Times 30-59 days past due but not worse w/in 2 years
##################################################################

do_summary(df, "NumberOfTime30.59DaysPastDueNotWorse")
do_summary(subset(df, NumberOfTime30.59DaysPastDueNotWorse<96), "NumberOfTime30.59DaysPastDueNotWorse")

# Counts in the 90s are likely "NA" values. Let's create a single NA value
df$NumberOfTime30.59DaysPastDueNotWorse <- ifelse(df$NumberOfTime30.59DaysPastDueNotWorse > 90, 
                                                  -1, 
                                                  df$NumberOfTime30.59DaysPastDueNotWorse)

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD30.59Bucket <- case_when(df$NumberOfTime30.59DaysPastDueNotWorse < 0 ~ "UN",
                              df$NumberOfTime30.59DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime30.59DaysPastDueNotWorse <= 3 ~ "1-3",
                              df$NumberOfTime30.59DaysPastDueNotWorse > 3 ~ "4+"
                              )

table(df$PD30.59Bucket)

##################################################################
# Num Times 60-89 days past due but not worse w/in 2 years
##################################################################

do_summary(df, "NumberOfTime60.89DaysPastDueNotWorse")
do_summary(subset(df, NumberOfTime60.89DaysPastDueNotWorse<96), "NumberOfTime60.89DaysPastDueNotWorse")

# Counts in the 90s are likely "NA" values. Let's create a single NA value
df$NumberOfTime60.89DaysPastDueNotWorse <- ifelse(df$NumberOfTime60.89DaysPastDueNotWorse > 90, 
                                                  -1, 
                                                  df$NumberOfTime60.89DaysPastDueNotWorse)

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD60.89Bucket <- case_when(df$NumberOfTime60.89DaysPastDueNotWorse < 0 ~ "UN",
                              df$NumberOfTime60.89DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime60.89DaysPastDueNotWorse <= 3 ~ "1-3",
                              df$NumberOfTime60.89DaysPastDueNotWorse > 3 ~ "4+")

prop.table(table(df$PD60.89Bucket, df$SeriousDlqin2yrs),2)

##################################################################
# Num Times 90+ days past due
##################################################################

do_summary(df, "NumberOfTimes90DaysLate")
do_summary(subset(df, NumberOfTimes90DaysLate<96), "NumberOfTimes90DaysLate")

# Counts in the 90s are likely "NA" values. Let's create a single NA value
df$NumberOfTimes90DaysLate <- ifelse(df$NumberOfTimes90DaysLate > 90, 
                                     -1, 
                                     df$NumberOfTimes90DaysLate)

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD90Bucket <- case_when(df$NumberOfTimes90DaysLate < 0 ~ "UN",
                           df$NumberOfTimes90DaysLate == 0 ~ "0",
                           df$NumberOfTimes90DaysLate <= 3 ~ "1-3",
                           df$NumberOfTimes90DaysLate > 3 ~ "4+")

df$PD90Bucket <- as.factor(df$PD90Bucket)

##################################################################
# Total times past due
##################################################################

# Only sum if the value isn't an NA
df$TotalPastDue <- ifelse(df$NumberOfTime30.59DaysPastDueNotWorse < 0, 0, df$NumberOfTime30.59DaysPastDueNotWorse) +
                   ifelse(df$NumberOfTime60.89DaysPastDueNotWorse < 0, 0, df$NumberOfTime60.89DaysPastDueNotWorse) +
                   ifelse(df$NumberOfTimes90DaysLate < 0, 0, df$NumberOfTimes90DaysLate)

# Summarize
do_summary(df, "TotalPastDue")

##################################################################
# Past Due Type
##################################################################

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PastDueBucket <- case_when(df$TotalPastDue == 0 ~ "0",
                              df$TotalPastDue <= 3 ~ "1-3",
                              df$TotalPastDue <= 7 ~ "4-7",
                              df$TotalPastDue > 7 ~ "8+")

# View the distributions
prop.table(table(df$PastDueBucket, df$SeriousDlqin2yrs),2)

##################################################################
# Number of Dependents
##################################################################

do_summary(df, "NumberOfDependents")
summary(df$NumberOfDependents)

# For now lets just use the median value
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents), 0, df$NumberOfDependents)

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$DependentsBucket <- case_when(df$NumberOfDependents == 0 ~ "0",
                                 df$NumberOfDependents == 1 ~ "1",
                                 df$NumberOfDependents == 2 ~ "2-3",
                                 df$NumberOfDependents == 3 ~ "2-3",
                                 df$NumberOfDependents > 3 ~ "4+")

##################################################################
# Debt Ratio
##################################################################

# Theoretically this is a percentage, but there are some non-percentage looking values here
do_summary(df, "DebtRatio")
summary(df$DebtRatio)

# Cap it at 100%
df$DebtRatio <- ifelse(df$DebtRatio > 1.0, 1.0, df$DebtRatio)

boxplot(df$DebtRatio ~ df$SeriousDlqin2yrs)

##################################################################
# Revolving Utilization
##################################################################

# This is supposed to be a percentage, but there are some outrageous values
do_summary(df, "RevolvingUtilizationOfUnsecuredLines")

# Cap it at 100%
df$RevolvingUtilizationOfUnsecuredLines <- ifelse(df$RevolvingUtilizationOfUnsecuredLines > 1.0, 
                                                  1.0, 
                                                  df$RevolvingUtilizationOfUnsecuredLines)

boxplot(df$RevolvingUtilizationOfUnsecuredLines ~ df$SeriousDlqin2yrs, outline=FALSE)

##################################################################
# Serious Delinquency Ever
##################################################################

# Did the customer ever have a serious delinquency - 90 or more days delinquent
df$SeriousDlqFlag <- ifelse(df$NumberOfTimes90DaysLate > 0, 1, 0)

table(df$SeriousDlqFlag, df$SeriousDlqin2yrs)
prop.table(table(df$SeriousDlqFlag, df$SeriousDlqin2yrs),2)

##################################################################
# Risk Index = Utilization by Debt Ratio + weighted delinquency
##################################################################

df$RiskIndex <- (df$DebtRatio * df$RevolvingUtilizationOfUnsecuredLines) + (df$TotalPastDue/2)

summary(df$RiskIndex)
do_summary(df, "RiskIndex")

boxplot(df$RiskIndex ~ df$SeriousDlqin2yrs, outline=FALSE)

##################################################################
# Age Buckets
##################################################################

df$AgeBucket <- case_when(df$age < 18 ~ "0-17",
                          df$age < 22 ~ "18-21",
                          df$age < 36 ~ "22-35",
                          df$age < 56 ~ "36-55",
                          df$age < 76 ~ "56-75",
                          df$age >= 76 ~ "76+")

table(df$AgeBucket)
table(df$AgeBucket, df$SeriousDlqin2yrs)

##################################################################
# Open Credit Lines and Loans
##################################################################

do_summary(df, "NumberOfOpenCreditLinesAndLoans")
boxplot(df$NumberOfOpenCreditLinesAndLoans ~ df$SeriousDlqin2yrs, outline=FALSE)

df$CreditLinesBucket <- case_when(df$NumberOfOpenCreditLinesAndLoans <= 0 ~ "0",
                                  df$NumberOfOpenCreditLinesAndLoans < 5 ~ "1-4",
                                  df$NumberOfOpenCreditLinesAndLoans < 9 ~ "5-8",
                                  df$NumberOfOpenCreditLinesAndLoans >= 9 ~ "9+")

round(prop.table(table(df$CreditLinesBucket))*100,2)
round(prop.table(table(df$CreditLinesBucket, df$SeriousDlqin2yrs),2)*100,2)

##################################################################
# Open Real Estate Lines and Loans
##################################################################

do_summary(df, "NumberRealEstateLoansOrLines")
boxplot(df$NumberRealEstateLoansOrLines ~ df$SeriousDlqin2yrs, outline=FALSE)

df$RealEstateBucket <- case_when(df$NumberRealEstateLoansOrLines <= 0 ~ "0",
                                 df$NumberRealEstateLoansOrLines < 2 ~ "1",
                                 df$NumberRealEstateLoansOrLines < 3 ~ "2",
                                 df$NumberRealEstateLoansOrLines < 6 ~ "3-5",
                                 df$NumberRealEstateLoansOrLines >= 6 ~ "6+")

round(prop.table(table(df$RealEstateBucket))*100,2)
round(prop.table(table(df$RealEstateBucket, df$SeriousDlqin2yrs),2)*100,2)

##################################################################
# Income
##################################################################

# There are a lot of NA incomes. This is potentially an important value.
# Maybe we shouldn't use the median
do_summary(df, "MonthlyIncome")

# Creating dummy variables
df$PD30.59Bucket <- as.factor(df$PD30.59Bucket)
df$PD60.89Bucket <- as.factor(df$PD60.89Bucket)
df$PD90Bucket <- as.factor(df$PD90Bucket)
df$PastDueBucket <- as.factor(df$PastDueBucket)
df$DependentsBucket <- as.factor(df$DependentsBucket)
df$SeriousDlqFlag <- as.factor(df$SeriousDlqFlag)
df$AgeBucket <- as.factor(df$AgeBucket)
df$CreditLinesBucket <- as.factor(df$CreditLinesBucket)
df$RealEstateBucket <- as.factor(df$RealEstateBucket)

# Create a test and training dataset
df.train <- subset(df, is.na(MonthlyIncome)==FALSE)

# Formula to use for the regression model
f <- MonthlyIncome ~ 
  RevolvingUtilizationOfUnsecuredLines +
  age +
  NumberOfTime30.59DaysPastDueNotWorse +
  DebtRatio +
  NumberOfOpenCreditLinesAndLoans +
  NumberOfTimes90DaysLate +
  NumberRealEstateLoansOrLines +
  NumberOfTime60.89DaysPastDueNotWorse +
  NumberOfDependents +
#  PD30.59Bucket +
#  PD60.89Bucket +
#  PD90Bucket +
  TotalPastDue +
#  PastDueBucket +
#  DependentsBucket +
  RiskIndex +
  SeriousDlqFlag 
#  AgeBucket +
#  CreditLinesBucket +
#  RealEstateBucket

# Repeat the steps below until a desired fit is acheived
#------------------------------------------------------------#
# Fit the linear model
fit <- lm(formula=f, data=df.train) 
summary(fit)

# Map the residuals values to the test dataset
df.train$resid <- resid(fit)

# Find the value of 2x the standard deviation of the residuals
sd2 <- 2*sd(df.train$resid) 

# Flagging hte outliers - those obs whose residuals are more than 2x stdv
df.train$outs <- ifelse(abs(df.train$resid) > sd2, 1, 0)
table(df.train$outs)

# Removing the outliers
df.train <- subset(df.train, outs==0) 

# Remove the residuals
df.train <- df.train[,-c(23,24)]
#------------------------------------------------------------#

# Save the modeled income model
save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/modeledincome.rdata")

# Make the predictions
df$ModeledIncome <- predict(fit, df)

# If below 0, set to 0
df$ModeledIncome <- ifelse(df$ModeledIncome < 0, 0, df$ModeledIncome)

# If monthly is N/A, then use the modeled income, otherwise keep the existing value
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome), df$ModeledIncome, df$MonthlyIncome)

head(subset(df, select=c(MonthlyIncome,ModeledIncome)), n=20)

do_summary(df, "ModeledIncome")
boxplot(df$ModeledIncome ~ df$SeriousDlqin2yrs, outline=FALSE)

# Cleanup
rm(df.train, df.test, fit, sd2)

# Creating logical income buckets
df$IncomeBucket <- case_when(df$MonthlyIncome < 1000 ~ "<1000",
                             df$MonthlyIncome < 2000 ~ "1000-1999",
                             df$MonthlyIncome < 4000 ~ "2000-3999",
                             df$MonthlyIncome < 8000 ~ "4000-7999",
                             df$MonthlyIncome < 16000 ~ "8000-15999",
                             df$MonthlyIncome < 32000 ~ "16000-31999",
                             df$MonthlyIncome < 64000 ~ "32000-63999",
                             df$MonthlyIncome >= 64000 ~ "64000+")

df$IncomeBucket <- as.factor(df$IncomeBucket)

boxplot(df$MonthlyIncome ~ df$IncomeBucket, outline=FALSE)

##################################################################
# Write to CSV
##################################################################

write.csv(x=df, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-modified.csv", row.names=FALSE)
