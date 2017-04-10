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

###################################################################
# Doing some basic data viz
###################################################################
library("ggplot2")

df.sum <- df %>% group_by(SeriousDlqin2yrs) %>% summarize(obs = n() / 1000)

ggplot(data=df.sum, aes(x=as.factor(SeriousDlqin2yrs), y=obs)) +
  geom_bar(stat="identity", fill="steelblue", width=0.5) +
  geom_text(aes(label=obs), vjust=1.6, color="white", size=3.5)+
  theme_minimal()


##################################################################
# Num Times 30-59 days past due but not worse w/in 2 years
##################################################################

# Summarize
do_summary(df, "NumberOfTime30.59DaysPastDueNotWorse")
do_summary(subset(df, NumberOfTime30.59DaysPastDueNotWorse<96), "NumberOfTime30.59DaysPastDueNotWorse")

# Counts in the 90s are likely "NA" values. Set these to 0
df$NumberOfTime30.59DaysPastDueNotWorse <- ifelse(df$NumberOfTime30.59DaysPastDueNotWorse > 90, 
                                                  0, 
                                                  df$NumberOfTime30.59DaysPastDueNotWorse)

# Visualize the distribution
summary(df$NumberOfTime30.59DaysPastDueNotWorse)
plot(density(df$NumberOfTime30.59DaysPastDueNotWorse))
boxplot(df$NumberOfTime30.59DaysPastDueNotWorse ~ df$SeriousDlqin2yrs, outline=FALSE)

# Creating buckets
df$PD30.59Bucket <- case_when(df$NumberOfTime30.59DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime30.59DaysPastDueNotWorse == 1 ~ "1",
                              df$NumberOfTime30.59DaysPastDueNotWorse == 2 ~ "2",
                              df$NumberOfTime30.59DaysPastDueNotWorse == 3 ~ "3",
                              df$NumberOfTime30.59DaysPastDueNotWorse > 3 ~ "4+"
                              )

# Distribution of the past due bucket
table(df$PD30.59Bucket)
sum(is.na(df$PD30.59Bucket))

##################################################################
# Num Times 60-89 days past due but not worse w/in 2 years
##################################################################

do_summary(df, "NumberOfTime60.89DaysPastDueNotWorse")
do_summary(subset(df, NumberOfTime60.89DaysPastDueNotWorse<96), "NumberOfTime60.89DaysPastDueNotWorse")

# Counts in the 90s are likely "NA" values. Set these to 0
df$NumberOfTime60.89DaysPastDueNotWorse <- ifelse(df$NumberOfTime60.89DaysPastDueNotWorse > 90, 
                                                  0, 
                                                  df$NumberOfTime60.89DaysPastDueNotWorse)

# Visualize the distribution
summary(df$NumberOfTime60.89DaysPastDueNotWorse)
plot(density(df$NumberOfTime60.89DaysPastDueNotWorse))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(NumberOfTime60.89DaysPastDueNotWorse))[,1]))
boxplot(df$NumberOfTime60.89DaysPastDueNotWorse ~ df$SeriousDlqin2yrs, outline=FALSE)

# Creating buckets. the 90+ bucket are the unknowns
df$PD60.89Bucket <- case_when(df$NumberOfTime60.89DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime60.89DaysPastDueNotWorse == 1 ~ "1",
                              df$NumberOfTime60.89DaysPastDueNotWorse == 2 ~ "2",
                              df$NumberOfTime60.89DaysPastDueNotWorse == 3 ~ "3",
                              df$NumberOfTime60.89DaysPastDueNotWorse > 3 ~ "4+")

# Show the distribution
prop.table(table(df$PD60.89Bucket))

##################################################################
# Num Times 90+ days past due
##################################################################

do_summary(df, "NumberOfTimes90DaysLate")
do_summary(subset(df, NumberOfTimes90DaysLate<96), "NumberOfTimes90DaysLate")

# Counts in the 90s are likely "NA" values. Set these to 0
df$NumberOfTimes90DaysLate <- ifelse(df$NumberOfTimes90DaysLate > 90, 
                                     0, 
                                     df$NumberOfTimes90DaysLate)

# Visualize the distribution
summary(df$NumberOfTimes90DaysLate)
plot(density(df$NumberOfTimes90DaysLate))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(NumberOfTimes90DaysLate))[,1]))
boxplot(df$NumberOfTimes90DaysLate ~ df$SeriousDlqin2yrs, outline=FALSE)

# Creating buckets. the 90+ bucket are the unknowns
df$PD90Bucket <- case_when(df$NumberOfTimes90DaysLate == 0 ~ "0",
                           df$NumberOfTimes90DaysLate == 1 ~ "1",
                           df$NumberOfTimes90DaysLate == 2 ~ "2",
                           df$NumberOfTimes90DaysLate == 3 ~ "3",
                           df$NumberOfTimes90DaysLate == 4 ~ "4",
                           df$NumberOfTimes90DaysLate > 4 ~ "5+")

# Show the distribution
prop.table(table(df$PD90Bucket))

##################################################################
# Total times past due
##################################################################

# Creating a total number of times past due feature
df$TotalPastDue <- df$NumberOfTime30.59DaysPastDueNotWorse +
                   df$NumberOfTime60.89DaysPastDueNotWorse +
                   df$NumberOfTimes90DaysLate

# Summarize
do_summary(df, "TotalPastDue")

# Visualize the distribution
summary(df$TotalPastDue)
plot(density(df$TotalPastDue))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(TotalPastDue))[,1]))
boxplot(df$TotalPastDue ~ df$SeriousDlqin2yrs, outline=FALSE)

# Bucketing the total past due feature
df$PastDueBucket <- case_when(df$TotalPastDue == 0 ~ "0",
                              df$TotalPastDue == 1 ~ "1",
                              df$TotalPastDue == 2 ~ "2",
                              df$TotalPastDue == 3 ~ "3",
                              df$TotalPastDue == 4 ~ "4",
                              df$TotalPastDue == 5 ~ "5",
                              df$TotalPastDue == 6 ~ "6",
                              df$TotalPastDue == 7 ~ "7",
                              df$TotalPastDue > 7 ~ "8+")

# View the distributions
prop.table(table(df$PastDueBucket))

##################################################################
# Number of Dependents
##################################################################

do_summary(df, "NumberOfDependents")
summary(df$NumberOfDependents)

# For now lets just use the median value
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents), 0, df$NumberOfDependents)

# Visualize the distribution
summary(df$NumberOfDependents)
plot(density(df$NumberOfDependents))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(NumberOfDependents))[,1]))

# Bucketing the dependents
df$DependentsBucket <- case_when(df$NumberOfDependents == 0 ~ "0",
                                 df$NumberOfDependents == 1 ~ "1",
                                 df$NumberOfDependents == 2 ~ "2",
                                 df$NumberOfDependents == 3 ~ "3",
                                 df$NumberOfDependents == 4 ~ "4",
                                 df$NumberOfDependents > 4 ~ "5+")

# View the distributions
prop.table(table(df$DependentsBucket))

##################################################################
# Debt Ratio
##################################################################

# Theoretically this is a percentage, but there are some non-percentage looking values here
do_summary(df, "DebtRatio")
summary(df$DebtRatio)

# Outliers are 1.5x the IQR
ol <- find_outliers(df$DebtRatio)[2]

# Anything above the outliers threshold - just set to the threshold
df$DebtRatio <- ifelse(df$DebtRatio > ol, ol, df$DebtRatio)

# Visualize the debt ratio
boxplot(df$DebtRatio ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$DebtRatio))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(DebtRatio))[,1]))

##################################################################
# Revolving Utilization
##################################################################

# This is supposed to be a percentage, but there are some outrageous values
do_summary(df, "RevolvingUtilizationOfUnsecuredLines")

# Outliers are 1.5x the IQR
ol <- find_outliers(df$RevolvingUtilizationOfUnsecuredLines)[2]

# Anything above the outliers threshold - just set to the threshold
df$RevolvingUtilizationOfUnsecuredLines <- ifelse(df$RevolvingUtilizationOfUnsecuredLines > ol, 
                                                  ol, 
                                                  df$RevolvingUtilizationOfUnsecuredLines)

# Visualize 
boxplot(df$RevolvingUtilizationOfUnsecuredLines ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$RevolvingUtilizationOfUnsecuredLines))
plot(density(subset(df, SeriousDlqin2yrs==1, select=c(RevolvingUtilizationOfUnsecuredLines))[,1]))

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

df$RiskIndex <- (df$DebtRatio * df$RevolvingUtilizationOfUnsecuredLines) + (df$TotalPastDue / 2)
df$RiskIndex <- (df$RiskIndex - min(df$RiskIndex)) / (max(df$RiskIndex) - min(df$RiskIndex))

summary(df$RiskIndex)
do_summary(df, "RiskIndex")

# Visualize
boxplot(df$RiskIndex ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$RiskIndex))
plot(density(subset(df, SeriousDlqin2yrs==0, select=c(RiskIndex))[,1]))
lines(density(subset(df, SeriousDlqin2yrs==1, select=c(RiskIndex))[,1]))

##################################################################
# Ratio of serious dlqs to total dlqs
##################################################################

df$SeriousDlqRatio <- df$NumberOfTimes90DaysLate / df$TotalPastDue
df$SeriousDlqRatio <- ifelse(is.nan(df$SeriousDlqRatio), 0, df$SeriousDlqRatio)

# Visualize
boxplot(df$SeriousDlqRatio ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$SeriousDlqRatio))
plot(density(subset(df, SeriousDlqin2yrs==0, select=c(SeriousDlqRatio))[,1]))
lines(density(subset(df, SeriousDlqin2yrs==1, select=c(SeriousDlqRatio))[,1]))

##################################################################
# Age Buckets
##################################################################

# Summarize
do_summary(df, "age")
do_summary(subset(df, age < 22), "age")

# If the age is less than 21, then set to 21
df$age <- ifelse(df$age < 21, 21, df$age)

# Visualize
boxplot(df$age ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$age))
plot(density(subset(df, SeriousDlqin2yrs==0, select=c(age))[,1]))
lines(density(subset(df, SeriousDlqin2yrs==1, select=c(age))[,1]))

# Creating buckets
df$AgeBucket <- case_when(df$age == 21 ~ "21",
                          df$age < 36 ~ "22-35",
                          df$age < 56 ~ "36-55",
                          df$age < 76 ~ "56-75",
                          df$age < 96 ~ "86-95",
                          df$age > 95 ~ "96+")

table(df$AgeBucket)
table(df$AgeBucket, df$SeriousDlqin2yrs)

##################################################################
# Open Credit Lines and Loans
##################################################################

do_summary(df, "NumberOfOpenCreditLinesAndLoans")

# Outliers are 1.5x the IQR
ol <- find_outliers(df$NumberOfOpenCreditLinesAndLoans)[2]

# Anything above the outliers threshold - just set to the threshold
df$NumberOfOpenCreditLinesAndLoans <- ifelse(df$NumberOfOpenCreditLinesAndLoans > ol, 
                                             ol, 
                                             df$NumberOfOpenCreditLinesAndLoans)

# Visualize
boxplot(df$NumberOfOpenCreditLinesAndLoans ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$NumberOfOpenCreditLinesAndLoans))
plot(density(subset(df, SeriousDlqin2yrs==0, select=c(NumberOfOpenCreditLinesAndLoans))[,1]))
lines(density(subset(df, SeriousDlqin2yrs==1, select=c(NumberOfOpenCreditLinesAndLoans))[,1]))

# Creating buckets
df$CreditLinesBucket <- case_when(df$NumberOfOpenCreditLinesAndLoans == 0 ~ "0",
                                  df$NumberOfOpenCreditLinesAndLoans == 1 ~ "1",
                                  df$NumberOfOpenCreditLinesAndLoans == 2 ~ "2",
                                  df$NumberOfOpenCreditLinesAndLoans == 3 ~ "3",
                                  df$NumberOfOpenCreditLinesAndLoans == 4 ~ "4",
                                  df$NumberOfOpenCreditLinesAndLoans == 5 ~ "5",
                                  df$NumberOfOpenCreditLinesAndLoans < 11 ~ "6-10",
                                  df$NumberOfOpenCreditLinesAndLoans > 10 ~ "11+")

round(prop.table(table(df$CreditLinesBucket))*100,2)
round(prop.table(table(df$CreditLinesBucket, df$SeriousDlqin2yrs),2)*100,2)

##################################################################
# Open Real Estate Lines and Loans
##################################################################

do_summary(df, "NumberRealEstateLoansOrLines")
boxplot(df$NumberRealEstateLoansOrLines ~ df$SeriousDlqin2yrs, outline=FALSE)

# Outliers are 1.5x the IQR
ol <- find_outliers(df$NumberRealEstateLoansOrLines)[2]

# Anything above the outliers threshold - just set to the threshold
df$NumberRealEstateLoansOrLines <- ifelse(df$NumberRealEstateLoansOrLines > ol, 
                                          ol, 
                                          df$NumberRealEstateLoansOrLines)

# Visualize
boxplot(df$NumberRealEstateLoansOrLines ~ df$SeriousDlqin2yrs, outline=FALSE)
plot(density(df$NumberRealEstateLoansOrLines))
plot(density(subset(df, SeriousDlqin2yrs==0, select=c(NumberRealEstateLoansOrLines))[,1]))
lines(density(subset(df, SeriousDlqin2yrs==1, select=c(NumberRealEstateLoansOrLines))[,1]))

# Creating buckets
df$RealEstateBucket <- case_when(df$NumberRealEstateLoansOrLines == 0 ~ "0",
                                 df$NumberRealEstateLoansOrLines == 1 ~ "1",
                                 df$NumberRealEstateLoansOrLines == 2 ~ "2",
                                 df$NumberRealEstateLoansOrLines == 3 ~ "3",
                                 df$NumberRealEstateLoansOrLines == 4 ~ "4",
                                 df$NumberRealEstateLoansOrLines == 5 ~ "5")

round(prop.table(table(df$RealEstateBucket))*100,2)
round(prop.table(table(df$RealEstateBucket, df$SeriousDlqin2yrs),2)*100,2)

##################################################################
# Income
##################################################################

# There are a lot of NA incomes
do_summary(df, "MonthlyIncome")

# Outliers are 1.5x the IQR
ol <- find_outliers(subset(df, is.na(MonthlyIncome)==FALSE, select=c(MonthlyIncome))[,1])[2]

# Anything above the outliers threshold - just set to the threshold
df$MonthlyIncome <- ifelse(df$MonthlyIncome > ol, ol, df$MonthlyIncome)

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

# Create a training dataset
df.train <- subset(df, is.na(MonthlyIncome)==FALSE)
# Creating a dataset of only the observations with n/a incomes
df.na <- subset(df, is.na(MonthlyIncome))

# Doing some visualization
boxplot(df.train$MonthlyIncome ~ df.train$AgeBucket, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$PastDueBucket, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$PD90Bucket, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$SeriousDlqFlag, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$DependentsBucket, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$RealEstateBucket, outline=FALSE)
boxplot(df.train$MonthlyIncome ~ df.train$CreditLinesBucket, outline=FALSE)

# Formula to use for the regression model
f <- MonthlyIncome ~ age + DependentsBucket + SeriousDlqFlag + RealEstateBucket + CreditLinesBucket

# Repeat the steps below until a desired fit is acheived
#------------------------------------------------------------#
# Fit the linear model
fit <- lm(formula=f, data=df.train) 
summary(fit)

# Map the residuals values to the test dataset
df.train$resid <- resid(fit)

# Visualize the residuals
plot(density(df.train$resid))

# Find the value of 2x the standard deviation of the residuals
sd2 <- 2*sd(df.train$resid) 

# Flagging hte outliers - those obs whose residuals are more than 2x stdv
df.train$outs <- ifelse(abs(df.train$resid) > sd2, 1, 0)
table(df.train$outs)

# Removing the outliers
df.train <- subset(df.train, outs==0) 

# Remove the residuals
df.train <- subset(df.train, select=-c(outs, resid))
#------------------------------------------------------------#

# Save the modeled income model
save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/modeledincome.rdata")
#load(file="C://Users/g557428/Projects/seis736_ml_project/models/modeledincome.rdata")

# Make the predictions on the n/a only dataset
df.na$ModeledIncome <- predict(fit, df.na)

# If below 0, set to 0
df.na$ModeledIncome <- ifelse(df.na$ModeledIncome < 0, 0, df.na$ModeledIncome)

# Summarize the predictions
summary(df.na$ModeledIncome)

# Visualize modeled income compared to monthlyincome
plot(density(df.na$ModeledIncome), col="red")
lines(density(df.train$MonthlyIncome), col="blue")
legend("topright", legend=c("Modeled", "Actual"), col=c("red","blue"), lty=1)

# Make the predictions on the actual dataset
df$ModeledIncome <- predict(fit, df)
df$ModeledIncome <- ifelse(df$ModeledIncome < 0, 0, df$ModeledIncome)
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome), df$ModeledIncome, df$MonthlyIncome)

# Summarize the data
summary(df$MonthlyIncome)
summary(df$ModeledIncome)

# View the head and tail
head(subset(df, select=c(MonthlyIncome,ModeledIncome)), n=20)
tail(subset(df, select=c(MonthlyIncome,ModeledIncome)), n=20)

# Cleanup
rm(df.train, fit, sd2, df.na)

# Creating logical income buckets
df$IncomeBucket <- case_when(df$MonthlyIncome < 1000 ~ "<1000",
                             df$MonthlyIncome < 2000 ~ "1000-1999",
                             df$MonthlyIncome < 4000 ~ "2000-3999",
                             df$MonthlyIncome < 8000 ~ "4000-7999",
                             df$MonthlyIncome >= 8000 ~ "8000+")

# Visualize
boxplot(df$MonthlyIncome ~ df$IncomeBucket, outline=FALSE)

##################################################################
# Write to CSV
##################################################################

write.csv(x=df, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-modified.csv", row.names=FALSE)
