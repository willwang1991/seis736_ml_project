
library("dplyr")
library("lazyeval")

# Read the file
df <- read.csv(file="C://Users/g557428/Projects/seis736_ml_project/data/raw/cs-test.csv")

df <- df[, -c(1:2)]

# Perform summarization on the dataframe - groupbed by the dependent variable
do_summary <- function(df, varname) {
  df %>% summarize_(obs = "n()",
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

prop.table(table(df$PD60.89Bucket))

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
prop.table(table(df$PastDueBucket))

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
# Income
##################################################################

do_summary(df, "MonthlyIncome")

# Get median income value
med <- median(subset(df, is.na(MonthlyIncome)==FALSE, select=c(MonthlyIncome))[,1])

# Replace N/As with the median income value
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome), med, df$MonthlyIncome)

# Creating logical income buckets
df$IncomeBucket <- case_when(df$MonthlyIncome < 1000 ~ "<1000",
                             df$MonthlyIncome < 2000 ~ "1000-1999",
                             df$MonthlyIncome < 4000 ~ "2000-3999",
                             df$MonthlyIncome < 8000 ~ "4000-7999",
                             df$MonthlyIncome < 16000 ~ "8000-15999",
                             df$MonthlyIncome < 32000 ~ "16000-31999",
                             df$MonthlyIncome < 64000 ~ "32000-63999",
                             df$MonthlyIncome >= 64000 ~ "64000+")

boxplot(df$MonthlyIncome ~ df$IncomeBucket, outline=FALSE)

##################################################################
# Debt Ratio
##################################################################

# Theoretically this is a percentage, but there are some non-percentage looking values here
do_summary(df, "DebtRatio")
summary(df$DebtRatio)

# Cap it at 100%
df$DebtRatio <- ifelse(df$DebtRatio > 1.0, 1.0, df$DebtRatio)

##################################################################
# Revolving Utilization
##################################################################

# This is supposed to be a percentage, but there are some outrageous values
do_summary(df, "RevolvingUtilizationOfUnsecuredLines")

# Cap it at 100%
df$RevolvingUtilizationOfUnsecuredLines <- ifelse(df$RevolvingUtilizationOfUnsecuredLines > 1.0, 
                                                  1.0, 
                                                  df$RevolvingUtilizationOfUnsecuredLines)

##################################################################
# Serious Delinquency Ever
##################################################################

# Did the customer ever have a serious delinquency - 90 or more days delinquent
df$SeriousDlqFlag <- ifelse(df$NumberOfTimes90DaysLate > 0, 1, 0)

##################################################################
# Risk Index = Utilization by Debt Ratio + weighted delinquency
##################################################################

df$RiskIndex <- (df$DebtRatio * df$RevolvingUtilizationOfUnsecuredLines) + (df$TotalPastDue/2)

summary(df$RiskIndex)
do_summary(df, "RiskIndex")

##################################################################
# Age Buckets
##################################################################

do_summary(df, "age")

df$AgeBucket <- case_when(df$age < 18 ~ "0-17",
                          df$age < 22 ~ "18-21",
                          df$age < 36 ~ "22-35",
                          df$age < 56 ~ "36-55",
                          df$age < 76 ~ "56-75",
                          df$age >= 76 ~ "76+")

table(df$AgeBucket)

##################################################################
# Open Credit Lines and Loans
##################################################################

do_summary(df, "NumberOfOpenCreditLinesAndLoans")

df$CreditLinesBucket <- case_when(df$NumberOfOpenCreditLinesAndLoans <= 0 ~ "0",
                                  df$NumberOfOpenCreditLinesAndLoans < 5 ~ "1-4",
                                  df$NumberOfOpenCreditLinesAndLoans < 9 ~ "5-8",
                                  df$NumberOfOpenCreditLinesAndLoans >= 9 ~ "9+")

round(prop.table(table(df$CreditLinesBucket))*100,2)

##################################################################
# Open Real Estate Lines and Loans
##################################################################

do_summary(df, "NumberRealEstateLoansOrLines")

df$RealEstateBucket <- case_when(df$NumberRealEstateLoansOrLines <= 0 ~ "0",
                                 df$NumberRealEstateLoansOrLines < 2 ~ "1",
                                 df$NumberRealEstateLoansOrLines < 3 ~ "2",
                                 df$NumberRealEstateLoansOrLines < 6 ~ "3-5",
                                 df$NumberRealEstateLoansOrLines >= 6 ~ "6+")

round(prop.table(table(df$RealEstateBucket))*100,2)

##################################################################
# Write to CSV
##################################################################

write.csv(x=df, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-test-modified.csv", row.names=FALSE)