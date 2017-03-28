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

#df.sub <- subset(df, NumberOfTime30.59DaysPastDueNotWorse != -1)

# Do kmeans clustering on the combined features
#km1 <- kmeans(x=df.sub[,4], 
#              centers=3,
#              iter.max=50,
#              nstart=25)

# Look at the distributions
#table(km1$cluster, df.sub$SeriousDlqin2yrs)
#prop.table(table(km1$cluster, df.sub$SeriousDlqin2yrs), 2)*100

# Convert the clusters to dataframe and merge with the existing dataframe
#df.sub <- cbind(df.sub, data.frame(km1$cluster))
#colnames(df.sub)[12] <- "Grp30.59DaysDlq"

#boxplot(df.sub$NumberOfTime30.59DaysPastDueNotWorse ~ df.sub$Grp30.59DaysDlq)

#df.sub %>% group_by(Grp30.59DaysDlq) %>% summarize(min=min(NumberOfTime30.59DaysPastDueNotWorse),
#                                                   max=max(NumberOfTime30.59DaysPastDueNotWorse),
#                                                   median=median(NumberOfTime30.59DaysPastDueNotWorse),
#                                                   mean=mean(NumberOfTime30.59DaysPastDueNotWorse),
#                                                   n=n(),
#                                                   na=sum(is.na(NumberOfTime30.59DaysPastDueNotWorse)))

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD30.59Bucket <- case_when(df$NumberOfTime30.59DaysPastDueNotWorse < 0 ~ "UN",
                              df$NumberOfTime30.59DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime30.59DaysPastDueNotWorse <= 3 ~ "1-3",
                              df$NumberOfTime30.59DaysPastDueNotWorse > 3 ~ "4+"
                              )

df$PD30.59Bucket <- as.factor(df$PD30.59Bucket)

table(df$PD30.59Bucket)

#rm(df.sub, km1)

##################################################################
# Num Times 60-89 days past due but not worse w/in 2 years
##################################################################

do_summary(df, "NumberOfTime60.89DaysPastDueNotWorse")
do_summary(subset(df, NumberOfTime60.89DaysPastDueNotWorse<96), "NumberOfTime60.89DaysPastDueNotWorse")

# Counts in the 90s are likely "NA" values. Let's create a single NA value
df$NumberOfTime60.89DaysPastDueNotWorse <- ifelse(df$NumberOfTime60.89DaysPastDueNotWorse > 90, 
                                                  -1, 
                                                  df$NumberOfTime60.89DaysPastDueNotWorse)

#df.sub <- subset(df, NumberOfTime60.89DaysPastDueNotWorse != -1)

# Do kmeans clustering on the combined features
#km1 <- kmeans(x=df.sub[,10], 
#              centers=3,
#              iter.max=50,
#              nstart=25)

# Convert the clusters to dataframe and merge with the existing dataframe
#df.sub <- cbind(df.sub, data.frame(km1$cluster))
#colnames(df.sub)[13] <- "Grp60.89DaysDlq"

#boxplot(df.sub$NumberOfTime60.89DaysPastDueNotWorse ~ df.sub$Grp60.89DaysDlq)

#df.sub %>% group_by(Grp60.89DaysDlq) %>% summarize(min=min(NumberOfTime60.89DaysPastDueNotWorse),
#                                                   max=max(NumberOfTime60.89DaysPastDueNotWorse),
#                                                   median=median(NumberOfTime60.89DaysPastDueNotWorse),
#                                                   mean=mean(NumberOfTime60.89DaysPastDueNotWorse),
#                                                   n=n(),
#                                                   na=sum(is.na(NumberOfTime60.89DaysPastDueNotWorse)))

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD60.89Bucket <- case_when(df$NumberOfTime60.89DaysPastDueNotWorse < 0 ~ "UN",
                              df$NumberOfTime60.89DaysPastDueNotWorse == 0 ~ "0",
                              df$NumberOfTime60.89DaysPastDueNotWorse <= 3 ~ "1-3",
                              df$NumberOfTime60.89DaysPastDueNotWorse > 3 ~ "4+")

df$PD60.89Bucket <- as.factor(df$PD60.89Bucket)

prop.table(table(df$PD60.89Bucket, df$SeriousDlqin2yrs),2)

#rm(df.sub, km1)

##################################################################
# Num Times 90+ days past due
##################################################################

do_summary(df, "NumberOfTimes90DaysLate")
do_summary(subset(df, NumberOfTimes90DaysLate<96), "NumberOfTimes90DaysLate")

# Counts in the 90s are likely "NA" values. Let's create a single NA value
df$NumberOfTimes90DaysLate <- ifelse(df$NumberOfTimes90DaysLate > 90, 
                                     -1, 
                                     df$NumberOfTimes90DaysLate)

#df.sub <- subset(df, NumberOfTimes90DaysLate != -1)

# Do kmeans clustering on the combined features
#km1 <- kmeans(x=df.sub[,8], 
#              centers=3,
#              iter.max=50,
#              nstart=25)

# Convert the clusters to dataframe and merge with the existing dataframe
#df.sub <- cbind(df.sub, data.frame(km1$cluster))
#colnames(df.sub)[14] <- "Grp90DaysDlq"

#boxplot(df.sub$NumberOfTimes90DaysLate ~ df.sub$Grp90DaysDlq, outline=FALSE)

#df.sub %>% group_by(Grp90DaysDlq) %>% summarize(min=min(NumberOfTimes90DaysLate),
#                                                max=max(NumberOfTimes90DaysLate),
#                                                median=median(NumberOfTimes90DaysLate),
#                                                mean=mean(NumberOfTimes90DaysLate),
#                                                n=n(),
#                                                na=sum(is.na(NumberOfTimes90DaysLate)))

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PD90Bucket <- case_when(df$NumberOfTimes90DaysLate < 0 ~ "UN",
                           df$NumberOfTimes90DaysLate == 0 ~ "0",
                           df$NumberOfTimes90DaysLate <= 3 ~ "1-3",
                           df$NumberOfTimes90DaysLate > 3 ~ "4+")

df$PD90Bucket <- as.factor(df$PD90Bucket)

#rm(df.sub, km1)

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

# Do kmeans clustering on the combined features
#km1 <- kmeans(x=df[,15], 
#              centers=4,
#              iter.max=50,
#              nstart=25)

# Look at the distributions
#table(km1$cluster, df$SeriousDlqin2yrs)
#prop.table(table(km1$cluster, df$SeriousDlqin2yrs), 2)*100

#summary(km1$cluster)

# Convert the clusters to dataframe and merge with the existing dataframe
#df <- cbind(df, data.frame(km1$cluster))
#colnames(df)[16] <- "PastDueBucket"

# Initial plotting of past due groups shows there is some potentially bad data. 
# It is a small portion of the population so go ahead and remove these
#boxplot(df$TotalPastDue ~ df$PastDueBucket)


#df %>% group_by(PastDueBucket) %>% summarize(min=min(TotalPastDue),
#                                             max=max(TotalPastDue),
#                                             median=median(TotalPastDue),
#                                             mean=mean(TotalPastDue),
#                                             n=n(),
#                                             na=sum(is.na(TotalPastDue)))

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$PastDueBucket <- case_when(df$TotalPastDue == 0 ~ "0",
                              df$TotalPastDue <= 3 ~ "1-3",
                              df$TotalPastDue <= 7 ~ "4-7",
                              df$TotalPastDue > 7 ~ "8+")

# Converting to dummy variable
df$PastDueBucket <- as.factor(df$PastDueBucket)

# View the distributions
prop.table(table(df$PastDueBucket, df$SeriousDlqin2yrs),2)


##################################################################
# Number of Dependents
##################################################################

do_summary(df, "NumberOfDependents")
summary(df$NumberOfDependents)

# For now lets just use the median value
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents), 0, df$NumberOfDependents)

# Do kmeans clustering on the combined features
#km1 <- kmeans(x=df[,11], 
#              centers=3,
#              iter.max=50,
#              nstart=25)

# Look at the distributions
#table(km1$cluster, df$SeriousDlqin2yrs)
#prop.table(table(km1$cluster, df$SeriousDlqin2yrs), 2)*100

# Convert the clusters to dataframe and merge with the existing dataframe
#df <- cbind(df, data.frame(km1$cluster))
#colnames(df)[17] <- "DependentsBucket"

#boxplot(df$NumberOfDependents ~ df$DependentsBucket, outline=FALSE)

#df %>% group_by(DependentsBucket) %>% summarize(min=min(NumberOfDependents),
#                                            max=max(NumberOfDependents),
#                                            median=median(NumberOfDependents),
#                                            mean=mean(NumberOfDependents),
#                                            n=n(),
#                                            na=sum(is.na(NumberOfDependents)))

# Creating the buckets based on the kmeans clustering
# These are the cutoffs we'll use for future data as well
df$DependentsBucket <- case_when(df$NumberOfDependents == 0 ~ "0",
                                 df$NumberOfDependents == 1 ~ "1",
                                 df$NumberOfDependents == 2 ~ "2-3",
                                 df$NumberOfDependents == 3 ~ "2-3",
                                 df$NumberOfDependents > 3 ~ "4+")

df$DependentsBucket <- as.factor(df$DependentsBucket)


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

df$IncomeBucket <- as.factor(df$IncomeBucket)

boxplot(df$MonthlyIncome ~ df$IncomeBucket, outline=FALSE)

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

df$SeriousDlqFlag <- as.factor(df$SeriousDlqFlag)

table(df$SeriousDlqFlag, df$SeriousDlqin2yrs)
prop.table(table(df$SeriousDlqFlag, df$SeriousDlqin2yrs),2)

##################################################################
# Risk Index = Utilization by Debt Ratio + weighted delinquency
##################################################################

df$RiskIndex <- (df$DebtRatio * df$RevolvingUtilizationOfUnsecuredLines) + (df$TotalPastDue/2)

summary(df$RiskIndex)
do_summary(df, "RiskIndex")

boxplot(df$RiskIndex ~ df$SeriousDlqin2yrs, outline=FALSE)

boxplot(df$RevolvingUtilizationOfUnsecuredLines ~ df$SeriousDlqin2yrs, outline=FALSE)
boxplot(df$DebtRatio ~ df$SeriousDlqin2yrs, outline=FALSE)
boxplot(df$TotalPastDue ~ df$SeriousDlqin2yrs, outline=FALSE)


##################################################################
# Age Buckets
##################################################################

df$AgeBucket <- case_when(df$age < 18 ~ "0-17",
                          df$age < 22 ~ "18-21",
                          df$age < 36 ~ "22-35",
                          df$age < 56 ~ "36-55",
                          df$age < 76 ~ "56-75",
                          df$age >= 76 ~ "76+")

df$AgeBucket <- as.factor(df$AgeBucket)

table(df$AgeBucket)
table(df$AgeBucket, df$SeriousDlqin2yrs)
round(prop.table(table(df$AgeBucket, df$SeriousDlqin2yrs), 2)*100,2)

##################################################################
# Open Credit Lines and Loans
##################################################################

do_summary(df, "NumberOfOpenCreditLinesAndLoans")
boxplot(df$NumberOfOpenCreditLinesAndLoans ~ df$SeriousDlqin2yrs, outline=FALSE)

df$CreditLinesBucket <- case_when(df$NumberOfOpenCreditLinesAndLoans <= 0 ~ "0",
                                  df$NumberOfOpenCreditLinesAndLoans < 5 ~ "1-4",
                                  df$NumberOfOpenCreditLinesAndLoans < 9 ~ "5-8",
                                  df$NumberOfOpenCreditLinesAndLoans >= 9 ~ "9+")

df$CreditLinesBucket <- as.factor(df$CreditLinesBucket)

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

df$RealEstateBucket <- as.factor(df$RealEstateBucket)

round(prop.table(table(df$RealEstateBucket))*100,2)
round(prop.table(table(df$RealEstateBucket, df$SeriousDlqin2yrs),2)*100,2)

##################################################################
# Write to CSV
##################################################################

write.csv(x=df, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-modified.csv", row.names=FALSE)
