# Read the csv into a dataframe
df <- read.csv(file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/raw/cs-training.csv")

# Remove the row counter
df <- df[,-1]

# Splits of the target attribute
table(df$SeriousDlqin2yrs)

# Target attribute as factor
df$SeriousDlqin2yrs <- as.factor(df$SeriousDlqin2yrs)

library("dplyr")
library("lazyeval")

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

#----------------------------------------------------------------------------#
#                   RevolvingUtilizationOfUnsecuredLines                     #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "RevolvingUtilizationOfUnsecuredLines")

# Go ahead and standarize using the z-score
df$RevolvingUtilizationOfUnsecuredLines <- as.vector(scale(df$RevolvingUtilizationOfUnsecuredLines, 
                                                           center=TRUE, scale=TRUE))

# This potentially is a good predictor. It appears there is good separation
boxplot(df$RevolvingUtilizationOfUnsecuredLines ~ df$SeriousDlqin2yrs, outline=FALSE)


#----------------------------------------------------------------------------#
#                                 Age                                        #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "age")

# Go ahead and standarize using the z-score
df$age <- as.vector(scale(df$age, center=TRUE, scale=TRUE))

# There is decent separation - this may be an okay predictor
boxplot(df$age ~ df$SeriousDlqin2yrs, outline=FALSE)


#----------------------------------------------------------------------------#
#                               DebtRatio                                    #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "DebtRatio")

# Go ahead and standarize using the z-score
df$DebtRatio <- as.vector(scale(df$DebtRatio, center=TRUE, scale=TRUE))

# Likely not a great predictor since there really is no difference b/w the two groups
boxplot(df$DebtRatio ~ df$SeriousDlqin2yrs, outline=FALSE)

#----------------------------------------------------------------------------#
#                 NumberOfTime30.59DaysPastDueNotWorse                       #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "NumberOfTime30.59DaysPastDueNotWorse")

df$NumberOfTime30.59DaysPastDueNotWorse <- case_when(
  df$NumberOfTime30.59DaysPastDueNotWorse == 0 ~ 0,
  df$NumberOfTime30.59DaysPastDueNotWorse == 1 ~ 1,
  df$NumberOfTime30.59DaysPastDueNotWorse == 2 ~ 2,
  df$NumberOfTime30.59DaysPastDueNotWorse == 3 ~ 3,
  df$NumberOfTime30.59DaysPastDueNotWorse == 4 ~ 4,
  df$NumberOfTime30.59DaysPastDueNotWorse >= 5 ~ 5)

df$NumberOfTime30.59DaysPastDueNotWorse <- as.factor(df$NumberOfTime30.59DaysPastDueNotWorse)

table(df$SeriousDlqin2yrs, df$NumberOfTime30.59DaysPastDueNotWorse)

#----------------------------------------------------------------------------#
#                     NumberOfOpenCreditLinesAndLoans                        #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "NumberOfOpenCreditLinesAndLoans")

# Likely not a great predictor
boxplot(df$NumberOfOpenCreditLinesAndLoans ~ df$SeriousDlqin2yrs, outline=FALSE)

df$NumberOfOpenCreditLinesAndLoans <- case_when(
  df$NumberOfOpenCreditLinesAndLoans <= 5 ~ "0-5",
  df$NumberOfOpenCreditLinesAndLoans == 6 ~ "6",
  df$NumberOfOpenCreditLinesAndLoans == 7 ~ "7",
  df$NumberOfOpenCreditLinesAndLoans == 8 ~ "8",
  df$NumberOfOpenCreditLinesAndLoans == 9 ~ "9",
  df$NumberOfOpenCreditLinesAndLoans == 10 ~ "10",
  df$NumberOfOpenCreditLinesAndLoans >= 11 ~ "11+")

df$NumberOfOpenCreditLinesAndLoans <- as.factor(df$NumberOfOpenCreditLinesAndLoans)

table(df$SeriousDlqin2yrs, df$NumberOfOpenCreditLinesAndLoans)

#----------------------------------------------------------------------------#
#                     NumberOfTimes90DaysLate                                #
#----------------------------------------------------------------------------#

# Summarize - There aren't any N/As
do_summary(df, "NumberOfTimes90DaysLate")

# Likely a good predictor. But let's change this into a factor
boxplot(df$NumberOfTimes90DaysLate ~ df$SeriousDlqin2yrs, outline=FALSE)

df$NumberOfTimes90DaysLate <- case_when(
  df$NumberOfTimes90DaysLate == 0 ~ 0,
  df$NumberOfTimes90DaysLate == 1 ~ 1,
  df$NumberOfTimes90DaysLate == 2 ~ 2,
  df$NumberOfTimes90DaysLate == 3 ~ 3,
  df$NumberOfTimes90DaysLate == 4 ~ 4,
  df$NumberOfTimes90DaysLate >= 5 ~ 5)

df$NumberOfTimes90DaysLate <- as.factor(df$NumberOfTimes90DaysLate)

table(df$SeriousDlqin2yrs, df$NumberOfTimes90DaysLate)

#----------------------------------------------------------------------------#
#                   NumberRealEstateLoansOrLines                             #
#----------------------------------------------------------------------------#

do_summary(df, "NumberRealEstateLoansOrLines")

# Doesn't look promising
boxplot(df$NumberRealEstateLoansOrLines ~ df$SeriousDlqin2yrs, outline=FALSE)

df$NumberRealEstateLoansOrLines <- case_when(
  df$NumberRealEstateLoansOrLines == 0 ~ 0,
  df$NumberRealEstateLoansOrLines == 1 ~ 1,
  df$NumberRealEstateLoansOrLines == 2 ~ 2,
  df$NumberRealEstateLoansOrLines == 3 ~ 3,
  df$NumberRealEstateLoansOrLines == 4 ~ 4,
  df$NumberRealEstateLoansOrLines >= 5 ~ 5)

df$NumberRealEstateLoansOrLines <- as.factor(df$NumberRealEstateLoansOrLines)

table(df$NumberRealEstateLoansOrLines, df$SeriousDlqin2yrs)

#----------------------------------------------------------------------------#
#               NumberOfTime60.89DaysPastDueNotWorse                         #
#----------------------------------------------------------------------------#

do_summary(df, "NumberOfTime60.89DaysPastDueNotWorse")
boxplot(df$NumberOfTime60.89DaysPastDueNotWorse ~ df$SeriousDlqin2yrs, outline=FALSE)

df$NumberOfTime60.89DaysPastDueNotWorse <- case_when(
  df$NumberOfTime60.89DaysPastDueNotWorse == 0 ~ 0,
  df$NumberOfTime60.89DaysPastDueNotWorse == 1 ~ 1,
  df$NumberOfTime60.89DaysPastDueNotWorse == 2 ~ 2,
  df$NumberOfTime60.89DaysPastDueNotWorse == 3 ~ 3,
  df$NumberOfTime60.89DaysPastDueNotWorse == 4 ~ 4,
  df$NumberOfTime60.89DaysPastDueNotWorse >= 5 ~ 5)

df$NumberOfTime60.89DaysPastDueNotWorse <- as.factor(df$NumberOfTime60.89DaysPastDueNotWorse)

# The % share b/w groups is different, but raw counts indicate this feature may not be so helpful
table(df$NumberOfTime60.89DaysPastDueNotWorse, df$SeriousDlqin2yrs)

#----------------------------------------------------------------------------#
#                             MonthlyIncome                                  #
#----------------------------------------------------------------------------#

# Summarize - There are a bunch of NA values. Need to replace these with something
do_summary(df, "MonthlyIncome")

# Let's try to predict income using a stratification approach - first look at the 1s
df.income1 <- df %>% filter(is.na(MonthlyIncome) == FALSE & SeriousDlqin2yrs == "1")

lm_resid <- function(df, formula, iter) {
  for (i in 1:iter) {
    fit <- lm(formula=formula, data=df) # Fit the model
    print(summary(fit))
    df$resid <- resid(fit) # Map the residuals
    sd2 <- 2*sd(df$resid) # 2x the stdev of the residuals
    df$outs <- ifelse(abs(df$resid) > sd2, 1, 0) # Flagging those outliers
    df <- subset(df, outs==0) # Removing the outliers
  }
  return(df)
}

f <- MonthlyIncome ~ 
  RevolvingUtilizationOfUnsecuredLines + 
  age +
  NumberOfTime30.59DaysPastDueNotWorse +
  DebtRatio +
  NumberOfOpenCreditLinesAndLoans +
  NumberOfTimes90DaysLate +
  NumberRealEstateLoansOrLines + 
  NumberOfTime60.89DaysPastDueNotWorse

df.income1 <- lm_resid(df.income1, f, 5)

fit <- lm(formula=f, data=df.income1)

summary(fit)
plot(fit$residuals)

# Make the predictions
df$PredIncome1 <- predict(fit, df)
df$PredIncome1 <- ifelse(df$PredIncome1 < 0, 0, df$PredIncome1)

# Replace monthly income with the new predicted values
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome) & df$SeriousDlqin2yrs == "1", 
                           df$PredIncome1, df$MonthlyIncome)

# Remove the predicted monthly income
df <- df[,-12]
rm(df.income1)

# Let's try to predict income using a stratification approach - now look at the 0s
df.income0 <- df %>% filter(is.na(MonthlyIncome) == FALSE & SeriousDlqin2yrs == "0")
df.income0 <- lm_resid(df.income0, f, 5)
fit <- lm(formula=f, data=df.income0)
summary(fit)

# Make the predictions
df$PredIncome0 <- predict(fit, df)
df$PredIncome0 <- ifelse(df$PredIncome0 < 0, 0, df$PredIncome0)

# Replace monthly income with the new predicted values
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome) & df$SeriousDlqin2yrs == "0", 
                           df$PredIncome0, df$MonthlyIncome)

# Remove the predicted monthly income
df <- df[,-12]
rm(df.income0)


#----------------------------------------------------------------------------#
#                       NumberOfDependents**                                 #
#----------------------------------------------------------------------------#

# Lots of NA values - Need to handle
do_summary(df, "NumberOfDependents")

f <- NumberOfDependents ~ 
  MonthlyIncome +
  RevolvingUtilizationOfUnsecuredLines + 
  age +
  NumberOfTime30.59DaysPastDueNotWorse +
  DebtRatio +
  NumberOfOpenCreditLinesAndLoans +
  NumberOfTimes90DaysLate +
  NumberRealEstateLoansOrLines + 
  NumberOfTime60.89DaysPastDueNotWorse

df.d1 <- df %>% filter(is.na(NumberOfDependents) == FALSE, SeriousDlqin2yrs == "1")
df.d1 <- lm_resid(df.d1, f, 5)
fit <- lm(formula=f, data=df.d1)
summary(fit)

# Make the predictions
df$PredDep1 <- predict(fit, df)
df$PredDep1 <- ifelse(df$PredDep1 < 0, 0, df$PredDep1)

# Replace with the new predicted values
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents) & df$SeriousDlqin2yrs == "1", 
                           df$PredDep1, df$NumberOfDependents)

# Remove the predicted monthly income
df <- df[,-12]
rm(df.d1)

df.d1 <- df %>% filter(is.na(NumberOfDependents) == FALSE, SeriousDlqin2yrs == "0")
df.d1 <- lm_resid(df.d1, f, 3)
fit <- lm(formula=f, data=df.d1)
summary(fit)

# Make the predictions
df$PredDep1 <- predict(fit, df)
df$PredDep1 <- ifelse(df$PredDep1 < 0, 0, df$PredDep1)

# Replace with the new predicted values
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents) & df$SeriousDlqin2yrs == "0", 
                                df$PredDep1, df$NumberOfDependents)

# Remove the predicted monthly income
df <- df[,-12]
rm(df.d1)

df$NumberOfDependents <- round(df$NumberOfDependents)

boxplot(df$NumberOfDependents ~ df$SeriousDlqin2yrs, outline=FALSE)

df$NumberOfDependents <- case_when(
  df$NumberOfDependents == 0 ~ 0,
  df$NumberOfDependents == 1 ~ 1,
  df$NumberOfDependents == 2 ~ 2,
  df$NumberOfDependents == 3 ~ 3,
  df$NumberOfDependents == 4 ~ 4,
  df$NumberOfDependents >= 5 ~ 5)

df$NumberOfDependents <- as.factor(df$NumberOfDependents)

prop.table(table(df$NumberOfDependents, df$SeriousDlqin2yrs),2)

write.csv(x=df, file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/processed/cs-modified.csv", 
          row.names=FALSE)