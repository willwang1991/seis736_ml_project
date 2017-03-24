######################################################
# Preprocessing take 3 
# This time use the median values to fill n/as
# But similar to the first time, bucket the counter 
# variables
######################################################

library("dplyr")
library("lazyeval")

# Read the csv into a dataframe
df <- read.csv(file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/raw/cs-training.csv")

# Remove the row counter
df <- df[,-1]

# Splits of the target attribute
table(df$SeriousDlqin2yrs)

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




#----------------------------------------------------------------------------#
#                         NumberOfDependents                                 #
#----------------------------------------------------------------------------#



write.csv(x=df, file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/processed/cs-modified.csv", 
          row.names=FALSE)