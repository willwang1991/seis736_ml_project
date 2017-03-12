#----------------------------------------------------------------------------#
# Data exploration & visualization                                           #
# SEIS763-02 Machine Learning                                                #
#----------------------------------------------------------------------------#

# Using the dplyr library, which is a popular package for data analysis
library("dplyr")
library("lazyeval")

# Read the CSV file into a dataframe
df <- read.csv(file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/raw/cs-training.csv")

# Get the dimensions of the dataframe - rows x cols
dim(df)

# Distribution of the dependent variable
table(df$SeriousDlqin2yrs)

# Change dependent variable to a factor
df$SeriousDlqin2yrs <- as.factor(df$SeriousDlqin2yrs)

#----------------------------------------------------------------------------#
# Summarize the attributes                                                   #
#----------------------------------------------------------------------------#

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

do_summary(df, 'RevolvingUtilizationOfUnsecuredLines')
do_summary(df, 'age')
do_summary(df, 'NumberOfTime30.59DaysPastDueNotWorse')
do_summary(df, 'DebtRatio')
do_summary(df, 'MonthlyIncome')
do_summary(df, 'NumberOfOpenCreditLinesAndLoans')
do_summary(df, 'NumberOfTimes90DaysLate')
do_summary(df, 'NumberRealEstateLoansOrLines')
do_summary(df, 'NumberOfTime60.89DaysPastDueNotWorse')
do_summary(df, 'NumberOfDependents')

# Data viz
boxplot(df$RevolvingUtilizationOfUnsecuredLines)
boxplot(df$MonthlyIncome ~ df$SeriousDlqin2yrs)
