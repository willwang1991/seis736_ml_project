######################################################
# Preprocessing take 2 
# This time use the median values to fill n/as
# In addition, scale all of the features rather 
# than grouping them into factors
######################################################

# Read the csv file
df <- read.csv(file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/raw/cs-training.csv")

# Remove the row counter
df <- df[,-1]

######################################################
# Fill N/As with the median values
# Fields with N/As include:
#   NumberOfDependents
#   MonthlyIncome
######################################################

# Number of depedents
med <- median(subset(df, is.na(NumberOfDependents)==FALSE, select=c(NumberOfDependents))[,1])
df$NumberOfDependents <- ifelse(is.na(df$NumberOfDependents), med, df$NumberOfDependents)

# MonthlyIncome
med <- median(subset(df, is.na(MonthlyIncome)==FALSE, select=c(MonthlyIncome))[,1])
df$MonthlyIncome <- ifelse(is.na(df$MonthlyIncome), med, df$MonthlyIncome)

######################################################
# Scale all of the features
######################################################

# Remove the target attribute
df.target <- subset(df, select=c(SeriousDlqin2yrs))

# Scale the remaining features
df <- subset(df, select=c(-SeriousDlqin2yrs))
df.scaled <- data.frame(scale(df))

# Merge the scaled features back with the target attribute
df <- cbind(df.scaled, df.target)

rm(df.scaled, df.target)

write.csv(x=df, file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/processed/cs-modified-2.csv", 
          row.names=FALSE)