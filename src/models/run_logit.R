# glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
library("glmnet")

################################################################
# Data prep
################################################################

# Reading the file
df <- read.csv(file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-test-modified.csv")

# Scale the numeric features
df$RevolvingUtilizationOfUnsecuredLines <- as.vector(scale(df$RevolvingUtilizationOfUnsecuredLines))
df$age <- as.vector(scale(df$age))
df$NumberOfTime30.59DaysPastDueNotWorse <- as.vector(scale(df$NumberOfTime30.59DaysPastDueNotWorse))
df$DebtRatio <- as.vector(scale(df$DebtRatio))
df$MonthlyIncome <- as.vector(scale(df$MonthlyIncome))
df$NumberOfOpenCreditLinesAndLoans <- as.vector(scale(df$NumberOfOpenCreditLinesAndLoans))
df$NumberOfTimes90DaysLate <- as.vector(scale(df$NumberOfTimes90DaysLate))
df$NumberRealEstateLoansOrLines <- as.vector(scale(df$NumberRealEstateLoansOrLines))
df$NumberOfTime60.89DaysPastDueNotWorse <- as.vector(scale(df$NumberOfTime60.89DaysPastDueNotWorse))
df$NumberOfDependents <- as.vector(scale(df$NumberOfDependents))
df$TotalPastDue <- as.vector(scale(df$TotalPastDue))
df$RiskIndex <- as.vector(scale(df$RiskIndex))

# Create dummy variables for the non-numeric features
df$PD30.59Bucket <- as.factor(df$PD30.59Bucket)
df$PD60.89Bucket <- as.factor(df$PD60.89Bucket)
df$PD90Bucket <- as.factor(df$PD90Bucket)
df$PastDueBucket <- as.factor(df$PastDueBucket)
df$DependentsBucket <- as.factor(df$DependentsBucket)
df$IncomeBucket <- as.factor(df$IncomeBucket)
df$SeriousDlqFlag <- as.factor(df$SeriousDlqFlag)
df$AgeBucket <- as.factor(df$AgeBucket)
df$CreditLinesBucket <- as.factor(df$CreditLinesBucket)
df$RealEstateBucket <- as.factor(df$RealEstateBucket)

################################################################
# make predictions
################################################################

# Load the fitted model
load(file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")

f <- ~ RevolvingUtilizationOfUnsecuredLines +
       age +
       NumberOfTime30.59DaysPastDueNotWorse +
       DebtRatio +
       MonthlyIncome +
       NumberOfOpenCreditLinesAndLoans +
       NumberOfTimes90DaysLate +
       NumberRealEstateLoansOrLines +
       NumberOfTime60.89DaysPastDueNotWorse +
       NumberOfDependents +
       PD30.59Bucket +
       PD60.89Bucket +
       PD90Bucket +
       TotalPastDue +
       PastDueBucket +
       DependentsBucket +
       IncomeBucket +
       RiskIndex +
       SeriousDlqFlag +
       AgeBucket +
       CreditLinesBucket +
       RealEstateBucket

x <- model.matrix(f, data=df)

# Make predictions
pred <- predict(fit, newx=x, type="response", s=c(fit$lambda.min, fit$lambda.1se))

# Final output
out <- data.frame(pred[,1])
colnames(out)[1] <- "Probability"

write.csv(x=out, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-final-2.csv")

