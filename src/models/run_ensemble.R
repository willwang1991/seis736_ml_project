################################################################
# Load required packages
################################################################

library("glmnet")
library("e1071")

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
df$ModeledIncome <- as.vector(scale(df$ModeledIncome))
df$NumberOfOpenCreditLinesAndLoans <- as.vector(scale(df$NumberOfOpenCreditLinesAndLoans))
df$NumberOfTimes90DaysLate <- as.vector(scale(df$NumberOfTimes90DaysLate))
df$NumberRealEstateLoansOrLines <- as.vector(scale(df$NumberRealEstateLoansOrLines))
df$NumberOfTime60.89DaysPastDueNotWorse <- as.vector(scale(df$NumberOfTime60.89DaysPastDueNotWorse))
df$NumberOfDependents <- as.vector(scale(df$NumberOfDependents))
df$TotalPastDue <- as.vector(scale(df$TotalPastDue))
df$RiskIndex <- as.vector(scale(df$RiskIndex))
df$SeriousDlqRatio <- as.vector(scale(df$SeriousDlqRatio))

#---Create dummy variables for the non-numeric features---#

# Past Due 30-59 bucket
df$PD30.59Bucket1 <- ifelse(df$PD30.59Bucket=="1",1,0)
df$PD30.59Bucket2 <- ifelse(df$PD30.59Bucket=="2",1,0)
df$PD30.59Bucket3 <- ifelse(df$PD30.59Bucket=="3",1,0)
df$PD30.59Bucket4 <- ifelse(df$PD30.59Bucket=="4+",1,0)
df <- subset(df, select=-c(PD30.59Bucket))
# Past Due 60-89 bucket
df$PD60.89Bucket1 <- ifelse(df$PD60.89Bucket=="1",1,0)
df$PD60.89Bucket2 <- ifelse(df$PD60.89Bucket=="2",1,0)
df$PD60.89Bucket3 <- ifelse(df$PD60.89Bucket=="3",1,0)
df <- subset(df, select=-c(PD60.89Bucket))
# Past Due 90+ bucket
df$PD90Bucket1 <- ifelse(df$PD90Bucket=="1",1,0)
df$PD90Bucket2 <- ifelse(df$PD90Bucket=="2",1,0)
df$PD90Bucket3 <- ifelse(df$PD90Bucket=="3",1,0)
df$PD90Bucket4 <- ifelse(df$PD90Bucket=="4",1,0)
df$PD90Bucket5 <- ifelse(df$PD90Bucket=="5+",1,0)
df <- subset(df, select=-c(PD90Bucket))
# Total Past Due bucket
df$PastDueBucket1 <- ifelse(df$PastDueBucket=="1",1,0)
df$PastDueBucket2 <- ifelse(df$PastDueBucket=="2",1,0)
df$PastDueBucket3 <- ifelse(df$PastDueBucket=="3",1,0)
df$PastDueBucket4 <- ifelse(df$PastDueBucket=="4",1,0)
df$PastDueBucket5 <- ifelse(df$PastDueBucket=="5",1,0)
df$PastDueBucket6 <- ifelse(df$PastDueBucket=="6",1,0)
df$PastDueBucket7 <- ifelse(df$PastDueBucket=="7",1,0)
df$PastDueBucket8 <- ifelse(df$PastDueBucket=="8+",1,0)
df <- subset(df, select=-c(PastDueBucket))
# Dependents bucket
df$DependentsBucket1 <- ifelse(df$DependentsBucket=="1",1,0)
df$DependentsBucket2 <- ifelse(df$DependentsBucket=="2",1,0)
df$DependentsBucket3 <- ifelse(df$DependentsBucket=="3",1,0)
df$DependentsBucket4 <- ifelse(df$DependentsBucket=="4",1,0)
df$DependentsBucket5 <- ifelse(df$DependentsBucket=="5+",1,0)
df <- subset(df, select=-c(DependentsBucket))
# Income bucket
df$IncomeBucket1000.1999 <- ifelse(df$IncomeBucket=="1000-1999",1,0)
df$IncomeBucket2000.3999 <- ifelse(df$IncomeBucket=="2000-3999",1,0)
df$IncomeBucket4000.7999 <- ifelse(df$IncomeBucket=="4000-7999",1,0)
df$IncomeBucket8000 <- ifelse(df$IncomeBucket=="8000+",1,0)
df <- subset(df, select=-c(IncomeBucket))
# Age bucket
df$AgeBucket22.35 <- ifelse(df$AgeBucket=="22-35",1,0)
df$AgeBucket36.55 <- ifelse(df$AgeBucket=="36-55",1,0)
df$AgeBucket56.75 <- ifelse(df$AgeBucket=="56-75",1,0)
df$AgeBucket86.95 <- ifelse(df$AgeBucket=="86-95",1,0)
df$AgeBucket96 <- ifelse(df$AgeBucket=="96+",1,0)
df <- subset(df, select=-c(AgeBucket))
# Credit lines bucket
df$CreditLinesBucket1 <- ifelse(df$CreditLinesBucket=="1",1,0)
df$CreditLinesBucket11 <- ifelse(df$CreditLinesBucket=="11+",1,0)
df$CreditLinesBucket2 <- ifelse(df$CreditLinesBucket=="2",1,0)
df$CreditLinesBucket3 <- ifelse(df$CreditLinesBucket=="3",1,0)
df$CreditLinesBucket4 <- ifelse(df$CreditLinesBucket=="4",1,0)
df$CreditLinesBucket5 <- ifelse(df$CreditLinesBucket=="5",1,0)
df$CreditLinesBucket6.10 <- ifelse(df$CreditLinesBucket=="6-10",1,0)
df <- subset(df, select=-c(CreditLinesBucket))
# Real Estate bucket
df$RealEstateBucket1 <- ifelse(df$RealEstateBucket=="1",1,0)
df$RealEstateBucket2 <- ifelse(df$RealEstateBucket=="2",1,0)
df$RealEstateBucket3 <- ifelse(df$RealEstateBucket=="3",1,0)
df$RealEstateBucket4 <- ifelse(df$RealEstateBucket=="4",1,0)
df$RealEstateBucket5 <- ifelse(df$RealEstateBucket=="5+",1,0)
df <- subset(df, select=-c(RealEstateBucket))

################################################################
# SVM Predictions
################################################################

# Load the fitted model
load(file="C://Users/g557428/Projects/seis736_ml_project/models/svm.rdata")

# Make sure you have the right columns
colnames(df)

# Make the predictions
pred_svm <- predict(fit, df[,1:14], decision.values=TRUE, probability=TRUE)
pred_svm <- attributes(pred_svm)$probabilities[,2]

################################################################
# Logit Predictions
################################################################

# Create a separate dataset for PCA
df.pca <- df[,]

# Columns on which we will apply PCA
colnames(df.pca)
pcols <- c(1:11,13:15)

# Apply PCA
x.pca <- prcomp(df.pca[,pcols], center=FALSE, scale.=FALSE)
summary(x.pca)
plot(x.pca, type="l")

# Create a dataframe of the selected PCA columns
x.pca <- data.frame(x.pca$x)
x.pca <- x.pca[,1:4]

# Make the final dataframe
df.pca <- cbind(df.pca[,-pcols], x.pca)

# Cleanup
rm(x.pca, pcols)

# Load the fitted model
load(file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")

# Make the predictions
pred_logit <- predict(fit, newx=as.matrix(df.pca[,]), type="response", s=fit$lambda.1se)

################################################################
# Average the scores
################################################################

# Create a dataset with the final predictions of each model
final <- data.frame(svm=as.vector(pred_svm), logit=as.vector(pred_logit))
head(final, n=20)

# Create a straight average of the model scores
final$Probability <- (final$svm + final$logit) / 2
final <- subset(final, select=(Probability))

# Write to csv
write.csv(x=final, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-final-5.csv")

