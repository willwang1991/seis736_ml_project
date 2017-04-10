################################################################
# Load required packages
################################################################

library("parallelSVM") # Parallel SVM
library("Rlof") # R Local Outlier Factor
library("caret") # Classification and Regression Training
library("pROC")

################################################################
# Data prep
################################################################

# Reading the file
df <- read.csv(file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-modified.csv")

# Scaling Revolving Utilization
df$RevolvingUtilizationOfUnsecuredLines <- as.vector(scale(df$RevolvingUtilizationOfUnsecuredLines))
# Looking at the distribution - this feature has good separation b/w classes
#boxplot(df$RevolvingUtilizationOfUnsecuredLines ~ df$SeriousDlqin2yrs, outline=FALSE)

# Scaling Risk Index
df$RiskIndex <- as.vector(scale(df$RiskIndex))
# Looking at the distribution - this feature has good separation b/w classes
#boxplot(df$RiskIndex ~ df$SeriousDlqin2yrs, outline=FALSE)

# Scaling Serious Delinquency Ratio
df$SeriousDlqRatio <- as.vector(scale(df$SeriousDlqRatio))
# Looking at the distribution - this feature has good separation b/w classes
#boxplot(df$SeriousDlqRatio ~ df$SeriousDlqin2yrs, outline=FALSE)

# Scaling Debt Ratio
df$DebtRatio <- as.vector(scale(df$DebtRatio))
# Not great separation - may want to drop this one
#boxplot(df$DebtRatio ~ df$SeriousDlqin2yrs)

# Income - Modeled
df$ModeledIncome <- as.vector(scale(df$ModeledIncome))
# Not great separation - may want to drop this one
#boxplot(df$ModeledIncome ~ df$SeriousDlqin2yrs, outline=FALSE)

# Income - Monthly
df$MonthlyIncome <- as.vector(scale(df$ModeledIncome))
# Not great separation - may want to drop this one
#boxplot(df$MonthlyIncome ~ df$SeriousDlqin2yrs, outline=FALSE)

# Dropping the "counter" features since these are bucketed. Also dropping features where
# we didn't see much separation
df <- subset(df, select=-c(DebtRatio, ModeledIncome, MonthlyIncome, age, NumberOfTime30.59DaysPastDueNotWorse,
                           NumberOfOpenCreditLinesAndLoans, NumberOfTimes90DaysLate, NumberRealEstateLoansOrLines,
                           NumberOfTime60.89DaysPastDueNotWorse, NumberOfDependents, TotalPastDue))

#---Create dummy variables for the non-numeric features---#

# Past Due 30-59 bucket
df$PD30.59Bucket <- as.factor(df$PD30.59Bucket)
dummy <- data.frame(model.matrix(~PD30.59Bucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(PD30.59Bucket))
# Past Due 60-89 bucket
df$PD60.89Bucket <- as.factor(df$PD60.89Bucket)
dummy <- data.frame(model.matrix(~PD60.89Bucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(PD60.89Bucket))
# Past Due 90+ bucket
df$PD90Bucket <- as.factor(df$PD90Bucket)
dummy <- data.frame(model.matrix(~PD90Bucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(PD90Bucket))
# Total Past Due bucket
df$PastDueBucket <- as.factor(df$PastDueBucket)
dummy <- data.frame(model.matrix(~PastDueBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(PastDueBucket))
# Dependents bucket
df$DependentsBucket <- as.factor(df$DependentsBucket)
dummy <- data.frame(model.matrix(~DependentsBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(DependentsBucket))
# Income bucket
df$IncomeBucket <- as.factor(df$IncomeBucket)
dummy <- data.frame(model.matrix(~IncomeBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(IncomeBucket))
# Age bucket
df$AgeBucket <- as.factor(df$AgeBucket)
dummy <- data.frame(model.matrix(~AgeBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(AgeBucket))
# Credit lines bucket
df$CreditLinesBucket <- as.factor(df$CreditLinesBucket)
dummy <- data.frame(model.matrix(~CreditLinesBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(CreditLinesBucket))
# Real Estate bucket
df$RealEstateBucket <- as.factor(df$RealEstateBucket)
dummy <- data.frame(model.matrix(~RealEstateBucket, df)[,-1])
df <- cbind(df, dummy)
df <- subset(df, select=-c(RealEstateBucket))

# Cleanup
rm(dummy)

#---Doing some basic feature selection by removing highly correlated features---#

# calculate correlation matrix
correlationMatrix <- cor(df[,-1])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)+1
# print indexes of highly correlated attributes
colnames(df)[highlyCorrelated]
# Remove the highly correlated features
df <- df[,-highlyCorrelated]

################################################################
# create train & test datasets
################################################################

# Frequency of the target attribute
table(df$SeriousDlqin2yrs)

# Setting the seed
set.seed(19861005)

# The dataset is extremely skewed towards the 0s. Split the ones and zeros
df.ones <- subset(df, SeriousDlqin2yrs == "1")
df.zeros <- subset(df, SeriousDlqin2yrs == "0")
# Select a subset of zeros
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 10026, replace=FALSE),]
# Combine the zeros and ones
df.new <- rbind(df.ones, df.zeros)
# Randomize the order of the data frame
df.new <- df.new[sample(1:nrow(df.new)),]

# Create a test dataset and a validation dataset
index <- sample(1:nrow(df.new), size=nrow(df.new)*0.6, replace=FALSE)
# Create the test and train dataframes
df.train <- df.new[index,]
df.test <- df.new[-index,]

# Perform some cleanup
rm(df.ones, df.zeros, df.new, highlyCorrelated, correlationMatrix)

# Confirm distribution of the target attribute in both datasets is ~ the same
table(df.train$SeriousDlqin2yrs)
round(prop.table(table(df.train$SeriousDlqin2yrs)),2)
round(prop.table(table(df.test$SeriousDlqin2yrs)),2)

################################################################
# Use LOF to identify local outliers
################################################################

# Get the Local Outlier factor at k=5, k=10, and k=20
dist <- lof(data=df.train, k=c(5, 10, 20))

# Distance cutoff boundary
summary(dist[,3])
q <- quantile(dist[,3], c(0.25, 0.75))
cutoff <- min(max(dist[,3]), q[2]+(q[2]-q[1])*1.5)

# Plotting visualizing the outliers
plot(dist[,3], col=ifelse(dist[,3]>cutoff, "red", "black"))
plot(density(dist))
boxplot(dist)

# Flagging the outliers
df.train$dist <- dist[,3]
df.train$outlier <- ifelse(df.train$dist > cutoff, 1, 0)

# Show the distribution
table(df.train$outlier, df.train$SeriousDlqin2yrs)

# Remove the outliers
df.train <- subset(df.train, outlier == 0)

# Delete the additional columns
df.train <- subset(df.train, select=-c(dist, outlier))

# Show the distribution of the target attribute
table(df.train$SeriousDlqin2yrs)
prop.table(table(df.train$SeriousDlqin2yrs))

################################################################
# tune the model parameters using cross validation
################################################################

# Parameters to test
gammas = c(0.1)
costs = c(100)
params <- expand.grid(gamma=gammas, cost=costs)

# Split data into k-folds
df.train$fold <- createFolds(1:nrow(df.train), k=10, list=FALSE)
# Confirm the folds
table(df.train$fold, df.train$SeriousDlqin2yrs)

# Register number of cores for parallel execution
registerDoParallel(cores = 2)

# Loop through parameter values
result <- foreach(i = 1:nrow(params), .combine = rbind) %do% {
  c <- params[i, ]$cost
  g <- params[i, ]$gamma
  # K-Fold Cross Validation
  out <- foreach(j = 1:max(df.train$fold), .combine=rbind, .inorder=FALSE) %dopar% {
    train <- df.train[df.train$fold != j,]
    test <- df.train[df.train$fold == j,]
    fit <- e1071::svm(x=train[,-c(1,32)], y=train[,1], type="C-classification", 
                      kernel="radial", cost=c, gamma=g, probability=TRUE)
    pred <- predict(fit, test[,-c(1,32)], decision.values=TRUE, probability=TRUE)
    data.frame(y=test$SeriousDlqin2yrs, prob=attributes(pred)$probabilities[,2])
  }
  # Calculate the performance
  cm <- table(out$y, ifelse(out$prob > 0.5, 1, 0))
  tpr <- round(cm[2,2]/sum(cm[2,]),2)
  fpr <- round(cm[1,2]/sum(cm[,2]),2)
  data.frame(params[i, ], tpr=tpr, fpr=fpr)
}

print(result)
rm(out, c, g, i, roc)

################################################################
# Fit the final model using optimized parameters
################################################################

# Fit the model using optimal parameters
fit <- e1071::svm(x=df.train[,-c(1,32)], y=df.train[,1], type="C-classification", 
                  kernel="radial", cost=1, gamma=0.001, probability=TRUE)

# Make the predictions
pred <- predict(fit, df.test[,-1], decision.values=TRUE, probability=TRUE)
pred <- data.frame(y=df.test$SeriousDlqin2yrs, prob=attributes(pred)$probabilities[,2])
roc <- pROC::roc(pred$y, pred$prob, plot=TRUE)
print(roc$auc)
table(pred$y)
head(pred)

# Make the confusion matrix
cm <- table(pred$y, ifelse(pred$prob > 0.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

################################################################
# Visualize the ROC curve
################################################################

################################################################
# Export the model
################################################################
