################################################################
# Load required packages
################################################################

# Lasso and Elastic-Net Regularized Generalized Linear Models
library("glmnet")
# Fast Nearest Neighbor
library("FNN")
# R Local Outlier Factor
#library("Rlof")
# Classification and Regression Training
library("caret")

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
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 10000, replace=FALSE),]
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
# Use KNN to identify potential global outliers
################################################################

# KNN Distance
dist <- knn.dist(df.train[,-1], k=20)[,20]

# Function to find the outliers that are more than 1.5 x the IQR
find_outliers <- function(x) {
  q <- as.vector(quantile(x, c(0.25, 0.50, 0.75)))
  q1 <- q[1]
  q3 <- q[3]
  iqr <- q3-q1
  return(c(max(min(x), q1 - 1.5 * iqr), min(max(x), q3 + 1.5 * iqr)))
}

# Get the cutoff value
cutoff <- find_outliers(dist)[2]

# Plotting visualizing the outliers
plot(dist, col=ifelse(dist>cutoff, "red", "black"))

# Flagging the outliers
df.train$dist <- dist
df.train$outlier <- ifelse(df.train$dist > cutoff, 1, 0)

# Show the distribution
table(df.train$outlier, df.train$SeriousDlqin2yrs)

# Remove the outliers
df.train <- subset(df.train, outlier == 0)

# Delete the additional columns
df.train <- df.train[,-c(32,33)]

# Show the distribution of the target attribute
table(df.train$SeriousDlqin2yrs)
prop.table(table(df.train$SeriousDlqin2yrs))

################################################################
# Fit the Logit model
################################################################

# Setting the x and y values
x <- as.matrix(df.train[,-1])
y <- as.matrix(df.train[,1])

# Run logit regression model using lasso w/ 10-fold cross validation
fit <- cv.glmnet(x, y, nfolds=10, family="binomial", type.measure="class", alpha=0.01)
#fit <- cv.glmnet(x, y, nfolds=10, family="binomial", type.measure="auc", alpha=0.01)

################################################################
# visualize model
################################################################

# Plot the misclassification rate and lambda values
plot(fit)

# Visualize lambda values and feature importance
plot(fit$glmnet.fit, xvar="lambda", label=TRUE)
abline(v=log(fit$lambda.min), col="red")
abline(v=log(fit$lambda.1se), col="blue")

# What are the theta values at each of the lambdas?
coef(fit, s=fit$lambda.min)
coef(fit, s=fit$lambda.1se)

################################################################
# Make predictions
################################################################

# Format the validation dataset
x <- as.matrix(df.test[,-1])

# Make predictions at both lambda values. Predict the probability, not the class.
pred <- predict(fit, newx=x, type="response", s=c(fit$lambda.min, fit$lambda.1se))

# Function to round based on a probability
round_prob <- function(x,p) {
  if(x < p) {
    return(0)
  }
  else {
    return(1)
  }
}

# Extract the predictions and round based on the probability
r <- unlist(lapply(pred[,2], FUN=function(x2) round_prob(x2, 0.5)))

# Make the confusion matrix
cm <- table(df.test$SeriousDlqin2yrs, r)

acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

################################################################
# Plot the ROC curve
################################################################

tpr1 <- array(0)
fpr1 <- array(0)
pred <- predict(fit, newx=as.matrix(df.test[,-1]), type="response", s=c(fit$lambda.min, fit$lambda.1se))
for (i in 1:99){
  
  #Probability to try
  p <- i / 100
  
  # Make predictions at the given probability
  r <- data.frame(unlist(lapply(pred[,2], FUN=function(x2) round_prob(x2, p))))

  x <- data.frame(df.test$SeriousDlqin2yrs, r)
  colnames(x)[1] <- "Actual"
  colnames(x)[2] <- "Pred"
  
  x$tp <- ifelse(x$Actual==1 & x$Pred==1, 1, 0)
  x$tn <- ifelse(x$Actual==0 & x$Pred==0, 1, 0)
  x$fp <- ifelse(x$Actual==0 & x$Pred==1, 1, 0)
  x$fn <- ifelse(x$Actual==1 & x$Pred==0, 1, 0)
  
  tp <- sum(x$tp)
  tn <- sum(x$tn)
  fp <- sum(x$fp)
  fn <- sum(x$fn)
  
  cm <- as.table(matrix(c(tp,fp,fn,tn),ncol=2))
  tpr1[i] <- round(cm[1,1]/sum(cm[1,]),2)
  fpr1[i] <- round(cm[2,1]/sum(cm[,1]),2)
}

tpr2 <- array(0)
fpr2 <- array(0)
pred <- predict(fit, newx=as.matrix(df.train[,-1]), type="response", s=c(fit$lambda.min, fit$lambda.1se))
for (i in 1:99){
  
  #Probability to try
  p <- i / 100
  
  # Make predictions at the given probability
  r <- data.frame(unlist(lapply(pred[,2], FUN=function(x2) round_prob(x2, p))))
  
  x <- data.frame(df.train$SeriousDlqin2yrs, r)
  colnames(x)[1] <- "Actual"
  colnames(x)[2] <- "Pred"
  
  x$tp <- ifelse(x$Actual==1 & x$Pred==1, 1, 0)
  x$tn <- ifelse(x$Actual==0 & x$Pred==0, 1, 0)
  x$fp <- ifelse(x$Actual==0 & x$Pred==1, 1, 0)
  x$fn <- ifelse(x$Actual==1 & x$Pred==0, 1, 0)
  
  tp <- sum(x$tp)
  tn <- sum(x$tn)
  fp <- sum(x$fp)
  fn <- sum(x$fn)
  
  cm <- as.table(matrix(c(tp,fp,fn,tn),ncol=2))
  tpr2[i] <- round(cm[1,1]/sum(cm[1,]),2)
  fpr2[i] <- round(cm[2,1]/sum(cm[,1]),2)
}

# Plotting the two
plot(x=fpr1, y=tpr1, xlim=c(0,1), ylim=c(0,1), col="blue")
points(x=fpr2, y=tpr2, type='p', col="red")
abline(a=0, b=1)

################################################################
# Save the model
################################################################

save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/logit5.rdata")
#load(file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")
