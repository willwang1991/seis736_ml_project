# glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
library("glmnet")

################################################################
# Data prep
################################################################

# Reading the file
df <- read.csv(file="C://Users/g557428/Projects/seis736_ml_project/data/processed/cs-modified.csv")

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

# The dataset is extremely skewed towards the 0s. 
# Let's get a sub-set of 0s that adequately represents the population
df.ones <- subset(df, SeriousDlqin2yrs == "1")
df.zeros <- subset(df, SeriousDlqin2yrs == "0")
set.seed(19861005)
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 15000, replace=FALSE),]
df.new <- rbind(df.ones, df.zeros)

rm(df.ones, df.zeros)

################################################################
# build model
################################################################

# Randomize the order of the data frame
df.new <- df.new[sample(1:nrow(df.new)),]

# Create a test dataset and a validation dataset
index <- sample(1:nrow(df.new), size=nrow(df.new)*0.6, replace=FALSE)

df.train <- df.new[index,]
df.test <- df.new[-index,]

# Confirm distribution of the target attribute in both datasets is ~ the same
round(prop.table(table(df.train$SeriousDlqin2yrs)),2)
round(prop.table(table(df.test$SeriousDlqin2yrs)),2)

# Formula to use for the regression model
f <- SeriousDlqin2yrs ~ 
  RevolvingUtilizationOfUnsecuredLines +
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

# Formatting the training dataset for consumption by the glmnet model
x <- model.matrix(f, data=df.train)[,-1]
y <- as.matrix(df.train[,1])

# Run logit regression model using lasso w/ 10-fold cross validation
fit <- cv.glmnet(x, y, nfolds=10, family="binomial", type.measure="class", alpha=0.1)
#fit <- cv.glmnet(x, y, nfolds=10, family="binomial", type.measure="auc", alpha=0.1)

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
x <- model.matrix(f, data=df.test)[,-1]

# Make predictions at both lambda values. Predict the probability, not the class.
pred <- predict(fit, newx=x, type="response", s=c(fit$lambda.min, fit$lambda.1se))

# Function to round based on a probability
# Assumpition here is both parameters are 0..1
round_prob <- function(x,p) {
  if(x < p) {
    return(0)
  }
  else {
    return(1)
  }
}

# Extract the predictions and round based on the probability
r <- unlist(lapply(pred[,2], FUN=function(x2) round_prob(x2, 0.4)))

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
for (i in 1:99){
  
  #Probability to try
  p <- i / 100
  
  # Make predictions at the given probability
  r <- data.frame(unlist(lapply(pred[,1], FUN=function(x2) round_prob(x2, p))))

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
  tpr2[i] <- round(cm[1,1]/sum(cm[1,]),2)
  fpr2[i] <- round(cm[2,1]/sum(cm[,1]),2)
}

# Plotting the two
plot(x=fpr1, y=tpr1, xlim=c(0,1), col="blue")
points(x=fpr2, y=tpr2, type='p', col="red")
abline(a=0, b=1)

################################################################
# Save the model
################################################################

save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")
#load(file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")
