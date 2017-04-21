################################################################
# Load required packages
################################################################

library(xgboost)
library(foreach)

# Setting the seed
set.seed(19861005)

################################################################
# Misc Functions
################################################################

# Getting a dataframe of TPR and FPR at percent likelihoods
plot_roc <- function(predictions, actual) {
  tpr <- array(0)
  fpr <- array(0)
  for (i in 1:99) {
    #Probability threshold
    p <- i / 100
    # Make predictions at the given probability
    r <- data.frame(Pred=unlist(lapply(predictions, FUN=function(x2) ifelse(x2 > p, 1, 0))))
    x <- data.frame(Actual=actual, Pred=r)
    x$tp <- ifelse(x$Actual==1 & x$Pred==1, 1, 0)
    x$tn <- ifelse(x$Actual==0 & x$Pred==0, 1, 0)
    x$fp <- ifelse(x$Actual==0 & x$Pred==1, 1, 0)
    x$fn <- ifelse(x$Actual==1 & x$Pred==0, 1, 0)
    # Count the true positives and true negatives
    tp <- sum(x$tp)
    tn <- sum(x$tn)
    fp <- sum(x$fp)
    fn <- sum(x$fn)
    # Calculate the TPR and FPR
    cm <- as.table(matrix(c(tp,fp,fn,tn),ncol=2))
    tpr[i] <- round(cm[1,1]/sum(cm[1,]),2)
    fpr[i] <- round(cm[2,1]/sum(cm[,1]),2)
  }
  return(data.frame(tpr=tpr, fpr=fpr))
}

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
df$PD60.89Bucket4 <- ifelse(df$PD60.89Bucket=="4+",1,0)
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
df$CreditLinesBucket2 <- ifelse(df$CreditLinesBucket=="2",1,0)
df$CreditLinesBucket3 <- ifelse(df$CreditLinesBucket=="3",1,0)
df$CreditLinesBucket4 <- ifelse(df$CreditLinesBucket=="4",1,0)
df$CreditLinesBucket5 <- ifelse(df$CreditLinesBucket=="5",1,0)
df$CreditLinesBucket6.10 <- ifelse(df$CreditLinesBucket=="6-10",1,0)
df$CreditLinesBucket11 <- ifelse(df$CreditLinesBucket=="11+",1,0)
df <- subset(df, select=-c(CreditLinesBucket))
# Real Estate bucket
df$RealEstateBucket1 <- ifelse(df$RealEstateBucket=="1",1,0)
df$RealEstateBucket2 <- ifelse(df$RealEstateBucket=="2",1,0)
df$RealEstateBucket3 <- ifelse(df$RealEstateBucket=="3",1,0)
df$RealEstateBucket4 <- ifelse(df$RealEstateBucket=="4",1,0)
df$RealEstateBucket5 <- ifelse(df$RealEstateBucket=="5+",1,0)
df <- subset(df, select=-c(RealEstateBucket))

################################################################
# create train & test datasets for SVM
################################################################

# Frequency of the target attribute
table(df$SeriousDlqin2yrs)
prop.table(table(df$SeriousDlqin2yrs))

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
rm(df.ones, df.zeros)

# Confirm distribution of the target attribute in both datasets is ~ the same
table(df.train$SeriousDlqin2yrs)
table(df.test$SeriousDlqin2yrs)
round(prop.table(table(df.train$SeriousDlqin2yrs)),2)
round(prop.table(table(df.test$SeriousDlqin2yrs)),2)

################################################################
# XGBoost Model
################################################################

dtrain <- xgb.DMatrix(data=as.matrix(df.train[,-1]), label=df.train[,1])
dtest <- xgb.DMatrix(data=as.matrix(df.test[,-1]), label=df.test[,1])

# Params
params <- list(booster="gbtree", objective="binary:logistic", max.depth=2, 
               eta=0.01, gamma=5, subsample=0.5, eval_metric="error")

# Fit w/ cross validation
fit <- xgb.cv(params=params, data=dtrain, nrounds=1000, nfold=10, 
              early_stopping_rounds=20, maximize=FALSE, stratified=TRUE)

fit$best_iteration

# Build the model
fit <- xgb.train(params=params, data=dtrain, nrounds=40, early_stopping_rounds=20, maximize=FALSE,
                 watchlist=list(val=dtest, train=dtrain))

# Make the predictions
pred <- predict(fit, dtest)

# Calculate the performance
cm <- table(df.test$SeriousDlqin2yrs, ifelse(pred > 0.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

# Viewing feature importance
mat <- xgb.importance(feature_names=colnames(df.train), model=fit)
xgb.plot.importance(importance_matrix=mat)

################################################################
# XGBoost - Grid search using Caret
################################################################

dtrain <- xgb.DMatrix(data=as.matrix(df.train[,-1]), label=df.train[,1])
dtest <- xgb.DMatrix(data=as.matrix(df.test[,-1]), label=df.test[,1])

# Creating parameter grid
xgbGrid <- expand.grid(nrounds=c(50),
                       max_depth=c(1, 2, 3, 4),
                       eta=c(0.001, 0.003, 1),
                       gamma=c(0, 1, 2),
                       colsample_bytree=c(1, 0.5, 0.25),
                       min_child_weight=c(1,2),
                       subsample=c(0.25, 0.5, 1))

# Loop through the grid
result <- foreach(i = 1:nrow(xgbGrid), .combine = rbind) %do% {
  
  # Run XGBoost
  fit <- xgboost(data=dtrain, nrounds=xgbGrid$nrounds[i], max_depth=xgbGrid$max_depth[i], eta=xgbGrid$eta[i],
                 gamma=xgbGrid$gamma[i], colsample_bytree=xgbGrid$colsample_bytree[i], 
                 min_child_weight=xgbGrid$min_child_weight[i], subsample=xgbGrid$subsample[i],
                 early_stopping_rounds=20, booster="gbtree", objective="binary:logistic")
  
  # Make the predictions
  pred <- predict(fit, dtest)
  
  # Make the confusion matrix
  cm <- table(df.test$SeriousDlqin2yrs, ifelse(pred > 0.5, 1, 0))
  acc <- round(sum(diag(cm))/sum(cm),2)
  tpr <- round(cm[2,2]/sum(cm[2,]),2)
  fpr <- round(cm[1,2]/sum(cm[,2]),2)
  f1 <- round((2*tpr*fpr)/(tpr+fpr),2)
  
  # Dataframe metrics along with alphas
  data.frame(nrounds=xgbGrid$nrounds[i], max_depth=xgbGrid$max_depth[i], eta=xgbGrid$eta[i],
             gamma=xgbGrid$gamma[i], colsample_bytree=xgbGrid$colsample_bytree[i], 
             min_child_weight=xgbGrid$min_child_weight[i], subsample=xgbGrid$subsample[i], 
             acc=acc, tpr=tpr, fpr=fpr, f1=f1)
}
