################################################################
# Load required packages
################################################################

library("glmnet")
library("e1071")
library("nnet")

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
# SVM Model
################################################################

# Setting the x and y
x <- df.train[ ,2:15]
y <- df.train[ ,1]

# Setting the regularization parameters
gamma <- 0.01 # This is the kernal regularization. High gamma = high error, low gamma = low error
cost <- 10 # High cost = low error, low cost = high error
epsilon <- 1
degree <- 2
coef <- 7

# Fit the SVM model
fit <- e1071::svm(x=x, y=y, type="C-classification", kernel="poly", gamma=gamma, cost=cost, epsilon=epsilon,
                  degree=degree, coef0=coef, probability=TRUE, scale=FALSE)

#load(file="C://Users/g557428/Projects/seis736_ml_project/models/svm.rdata")

# Summarize the SVM model
summary(fit)

# Make the predictions
pred_svm <- predict(fit, df.test[,2:15], decision.values=TRUE, probability=TRUE)
pred_svm <- attributes(pred_svm)$probabilities[,2]

# Calculate the performance
cm <- table(df.test$SeriousDlqin2yrs, ifelse(pred_svm > 0.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

#------------------------------------------#
#------ROC Curve---------------------------#
#------------------------------------------#

# Predict on test data
pred_svm <- predict(fit, df.test[,2:15], decision.values=TRUE, probability=TRUE)
pred_svm <- attributes(pred_svm)$probabilities[,2]
roc <- plot_roc(pred_svm, df.test$SeriousDlqin2yrs)
tpr1 <- roc[,1]
fpr1 <- roc[,2]

# Predict on training data
pred_svm <- predict(fit, df.train[,2:15], decision.values=TRUE, probability=TRUE)
pred_svm <- attributes(pred_svm)$probabilities[,2]
roc <- plot_roc(pred_svm, df.train$SeriousDlqin2yrs)
tpr2 <- roc[,1]
fpr2 <- roc[,2]

# Visualizing ROC of training vs. test
plot(x=fpr1, y=tpr1, xlim=c(0,1), ylim=c(0,1), col="blue")
points(x=fpr2, y=tpr2, type='p', col="red")
abline(a=0, b=1)
legend("topright", legend=c("Test", "Train"), col=c("blue","red"), lty=1)

#------------------------------------------#
#------Outlier Detection & Elimination-----#
#------------------------------------------#

# Support vectors
sv <- data.frame(index=as.numeric(rownames(fit$SV)), coefs=fit$coefs)

# Decision values
out <- data.frame(index=as.numeric(attributes(fit$decision.values[,1])$names),
                  decision.values=as.numeric(fit$decision.values))

# Merge support vectors with decision values
out <- merge(out, sv, by="index")
out <- merge(out, data.frame(index=as.numeric(rownames(df.train)), y=df.train$SeriousDlqin2yrs), by="index")

# Viewing the ones and zeros support vectors
out.ones <- subset(out, y==1)
out.zeros <- subset(out, y==0)

# What is the distribution of their decision values
summary(out.ones$decision.values)
summary(out.zeros$decision.values)
plot(density(out.ones$decision.values))
lines(density(out.zeros$decision.values))

# Identifying cutoff boundaries
cutoff.1 <- quantile(out.ones$decision.values, c(0.90))[1]
cutoff.2 <- quantile(out.zeros$decision.values, c(0.10))[1]

# Flag outliers to remove
out$flag <- ifelse(out$y==1 & out$decision.values>cutoff.1, 1, 
                   ifelse(out$y==0 & out$decision.values<cutoff.2, 1, 0))

# Distribution of the outliers
table(out$flag)

# Vector of index values for the outlier records
ol.index <- subset(out, flag==1, select=c(index))[,1]

# Removing outlier values from the test data. Skip this step on the first run
x <- subset(df.train, !(as.numeric(rownames(df.train)) %in% ol.index))[,2:15]
y <- subset(df.train, !(as.numeric(rownames(df.train)) %in% ol.index), select=c(SeriousDlqin2yrs))
prop.table(table(y))
#------------------------------------------#
#------------------------------------------#

# Cleanup
rm(fpr1, tpr1, fpr2, tpr2, ol.index, cutoff.1, cutoff.2, sv, out, out.ones, out.zeros,
   tpr, fpr, gamma, acc, cm, coef, cost, degree, epsilon, f1, x, y, roc)

# Save the model
save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/svm.rdata")

################################################################
# PCA - Principal Component Analysis
################################################################

# Create a separate dataset for PCA
df.pca <- df.new[,]

# Columns on which we will apply PCA
colnames(df.pca)
pcols <- c(2:12,14:16)

# Apply PCA
x.pca <- prcomp(df.pca[,pcols], center=FALSE, scale.=FALSE)
summary(x.pca)
plot(x.pca, type="l")

# Create a dataframe of the selected PCA columns
x.pca <- data.frame(x.pca$x)
x.pca <- x.pca[,1:4]

# Make the final dataframe
df.pca <- cbind(df.pca[,-pcols], x.pca)

colnames(df.pca)

# Cleanup
rm(x.pca, pcols)

################################################################
# create train & test datasets for Logistic Regression
################################################################

# Frequency of the target attribute
table(df.pca$SeriousDlqin2yrs)

# Create the test and train dataframes
df.train <- df.pca[index,]
df.test <- df.pca[-index,]

# Confirm distribution of the target attribute in both datasets is ~ the same
table(df.train$SeriousDlqin2yrs)
round(prop.table(table(df.train$SeriousDlqin2yrs)),2)
round(prop.table(table(df.test$SeriousDlqin2yrs)),2)

################################################################
# GLMNET Logistic Model
################################################################

# Parameter tuning - there is really only one parameter for glmnet >> alpha / learning rate
alpha = c(0.0001, 0.001, 0.01, 0.1, 1.0)

# Loop through the alphas
result <- foreach(a = 1:length(alpha), .combine = rbind) %do% {
  
  # Run logit regression model using lasso w/ 10-fold cross validation
  fit <- cv.glmnet(as.matrix(df.train[,-1]), 
                   as.matrix(df.train[,1]), 
                   nfolds=10, 
                   family="binomial", 
                   type.measure="class", 
                   alpha=alpha[a]
                   )
  
  # Make predictions
  pred <- predict(fit, newx=as.matrix(df.test[,-1]), type="response", s=fit$lambda.1se)
  
  # Make the confusion matrix
  cm <- table(df.test$SeriousDlqin2yrs, round(pred))
  acc <- round(sum(diag(cm))/sum(cm),2)
  tpr <- round(cm[2,2]/sum(cm[2,]),2)
  fpr <- round(cm[1,2]/sum(cm[,2]),2)
  f1 <- round((2*tpr*fpr)/(tpr+fpr),2)
  
  # Dataframe metrics along with alphas
  data.frame(alpha=alpha[a], acc=acc, tpr=tpr, fpr=fpr, f1=f1)
}

# Build final Logit model
fit <- cv.glmnet(as.matrix(df.train[,-c(1,3:4,7:12,16)]), 
                 as.matrix(df.train[,1]), 
                 nfolds=10, 
                 family="binomial", 
                 type.measure="auc", 
                 alpha=1
                 )

# Visualize the model
plot(fit)
plot(fit$glmnet.fit, xvar="lambda", label=TRUE)
abline(v=log(fit$lambda.min), col="red")
abline(v=log(fit$lambda.1se), col="blue")

# Load the fitted model
#load(file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")

# Make predictions at both lambda values. Predict the probability, not the class.
pred_logit <- as.vector(predict(fit, newx=as.matrix(df.test[,-c(1,3:4,7:12,16)]), 
                                type="response", s=fit$lambda.1se))

# Make the confusion matrix
cm <- table(df.test$SeriousDlqin2yrs, ifelse(pred_logit > 0.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

#------------------------------------------#
#------ROC Curve---------------------------#
#------------------------------------------#

# Predict on test data
pred_logit <- as.vector(predict(fit, newx=as.matrix(df.test[,-c(1,3:4,7:12,16)]), 
                                type="response", s=fit$lambda.1se))
roc <- plot_roc(pred_logit, df.test$SeriousDlqin2yrs)
tpr1 <- roc[,1]
fpr1 <- roc[,2]

# Predict on training data
pred_logit <- as.vector(predict(fit, newx=as.matrix(df.train[,-c(1,3:4,7:12,16)]), 
                                type="response", s=fit$lambda.1se))
roc <- plot_roc(pred_logit, df.train$SeriousDlqin2yrs)
tpr2 <- roc[,1]
fpr2 <- roc[,2]

# Visualizing ROC of training vs. test
plot(x=fpr1, y=tpr1, xlim=c(0,1), ylim=c(0,1), col="blue")
points(x=fpr2, y=tpr2, type='p', col="red")
abline(a=0, b=1)
legend("topright", legend=c("Test", "Train"), col=c("blue","red"), lty=1)

#------------------------------------------#
#------Outlier Removal---------------------#
#------------------------------------------#

# Fit the linear model
fit <- lm(formula=SeriousDlqin2yrs ~ ., data=df.train) 
summary(fit)

# Map the residuals values to the test dataset
df.train$resid <- resid(fit)

# Visualize the residuals
plot(density(df.train$resid))

# Find the value of 2x the standard deviation of the residuals
sd2 <- 2*sd(df.train$resid) 

# Flagging hte outliers - those obs whose residuals are more than 2x stdv
df.train$outs <- ifelse(abs(df.train$resid) > sd2, 1, 0)
#df.train$outs <- ifelse(abs(df.train$resid) > sd2 & df.train$SeriousDlqin2yrs == 0, 1, 0)
table(df.train$outs)
table(df.train$outs, df.train$SeriousDlqin2yrs)

# Removing the outliers
df.train <- subset(df.train, outs==0) 

# Remove the residuals
df.train <- subset(df.train, select=-c(outs,resid))
#------------------------------------------#

# Save the model
save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/logit.rdata")

# Cleanup
rm(acc, cm, f1, fpr, fpr1, fpr2, tpr, tpr1, tpr2, roc, fit)

################################################################
# Neural Network Model
################################################################

# Parameter tuning
size <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20) # Number of units in hidden layer
maxit <- c(200) # Maximum number of iterations
rang <- 0.5
decay <- 10^seq(-3, 0, length = 10) # Parameter for weight decay
params <- data.frame(expand.grid(size=size, maxit=maxit, decay=decay)) # Creating the parameter search grid

# Loop through the search grid
result <- foreach(i = 1:nrow(params), .combine = rbind) %do% {
  
  # Train the model
  fit <- nnet(x=df.train[,-1], y=df.train[,1], size=params[i,]$size, maxit=params[i,]$maxit,
              decay=params[i,]$decay, MaxNWts=4000)
  
  # Make the predictions
  pred_nn <- predict(fit, df.test[,-1], prob=TRUE)
  
  # Create the confusion matrix
  cm <- table(df.test[,1], ifelse(pred_nn > 0.5, 1, 0))
  acc <- tryCatch(round(sum(diag(cm))/sum(cm),2), error=function(x) 0)
  tpr <- tryCatch(round(cm[2,2]/sum(cm[2,]),2), error=function(x) 0)
  fpr <- tryCatch(round(cm[1,2]/sum(cm[,2]),2), error=function(x) 0)
  f1 <- tryCatch(round((2*tpr*fpr)/(tpr+fpr),2), error=function(x) 0)
  
  # Dataframe metrics along with alphas
  data.frame(size=params[i,]$size, maxit=params[i,]$maxit, decay=params[i,]$decay, 
             acc=acc, tpr=tpr, fpr=fpr, f1=f1)
}

# Write the params to csv
print(result)
write.csv(x=result, file="C://Users/g557428/Projects/seis736_ml_project/data/processed/nn_params.csv", row.names=FALSE)

# Train the model using the optimized parameters
fit <- nnet(x=df.train[,-1], y=df.train[,1], size=100, maxit=200, MaxNWts=10000)

#load(file="C://Users/g557428/Projects/seis736_ml_project/models/nnet.rdata")

# Make predictions
pred_nn <- predict(fit, df.test[,-1], prob=TRUE)

# Create the confusion matrix
cm <- table(df.test[,1], ifelse(pred_nn > 0.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

#------------------------------------------#
#------ROC Curve---------------------------#
#------------------------------------------#

# Predict on test data
pred_nn <- predict(fit, df.test[,-1], prob=TRUE)
roc <- plot_roc(pred_nn, df.test$SeriousDlqin2yrs)
tpr1 <- roc[,1]
fpr1 <- roc[,2]

# Predict on training data
pred_nn <- predict(fit, df.train[,-1], prob=TRUE)
roc <- plot_roc(pred_nn, df.train$SeriousDlqin2yrs)
tpr2 <- roc[,1]
fpr2 <- roc[,2]

# Visualizing ROC of training vs. test
plot(x=fpr1, y=tpr1, xlim=c(0,1), ylim=c(0,1), col="blue")
points(x=fpr2, y=tpr2, type='p', col="red")
abline(a=0, b=1)
legend("topright", legend=c("Test", "Train"), col=c("blue","red"), lty=1)

# Save the model
save(fit, file="C://Users/g557428/Projects/seis736_ml_project/models/nnet.rdata")

# Cleanup
rm(params, result, roc, acc, cm, decay, f1, fit, fpr, fpr1, fpr2, i, maxit, rang, size,
   tpr, tpr1, tpr2)

################################################################
# Average the scores / Majority Vote
################################################################

# Create a dataset with the final predictions of each model
final <- data.frame(y=df.test[,1], svm=pred_svm, logit=pred_logit, nnet=pred_nn)
head(final, n=20)

# Majority Voting
final$majority <- ifelse((round(final$svm) + round(final$logit) + round(final$nnet)) > 1, 1, 0)

# Create the confusion matrix
cm <- table(final$y, final$majority)
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

# Create a straight average of the model scores
final$avg <- (final$svm + final$logit + final$nnet) / 3

# Create the confusion matrix
cm <- table(final$y, ifelse(final$avg>.5, 1, 0))
acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

# TPR / FPR at various probabilities
roc <- plot_roc(final$avg, final$y)
tpr <- roc[,1]
fpr <- roc[,2]

# Plotting the ROC curve
plot(x=fpr, y=tpr, xlim=c(0,1), ylim=c(0,1), col="blue")
abline(a=0, b=1)

# Viewing some of the false positives
fp <- as.numeric(rownames(subset(final, y==0 & majority==1)))
View(df[fp,])

