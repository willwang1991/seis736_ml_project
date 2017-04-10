library("parallelSVM")
library("FNN")

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

table(df$SeriousDlqin2yrs)

# The dataset is extremely skewed towards the 0s. 
# Let's get a sub-set of 0s that adequately represents the population
df.ones <- subset(df, SeriousDlqin2yrs == "1")
df.zeros <- subset(df, SeriousDlqin2yrs == "0")
set.seed(19861005)
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 11000, replace=FALSE),]
df.new <- rbind(df.ones, df.zeros)

rm(df.ones, df.zeros)

################################################################
# create train & test datasets
################################################################

# Randomize the order of the data frame
df.new <- df.new[sample(1:nrow(df.new)),]

# Create a test dataset and a validation dataset
index <- sample(1:nrow(df.new), size=nrow(df.new)*0.6, replace=FALSE)

df.train <- df.new[index,]
df.test <- df.new[-index,]

# Confirm distribution of the target attribute in both datasets is ~ the same
round(prop.table(table(df.train$SeriousDlqin2yrs)),2)
table(df.train$SeriousDlqin2yrs)
round(prop.table(table(df.test$SeriousDlqin2yrs)),2)

################################################################
# x and y matrices
################################################################

x.train <- df.train[,c(2:11,15,19:20,24)]
y.train <- df.train[,1]
x.test <- df.test[,c(2:11,15,19:20,24)]
y.test <- df.test[,1]

################################################################
# Use KNN to identify potential outliers and remove them
# from the training dataset
# Repeat as many times as needed
################################################################

# Show the columns
colnames(df.train)

# KNN Distance
dist <- knn.dist(x.train, k=20)[,20]

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
plot(density(dist))
boxplot(dist)

# Flagging the outliers
df.train$dist <- dist
df.train$outlier <- ifelse(df.train$dist > cutoff, 1, 0)

# Show the distribution
table(df.train$outlier, df.train$SeriousDlqin2yrs)

# Remove the outliers
df.train <- subset(df.train, outlier == 0)

# Delete the additional columns
df.train <- df.train[,-c(26,27)]

# Show the distribution of the target attribute
table(df.train$SeriousDlqin2yrs)
prop.table(table(df.train$SeriousDlqin2yrs))

# Resetting the training dataset
x.train <- df.train[,c(2:11,15,19:20,24)]
y.train <- df.train[,1]

################################################################
# tune the model parameters
################################################################

# 10-fold cross validation to identify optimal model parameters
svm <- parallelSVM(x=x.train, y=y.train, numberCores=2, type="C-classification",
			 kernal="radial", gamma=0.001, cost=1, cross=10, probability=TRUE, seed=19861005, 
			 class.weights=c("0"=0.5, "1"=2))

# Print out the results
print(svm)

# Making the predictions
pred <- predict(svm, x.test, probability=TRUE)
pred <- attr(pred, "probabilities")[,2]

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
r <- unlist(lapply(pred, FUN=function(x2) round_prob(x2, 0.5)))

# Make the confusion matrix
cm <- table(df.test$SeriousDlqin2yrs, r)

acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

#################################################
# Create ROC curve
#################################################

tpr1 <- array(0)
fpr1 <- array(0)
pred <- predict(svm, x.test, probability=TRUE)
pred <- attr(pred, "probabilities")[,2]
for (i in 1:99){
  
  #Probability to try
  p <- i / 100
  
  # Make predictions at the given probability
  r <- data.frame(unlist(lapply(pred, FUN=function(x2) round_prob(x2, p))))
  
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
pred <- predict(svm, x.train, probability=TRUE)
pred <- attr(pred, "probabilities")[,2]
for (i in 1:99){
  
  #Probability to try
  p <- i / 100
  
  # Make predictions at the given probability
  r <- data.frame(unlist(lapply(pred, FUN=function(x2) round_prob(x2, p))))
  
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

save(svm, file="C://Users/g557428/Projects/seis736_ml_project/models/svm1.rdata")