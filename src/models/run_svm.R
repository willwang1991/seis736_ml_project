library("e1071")
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
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 10026, replace=FALSE),]
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

x <- df.train[,c(2:11,15,19:20,24)]
y <- df.train[,1]

################################################################
# Use KNN to identify potential outliers and remove them
# from the training dataset
# Repeat as many times as needed
################################################################

# Show the columns
colnames(df.train)

# KNN Distance
dist <- knn.dist(x, k=20)[,20]

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

################################################################
# tune the model parameters
################################################################

# Parameters to test
#c = 10^(-1:2)
c = c(.5, 1, 10)
g = c(.5, 1, 2)
tc <- tune.control(cross = 10)

# 10-fold cross validation to identify optimal model parameters
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", 
                 ranges=list(cost=c, gamma=g), tunecontrol=tc)

# Print out the results
print(svm_tune)

################################################################
# fit the model w/ optimal params
################################################################

colnames(df.train)

# Fitting the SVM model
fit <- svm(x=df.train[,c(2:11,15,19:20,24)], y=df.train[,1], probability=TRUE, 
           type='C-classification')
summary(fit)

# Making the predictions
pred <- predict(fit, df.test[,c(2:11,15,19:20,24)], probability=TRUE)
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