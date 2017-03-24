# glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
library("glmnet")

################################################################
# Data prep
################################################################

# Reading the file
df <- read.csv(file="C://Users/Dane/Documents/GitHub/seis736_ml_project/data/processed/cs-modified-2.csv")

table(df$SeriousDlqin2yrs)

# The dataset is extremely skewed towards the 0s. 
# Let's get a sub-set of 0s that adequately represents the population
df.ones <- subset(df, SeriousDlqin2yrs == "1")
df.zeros <- subset(df, SeriousDlqin2yrs == "0")
set.seed(19861005)
df.zeros <- df.zeros[sample(1:nrow(df.zeros), 20000, replace=FALSE),]
df.new <- rbind(df.ones, df.zeros)

# Is the subset representatize of the population? 
# Need to do some signifance testing
summary(df$RevolvingUtilizationOfUnsecuredLines)
summary(df.new$RevolvingUtilizationOfUnsecuredLines)

boxplot(df$RevolvingUtilizationOfUnsecuredLines, 
        df.new$RevolvingUtilizationOfUnsecuredLines, 
        outline=FALSE)

boxplot(df.zeros$RevolvingUtilizationOfUnsecuredLines, 
        df.ones$RevolvingUtilizationOfUnsecuredLines, 
        outline=FALSE)

rm(df, df.ones, df.zeros)

# Formatting the categorical features as factors
df.new$SeriousDlqin2yrs <- as.factor(df.new$SeriousDlqin2yrs)
#df.new$NumberOfTime30.59DaysPastDueNotWorse <- as.factor(df.new$NumberOfTime30.59DaysPastDueNotWorse)
#df.new$NumberOfOpenCreditLinesAndLoans <- as.factor(df.new$NumberOfOpenCreditLinesAndLoans)
#df.new$NumberOfTimes90DaysLate <- as.factor(df.new$NumberOfTimes90DaysLate)
#df.new$NumberRealEstateLoansOrLines <- as.factor(df.new$NumberRealEstateLoansOrLines)
#df.new$NumberOfTime60.89DaysPastDueNotWorse <- as.factor(df.new$NumberOfTime60.89DaysPastDueNotWorse)
#df.new$NumberOfDependents <- as.factor(df.new$NumberOfDependents)

################################################################
# build model
################################################################

# Randomize the order of the data frame
df.new <- df.new[sample(1:nrow(df.new)),]

# Create a test dataset and a validation dataset
index <- sample(1:nrow(df.new), 
                size=nrow(df.new)*0.8, 
                replace=FALSE)

df.train <- df.new[index,]
df.test <- df.new[-index,]

# Confirm distribution of the target attribute in both datasets is ~ the same
prop.table(table(df.train$SeriousDlqin2yrs))
prop.table(table(df.test$SeriousDlqin2yrs))

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
  NumberOfDependents

# Formatting the training dataset for consumption by the glmnet model
x <- model.matrix(f, data=df.train)[,-1]
y <- as.matrix(df.train[,11])

# Run logit regression model using lasso w/ 10-fold cross validation
fit <- cv.glmnet(x, y, nfolds=10, family="binomial", type.measure="auc", alpha=0.1)

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

#head(round(pred[,2]))
#head(df.test$SeriousDlqin2yrs)

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
r <- unlist(lapply(pred[,1], FUN=function(x2) round_prob(x2, 0.5)))

head(r)
head(pred[,1])

# Make the confusion matrix
cm <- table(df.test$SeriousDlqin2yrs, r)

acc <- round(sum(diag(cm))/sum(cm),2)
tpr <- round(cm[2,2]/sum(cm[2,]),2)
fpr <- round(cm[1,2]/sum(cm[,2]),2)
f1 <- round((2*tpr*fpr)/(tpr+fpr),2)

################################################################
# Plot the ROC curve
################################################################

tpr <- array(0)
fpr <- array(0)
for (i in 1:99){
  p <- i / 100
  r <- unlist(lapply(pred[,1], FUN=function(x2) round_prob(x2, p)))
  cm <- table(df.test$SeriousDlqin2yrs, r)
  tpr[i] <- round(cm[2,2]/sum(cm[2,]),2)
  fpr[i] <- round(cm[1,2]/sum(cm[,2]),2)
}

plot(x=fpr, y=tpr, xlim=c(0,1), col="blue")
abline(a=0, b=1)
