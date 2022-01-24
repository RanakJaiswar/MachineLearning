#Data Preprocessing

#Importing the dataset
dataset = read.csv("50_Startups.csv")

#Encoding categorical data
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3))
#Splitting the dataset into training set and test set

library(caTools)
set.seed(123)

split = sample.split(dataset$Profit, SplitRatio = 0.8)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#fitting Multiple Linear Regression

#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)#"." is for spaces in the dataset variables
regressor = lm(formula = Profit ~ .,
               data = training_set)#"." for all independent variables

#predicting the test set result
y_pred = predict(regressor, newdata = test_set)

#building the optimal model using Backward elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

#removing state variable coz be p-value is 0.990 
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)

#removing the administration variable coz the p-value is 0.602
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)

#removing the marketing spend variable coz the p-value is 0.602
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)


#automatic implementation of Backward Elimination in R
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)