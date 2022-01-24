#Importing the dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]#indexes in R starts at 1.

#fitting the polynoimal regression to the dataset

#install.packages("e1071")
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = "eps-regression")

#Predicting results with support vector regression
y_pred = predict(regressor, data.frame(Level = 6.5))

#visualising the support vector regression model
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = "blue") +
  ggtitle("support vector Regression") +
  xlab("Levels") +
  ylab("Salary")


