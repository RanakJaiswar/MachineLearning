#Importing the dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]#indexes in R starts at 1.

#fitting the decision tree regression to the dataset

#install.packages("rpart")
library(rpart)
regressor = rpart(formula = Salary ~ .,
                data = dataset,
                control = rpart.control(minsplit = 1))

#Predicting results with decision tree regression
y_pred = predict(regressor, data.frame(Level = 6.5))

#visualising the decision tree regression model
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Decision Tree Regression(higher resolution)") +
  xlab("Levels") +
  ylab("Salary")


