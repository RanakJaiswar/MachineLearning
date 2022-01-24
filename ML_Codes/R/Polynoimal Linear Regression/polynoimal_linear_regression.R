#Polynoimal Linear Regression

#Importing the dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]#indexes in R starts at 1.

#fitting the linear regression to the dataset
lin_reg = lm(formula = Salary ~ Level,
             data = dataset)

#fitting the polynoimal regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#visualising the linear regression model
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
                colour = "blue") +
  ggtitle("Linear Regression") +
  xlab("Levels") +
  ylab("Salary")

#visualising the polynomail regression model
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = "blue") +
  ggtitle("Polynomial Regression") +
  xlab("Levels") +
  ylab("Salary")

#visualising the polynomail regression model(higher resloution)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x_grid, y = predict(poly_reg, newdata = data.frame(Level = x_grid,
                                                                   Level2 = x_grid^2,
                                                                   Level3 = x_grid^3,
                                                                   Level4 = x_grid^4))),
            colour = "blue") +
  ggtitle("Polynomial Regression(higher resolution)") +
  xlab("Levels") +
  ylab("Salary")

#Predicting results with linear regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))#data.frame to predict single value

#Predicting results with polynomial regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, 
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))