#Regression template

#Importing the dataset
dataset = read.csv("Position_Salaries.csv")
dataset = dataset[2:3]#indexes in R starts at 1.


#fitting the polynoimal regression to the dataset


#visualising the polynomail regression model
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = "red") +
  geom_line(aes(x_grid , y = predict(poly_reg, newdata = data.frame(Level = x_grid))),
            colour = "blue") +
  ggtitle("Polynomial Regression") +
  xlab("Levels") +
  ylab("Salary")


#Predicting results with polynomial regression
y_pred = predict(poly_reg, data.frame(Level = 6.5, 
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))