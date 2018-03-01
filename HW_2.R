# Remove all extraneous variables
rm(list = ls())

mse <- function(sm) {
  m <- mean(sm$residuals^2)
  return(m);
}

# Part I #################################################################################
# Simulating the fake data set
# from a normal distribution, simulate 1000 values with a mean of 5 and std of 7
x_1 <- rnorm(1000, 5, 7) 

# plotting data
hist(x_1, col="grey")

true_error <- rnorm(1000, 0, 2)
true_beta_0 <- 1.1
true_beta_1 <- -8.2 
true_beta_2 <- 3
true_beta_3 <- -5

y <- true_beta_0 + true_beta_1*x_1 + true_error

hist(y) 
plot(x_1, y, pch = 20, col ="red") 

# Checking the density of the repsonse variable
plot(density(y), main="Density Plot: Y", ylab="y") 

# Building a regression model, and finding estimated beta values
mod1 <- lm(y~x_1)
summary(mod1)

plot(mod1, pch = 16, which = 1)

# Declaring second variable, and finding linear distributions
x_2 <- rgamma(1000, 2, 10)
hist(x_2, col="blue")

y_2 <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_error

# fitting a model depending only on x_1
mod2 <- lm(y_2~x_1)
summary(mod2)

# fitting a model depending only on x_2
mod3 <- lm(y_2~x_2)
summary(mod3)

# finding a model dependant on both x_1 and X_2
mod4 <- lm(y_2~x_1 + x_2)
summary(mod4)


# Finding the sample  
data.1 <- data.frame(cbind(x_1, x_2, y))

set.seed(100)
trainingIndices <- sample(1:nrow(data.1), 0.8*nrow(data.1), replace = FALSE, prob = NULL)

training <- data.1[trainingIndices ,]
test <- data.1[-trainingIndices ,]

# finding the model of sample data
modTraining1 <- lm(training[, 3] ~ training[, 1] + training[, 2])

# Calculate the square mean
sq_train <- mse(modTraining1)

# test on test data 
sq_test <- mean((test$y - predict.lm(modTraining1, test))^2)

# Delaring new variable z 
z <- x_1^2

y_3 <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_beta_3*z+ true_error

# declaring a model based on x_1, AND z only
mod5 <- lm(y_3~x_1)
summary(mod5) 

mod6 <- lm(y_3~x_1 + z)
summary(mod6)

# DO THE SAMPLE MEAN SHIT SHIT SHIT

# Playing around with data--just change the values in this section
# Then run only this section and record the results in the report
true_error.t <- rnorm(1000, 0, 2)
true_beta_0.t <- 5
true_beta_1.t <- 2

x_1.t <- rnorm(1000, 2) 
x_2.t <- rexp(1000)

y_3 <- true_beta_0.t*x_1.t + true_beta_1.t*x_2.t + true_error.t

data.4 <- data.frame(cbind(x_1.t, x_2.t, y_3))

mod7 <- lm(y_3~x_1.t + x_2.t, data = data.4)
summary(mod7)

plot(mod7, pch = 16, which = 1)

# plotting histograms and scatterplots (just for part d)
hist(x_1.t, col="red")
hist(x_2.t, col = "blue")
hist(y_3, col = "pink" )

plot(data.4)

# Part II ##############################################################################


