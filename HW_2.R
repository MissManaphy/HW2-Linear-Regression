# Remove all extraneous variables
rm(list = ls())
graphics.off()

mse <- function(sm) {
  m <- mean(sm$residuals^2)
  return(m);
}

# Importing packages
install.packages("Metrics")
library("Metrics")


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

# fitting a model depending only on x_1 & plotting root mean square error
mod2 <- lm(y_2~x_1)
summary(mod2)

y_p_1 <- mod2$coefficients[1] + mod2$coefficients[2]*x_1
rmse_data <- c()

set.seed(100)

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_2[idxs]
  simY <- y_p_1[idxs]

  rmse_data <- c(rmse_data, rmse(obsY, simY))
    
}

# Plotting the RMSE for x_1
tb <- data.frame(t(rbind(seq(10,1000,20), rmse_data)))
plot(tb, main = "Root Mean Square Error Plot of x_1", xlab = "Sample size", 
     ylab = "RMSE")
lines(tb, col = "blue" ) 


# fitting a model depending only on x_2
mod3 <- lm(y_2~x_2)
summary(mod3)

y_p_2 <- mod3$coefficients[1] + mod3$coefficients[2]*x_2 
rmse_data.1 <- c()

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_2[idxs]
  simY <- y_p_2[idxs]
  
  rmse_data.1 <- c(rmse_data.1, rmse(obsY, simY))
  
}

# Plotting the RMSE for x_2
tb.1 <- data.frame(t(rbind(seq(10,1000,20), rmse_data.1)))
plot(tb.1, main = "Root Mean Square Error Plot of x_2", xlab = "Sample Size",
     ylab = "RMSE")
lines(tb.1, col = "red" )


# finding a model dependant on both x_1 and X_2
mod4 <- lm(y_2~x_1 + x_2)
summary(mod4)

y_p_3 <- mod4$coefficients[1] + mod4$coefficients[2]*x_1 + mod4$coefficients[3]*x_2 
rmse_data.2 <- c()

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_2[idxs]
  simY <- y_p_3[idxs]
  
  rmse_data.2 <- c(rmse_data.2, rmse(obsY, simY))
  
}

# Plotting the RMSE for x_1 and x_2 
tb.2 <- data.frame(t(rbind(seq(10,1000,20), rmse_data.2)))
plot(tb.2, main = "Root Mean Square Error Plot of both x_1 and x_2", 
     xlab = "Sample Size", ylab = "Root Mean Square Error")
lines(tb.2, col = "pink" )

# separating the new data set 
data.1 <- data.frame(cbind(x_1, x_2, y_2))


# Delaring new variable z 
z <- x_1^2

y_3 <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_beta_3*z+ true_error

# declaring a model based on x_1, AND z only
mod5 <- lm(y_3~x_1)
summary(mod5) 

mod6 <- lm(y_3~x_1 + z)
summary(mod6)

# finding the sample mean 
y_p_4 <- mod6$coefficients[1] + mod6$coefficients[2]*x_1 + mod6$coefficients[3]*z
rmse_data.3 <- c()

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_3[idxs]
  simY <- y_p_4[idxs]
  
  rmse_data.3 <- c(rmse_data.3, rmse(obsY, simY))
}

# Plotting the RMSE for x_1 and x_2 
tb.3 <- data.frame(t(rbind(seq(10,1000,20), rmse_data.3)))
plot(tb.3, main = "Root Mean Square Error Plot of both x_1 and z",
     xlab = "Sample Size", ylab = "RMSE")
lines(tb.3, col = "pink" )


# TEST CHANGE
# Playing around with data--just change the values in this section
# Then run only this section and record the results in the report
true_error.t <- rlnorm(1000, 0, 2)
true_beta_0.t <- -5.5
true_beta_1.t <- -2
true_beta_2.t <- -4.1

x_1.t <- rgamma(1000, 2) 
x_2.t <- rexp(1000)

y.t <- true_beta_0.t + true_beta_1.t*x_1.t + true_beta_2.t*x_2.t+ true_error.t

data.4 <- data.frame(cbind(x_1.t, x_2.t, y.t))

mod7 <- lm(y.t~x_1.t + x_2.t, data = data.4)
summary(mod7)

plot(mod7, pch = 16, which = 1)

# plotting histograms and scatterplots (just for part d)
hist(x_1.t, col="red")
hist(x_2.t, col = "blue")
hist(y.t, col = "pink" )


plot(data.4)
