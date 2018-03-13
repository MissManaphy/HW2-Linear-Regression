# Remove all extraneous variables
rm(list = ls())
graphics.off()

mse <- function(sm) {
  m <- mean(sm$residuals^2)
  return(m);
}

# Part I #################################################################################
# Importing packages
install.packages("Metrics")
library("Metrics")

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

y <- true_beta_0 + true_beta_1*x_1 + true_error ##Linear model depending only on x_1
#What we're trying to recover

hist(y) 
plot(x_1, y, pch = 20, col ="red") 

# Checking the density of the repsonse variable
plot(density(y), main="Density Plot: Y", ylab="y") 

# Building a regression model, and finding estimated beta values
mod1 <- lm(y~x_1)
summary(mod1)

#Summary shows intercept and coefficents of x_1 estimate, which are beta0 and beta1
#Coefficients:(these ones)
#               Estimate  Std. Error t value  Pr(>|t|)    
#  (Intercept)  1.234657   0.078525   15.72   <2e-16 ***
#  x_1         -8.200803   0.008841 -927.62   <2e-16 ***

plot(mod1, pch = 16, which = 1)

# Declaring second variable, and finding linear distributions
x_2 <- rgamma(1000, 2, 10) ##gamma distribution is left heavy instead of center heavy
hist(x_2, col="blue")

y_2 <- true_beta_0 + true_beta_1*x_1 + true_beta_2*x_2 + true_error

# fitting a model depending only on x_1 & plotting root mean square error
mod2 <- lm(y_2~x_1)
summary(mod2)

#Coefficients:                 (C)
#             Estimate Std.    Error     t value   Pr(>|t|)    
#(Intercept)  (A) (1.839487)   0.080625   22.82    <2e-16 ***
#  x_1        (B) (-8.201836)  0.009077  -903.57   <2e-16 ***

#YP1 is our way of seeing how significantly each variable contributes to the model
#Starting in this case with x_1

#         (A)                    (B)                        (C)
y_p_1 <- mod2$coefficients[1] + mod2$coefficients[2]*x_1 + mod2$residuals
rmse_data <- c() #pre-declared vector for root mean squared error

set.seed(100) ##for random number generators

for (i in seq(10,1000,20)) { ##10 to 1000 with 20 as the step size --> 10, 30, 50, etc...
  idxs <- sample(1:1000, as.integer(i)) #extracts random numbers between 1 and 1000 
  
  obsY <- y_2[idxs] #take the values out of the original y_2
  simY <- y_p_1[idxs] #take those out of our y_prime_1 model

  rmse_data <- c(rmse_data, rmse(obsY, simY))
  #RMSE is a pre-defined function that we have access to with a specific package
  #the RMSE generally gives you the numerical difference between the actual and predicted values
    
}

# Plotting the RMSE for x_1
#this is just putting it all in a data frame and plotting it out
tb <- data.frame(t(rbind(seq(10,1000,20), rmse_data)))
plot(tb, main = "Root Mean Square Error Plot of x_1")
lines(tb, col = "blue" ) 
#The plot itself tells us how much error you see with each random sampling of data

# fitting a model depending only on x_2
mod3 <- lm(y_2~x_2)
summary(mod3)

y_p_2 <- mod3$coefficients[1] + mod3$coefficients[2]*x_2 + mod3$residuals
rmse_data.1 <- c()

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_2[idxs]
  simY <- y_p_2[idxs]
  
  rmse_data.1 <- c(rmse_data.1, rmse(obsY, simY))
  
}

# Plotting the RMSE for x_2
tb.1 <- data.frame(t(rbind(seq(10,1000,20), rmse_data.1)))
plot(tb.1, main = "Root Mean Square Error Plot of x_2")
lines(tb.1, col = "red" )


# finding a model dependant on both x_1 and X_2
mod4 <- lm(y_2~x_1 + x_2)
summary(mod4)

y_p_3 <- mod4$coefficients[1] + mod4$coefficients[2]*x_1 + mod4$coefficients[3]*x_2 + mod4$residuals
rmse_data.2 <- c()

for (i in seq(10,1000,20)) {
  idxs <- sample(1:1000, as.integer(i))
  
  obsY <- y_2[idxs]
  simY <- y_p_3[idxs]
  
  rmse_data.2 <- c(rmse_data.2, rmse(obsY, simY))
  
}

# Plotting the RMSE for x_1 and x_2 
tb.2 <- data.frame(t(rbind(seq(10,1000,20), rmse_data.2)))
plot(tb.2, main = "Root Mean Square Error Plot of both x_1 and x_2")
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


