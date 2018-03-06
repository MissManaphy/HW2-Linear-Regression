#remove all variables
rm(list = ls())

#clear screen
cat("\014")

#close graphic windows
graphics.off()

#Remember to set your working directory to the one the data is stored
# in before running the rest of the code
setwd("~/Desktop/2017-2018/Second Semester/Intro to Data Science/Labs/HW2-Linear-Regression")
##Run this line!

Water <- read.table("~/Desktop/2017-2018/Second Semester/Intro to Data Science/Labs/HW2-Linear-Regression/Water.txt", header=TRUE, quote="'", comment.char="")

summary(Water)
#For each state the researchers measured the per capita consumption of water 
#(in gallons per day), the per capita income (in $1000), 
#the average annual rainfall (in inches) 
#and the average cost of 1000 gallons of water (in dollars)



####**part 1 plotting data**#####
# plotting data
hist(Water[,2], col="grey")
hist(Water[,3], col="red")
hist(Water[,4], col="blue")
hist(Water[,5], col="green")

data.Water <- data.frame(cbind(income = Water[,2], consume = Water[,3], rain = Water[,4], cost = Water[,5]))

plot(data.Water) #Not really seeing significant trends here

y.consume <- cbind(consume = Water[,3])
x.income <- cbind(income = Water[,2])
x.rain <- cbind(rain = Water[,4])
x.cost <- cbind(cost = Water[,5])

y1 <- plot(x.income, y.consume) #kind of all over the place
y2 <- plot(x.rain, y.consume) #1/x function ish
y3 <- plot(x.cost, y.consume) #linear?

data.1 <- data.frame(cbind(x.income, y.consume))
data.2 <- data.frame(cbind(x.rain, y.consume))
data.3 <- data.frame(cbind(x.cost, y.consume))

# Building a regression model, and finding estimated beta values
mod1 <- lm(y.consume~x.income, data = data.1)
summary(mod1)

mod2 <- lm(y.consume~x.rain, data = data.2)
summary(mod2)

mod3 <- lm(y.consume~x.cost, data = data.3)
summary(mod3)

# estimate_beta_0 = 1.06997
# estimate_beta_1 = -8.19802

plot(mod1, pch = 16, which = 1)
plot(mod2, pch = 16, which = 1)
plot(mod3, pch = 16, which = 1)







