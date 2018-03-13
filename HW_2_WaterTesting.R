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

####**part 2 plotting data**#####
# Grabbing Data from Water Data Set
# no standard distribution
y.consume <- cbind(consume = Water[,3])
x.income <- cbind(income = Water[,2])
x.rain <- cbind(rain = Water[,4])
x.cost <- cbind(cost = Water[,5])

# plotting data
# Histograms
hist(Water[,2], col="grey")
hist(Water[,3], col="red")
hist(Water[,4], col="blue")
hist(Water[,5], col="green")

#Variables v Consumption
y1 <- plot(x.income, y.consume) #kind of all over the place
y2 <- plot(x.rain, y.consume) #1/x function ish
y3 <- plot(x.cost, y.consume) #linear?

#All at once
data.Water <- data.frame(cbind(income = Water[,2], consume = Water[,3], rain = Water[,4], cost = Water[,5]))

plot(data.Water) #Not really seeing significant trends here

####A: Linear Regression Model####

#Redifining the water data set
data.1 <- data.frame(cbind(x.income, x.cost, x.rain, y.consume))

fullmod <- lm(y.consume~x.income + x.cost + x.rain, data = data.1)
summary(fullmod)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-81.259 -20.499  -0.687  17.350 112.284 

#Coefficients:             (C)
#             Estimate      Std. Error  t value  Pr(>|t|)    
#(Intercept)  (A) 250.9477    41.8117   6.002    3.35e-07 ***
#  x.income   (B)  -4.9158     3.7692  -1.304    0.199    
#  x.cost     (C) -17.7517    10.8043  -1.643    0.108    
#  x.rain     (D)  -1.9342     0.3683  -5.252    4.18e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 36.78 on 44 degrees of freedom
# Multiple R-squared:  0.4436,	Adjusted R-squared:  0.4057 
# F-statistic: 11.69 on 3 and 44 DF,  p-value: 9.21e-06

plot(fullmod, pch = 16, which = 1)

####B: Variation Explained by Regression Model####
#The adjusted R-squared is a modified version of R-squared that has been adjusted for 
#the number of predictors in the model. The adjusted R-squared increases only if the 
#new term improves the model more than would be expected by chance. 
#It decreases when a predictor improves the model by less than expected by chance.

#For our current model, we found 
## Multiple R-squared:  0.4436,	Adjusted R-squared:  0.4057 

#So about 40% of our model makes sense to the computer basically (I Guess????)

####C: Conduct an F-Test####

rmod.1 = lm(y.consume ~ x.rain, data=data.1) # Reduced Model with rain
rmod.2 = lm(y.consume ~ x.income, data=data.1) # Reduced Model with income
rmod.3 = lm(y.consume ~ x.cost, data=data.1) # Reduced Model with cost

rmod.4 = lm(y.consume ~ x.income + x.rain, data=data.1) # Reduced Model with income and rain
rmod.5 = lm(y.consume ~ x.cost + x.rain, data=data.1) # Reduced Model with cost and rain
rmod.6 = lm(y.consume ~ x.income + x.cost, data=data.1) # Reduced Model with income and cost

#fullmode already exists, putting it here for reference purposes
#fullmod <- lm(y.consume~x.income + x.cost + x.rain, data = data.1)

anova(rmod.1,fullmod) #For rain
#Analysis of Variance Table

#Model 1: y.consume ~ x.rain
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS    Df   Sum of Sq  F      Pr(>F)
#1     46 65980                           
#2     44 59538  2    6442.6     2.3806 0.1043 .

#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#The output shows the results of the partial F-test. Since F=2.3806 (p-value=0.1043)
#we cannot reject the null hypothesis at the 5% level of significance.
#It appears that the variables Income and Cost do not contribute significant
#information to the consumption of water once the average rainfall has been taken
#into consideration.


anova(rmod.2,fullmod) #For income
#Analysis of Variance Table

#Model 1: y.consume ~ x.income
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS     Df    Sum of Sq      F      Pr(>F)    
#1     46 103471                                  
#2     44  59538  2     43933          16.234 5.241e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(rmod.3,fullmod) #For cost
#Analysis of Variance Table

#Model 1: y.consume ~ x.cost
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS    Df    Sum of Sq      F       Pr(>F)    
#1     46 99636                                  
#2     44 59538  2     40098          14.817  1.203e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(rmod.4,fullmod) #For income and rain

#Analysis of Variance Table
#Model 1: y.consume ~ x.income + x.rain
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS   Df    Sum of Sq      F P    r(>F)
#1     45 63190                           
#2     44 59538  1    3652.8         2.6995 0.1075


anova(rmod.5,fullmod) #For cost and rain
#Analysis of Variance Table

#Model 1: y.consume ~ x.cost + x.rain
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS    Df   Sum of Sq      F      Pr(>F)
#1     45 61839                           
#2     44 59538  1    2301.6         1.7009 0.1989


anova(rmod.6,fullmod) #For cost and income
#Analysis of Variance Table

#Model 1: y.consume ~ x.income + x.cost
#Model 2: y.consume ~ x.income + x.cost + x.rain
#Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
#1     45 96857                                  
#2     44 59538  1     37320 27.581 4.182e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#This confirms the results of test 1, which indicates that rain is the only variable that 
#has any statistically significant effect on comsumption


####D: Individual Regression Coefficents####
#From the tests above, it is pretty clear that rain is the only variable
#that effects consumption, therefore the full model would be:

updated.model <- lm(y.consume ~ x.rain, data=data.1)
summary(updated.model) 
#Residuals:
#  Min      1Q  Median      3Q     Max
#-72.794 -24.043  -3.982  17.350 127.515
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 179.2406    13.9638  12.836  < 2e-16 ***
#  x.rain       -2.0155     0.3769  -5.348 2.71e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 37.87 on 46 degrees of freedom
#Multiple R-squared:  0.3834,	Adjusted R-squared:   0.37
#F-statistic:  28.6 on 1 and 46 DF,  p-value: 2.71e-06


plot(updated.model)


####Takeaways and Suggestions####
#With the current data, the only thing that you can see has any effect on the consumption
#of water is the average rainfall.

#Some question I have about the data are when was it collected? Where was the data sourced?
#Can we add the population of the state and total square 


