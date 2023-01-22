#######################################################################
#
# Author: DeAndre Thomas
# Professor: Dr. Anastasopoulos
# Purpose: Homework 8
# Class: IST 772
# Date: 06/03/2022
#
#######################################################################

# Question 1

myCars<- data.frame(mtcars[,1:6])

#######################################################################

# Question 2

cor(myCars)

# The weight variable would be best predictor of mpg as it has the 
# strong (negative/inverese) correlation of -0.868. 

#######################################################################

# Question 3

lmOut<- lm(mpg~wt+hp, data = myCars)
lmOut
summary(lmOut)

# The R-Squared value is 0.815. A R-Squared value as such would be 
# considered a significantly strong one. The weight, as we 
# estimated before, has the strongest correlation to miles per
# gallon, as it has a coefficient value (B-weight value) of
# -3.878. , while the horsepower has a one of -0.032. 

#######################################################################

# Question 4

coeffs<- coefficients(lmOut)
coeffs

wt<- 3
hp<- 110 
mpg1<- coeffs[1] + (coeffs[2] * wt) + (coeffs[3] * hp)
mpg1

myCars1<- data.frame(wt = 3, hp = 110)
predict(lmOut, myCars1)

# Per the new linear model, if a car weight 3 tons and this car has 
# a horsepower of 110, the predicted miles per gallon value would
# be 22.1 miles per gallon. 

#######################################################################

# Question 5

library(BayesFactor)

lmMCMCout<- lmBF(mpg~wt+hp, data = myCars)
summary(lmMCMCout)

# The Bayes Factor for linear model prediction finds that there is 
# significant odds (788547604 ±0) in favor of weight and horsepower 
# predicting the miles per gallon of a vehicle. 

#######################################################################

# Question 6

lmMCMCout1<- lmBF(mpg~wt+hp, data = myCars
                  , posterior = TRUE, iterations = 10000)
summary(lmMCMCout1)

# Due to the quantile values for weight and horsepower being less
# than 0, and never greater, we can further conclude that  both 
# variables are significant variables in predicting miles per 
# gallon. 

#######################################################################

# Question 7

install.packages("car")
library(car)
?vif()

# The Variance Inflation Factor (VIF) allows one the measure the 
# the amount of multicollinearity between predictor variables in 
# linear model. Multicollinearity is when when (independent) 
# varibales have high correlation, and when used in a linear model,
# the results can be skewed, unreliable, and/ or undesirable. 
# Using VIF will help prevent the use of highly correlated 
# independent variables. 

#######################################################################

# Question 8

vif(lmOut)

# The ideal model with low multicollinearty would have VIF values 
# no higher than 5-10. In the above model, the VIF of both 
# variables, weight and horsepower have VIF values of 1.767. Thus,
# these variables are not highly correlated. 

# colnames(myCars)
vif(lm(mpg~cyl+disp+hp+drat+wt, data = myCars))

# In the above model, the varibales with the highest VIF values
# are cyl, with a value of 7.869, and disp, with a value of 
# 10.464. Thus, those variables should not be included when 
# creating a linear model of this data set; those variables could
# create skewed, unreliable, and/or undesirable results. 




