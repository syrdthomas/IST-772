#######################################################################
#
# Author: DeAndre Thomas
# Professor: Dr. Anastasopoulos
# Purpose: Homework 6
# Class: IST 772
# Date: 05/20/2022
#
#######################################################################

# Question 1

View(InsectSprays)

# The dependent variable is the number of insects killed by the spray. The 
# independent variable is the type of spray used. There are 72 observations. 

#######################################################################

summary(aov(InsectSprays$count ~ InsectSprays$spray))

# Question 2

# The Residuals is the within-group variance. The between group variance is
# the first line on the ANOVA report, and is typically a larger value than
# the second line of the report, the within-group variance. This is because
# the between group variance takes in account of the each group's mean 
# together, while the within-group takes in account for each groups mean
# individually. 

#######################################################################

# Question 3

# Between Group MnSq =  533.8   Within Group MnSq = 15.4
533.8/15.4

# F-Ratio = 34.66

# I would reject the null hypothesis, because in order to fail to reject the 
# null hypothesis, the F-Ratio would be closer to 1. Here, the F-ratio is 
# nearly 35.Also, the p value < 0.001, meaning that we should reject the 
# null hypothesis. 

#######################################################################

# Question 4

# The degrees of freedom between groups is 5 and the degrees of freedom 
# within-groups is 66. The sum of these two values is 71; there are 72 
# observations. The reason this is so because (the total value of) 
# degrees of freedom is equal to the number of observations minus 1.

#######################################################################

# Question 5

insectResults<- summary(aov(InsectSprays$count ~ InsectSprays$spray))
insectResults

# For the between group, the degrees of freedom is 5, the sum of squares is 
# 2669, and the mean of squares is 533.8. For the within-group, the degrees 
# of freedom is 66, the sum of squares is 1015, and the mean of squares is
# 15.4. The F-Value is 34.7, and the p-value is less than .001 (very low, 
# thus we would reject the null hypothesis).
# H0: Mu1 = Mu(of sprays A-F)  Ha: Mu1 (does not equal) =/ Mu(of sprays A-F)

#######################################################################

# Question 6

install.packages("BayesFactor")
library(BayesFactor)

ISBF<- anovaBF(count ~ spray, data = InsectSprays)
# This analysis shows odds of 1.506e+14:1 in favor of the alternative 
# hypothesis. This is a very strong indication to reject the null hypothesis. 

summary(posterior(ISBF, iterations = 1000))
# This tells us that, in respect to the grand mu, sprays A, B and F are t
# he most effective, while sprays C, D, and E are the least effective; 
# Spray F is most effective, and spray C is least effective. 

#######################################################################

# Question 7
library(BEST)

plot(BESTmcmc(InsectSprays[InsectSprays$spray=="C",1]
         , InsectSprays[InsectSprays$spray=="F",1] ))

# This analysis concludes that the mean value of both groups are 100% less 
# than 0, with 95% certainty that the mean is -14.5. The HDI of 
# these two groups are between -19 and -10.2. 