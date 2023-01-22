#######################################################################
#
# Author: DeAndre Thomas
# Professor: Dr. Anastasopoulos
# Purpose: Homework 4
# Class: IST 772
# Date: 05/06/2022
#
#######################################################################

# Question 7

PlantGrowth
View(PlantGrowth)

summary(PlantGrowth)
# The output of 'summary(PlantGrowth)' shows that there are two columns of 
# data; 'weight' and 'group'. The 'group' column, per the summary function, 
# shows three categories control ('ctrl'), treatment 1('trt1'), and 
# treatment 2('trt2'), all having 10 items each. The 'weight' column, per the 
# summary function, shows the minimum value, 1st quartile, 2nd quartile 
# (median), 3rd quartile, and maximum value. 


hist((PlantGrowth$weight[PlantGrowth$group=="ctrl"])
     , main = "Control Group Histogram")
hist((PlantGrowth$weight[PlantGrowth$group=="trt1"])
     , main = "Treatment Group 1 Histogram")
hist((PlantGrowth$weight[PlantGrowth$group=="trt2"])
     , main = "Treatment Group 2 Histogram")

# The control group shows more of a normal distribution of values. Treatment
# group 1 demonstrates right skewed distribution. Treatment group 2 
# demonstrates normal distribution of values, as well. We can also see that
# the minimum, maximum, mode, and median values of these subsets are different. 

#######################################################################

# Question 8

boxplot(PlantGrowth$weight[PlantGrowth$group=="ctrl"]
        , main = "Control Group Boxplot")
boxplot(PlantGrowth$weight[PlantGrowth$group=="trt1"]
        , main = "Treatment Group 1 Boxplot")
boxplot(PlantGrowth$weight[PlantGrowth$group=="trt2"]
        , main = "Treatment Group 2 Boxplot")

# The mean value of the test group 2 is the highest at 5.4, with the 
# treatment group 1 at ~4.5-4.6, and the control group at ~5.1. Test group 1
# also has an outlier that is displayed on the boxplot. 

#######################################################################

# Question 9

t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"]
       , (PlantGrowth$weight[PlantGrowth$group=="trt1"]))

# The 95% confidence interval is [5.032, 4.661]. This means that there is a 
# 95% chance that the population mean of the control and treatment 1 group 
# will fall within the values of this confidence interval. 

#######################################################################

# Question 10

t.test(PlantGrowth$weight[PlantGrowth$group=="ctrl"]
       , (PlantGrowth$weight[PlantGrowth$group=="trt2"]))

# The 95% confidence interval is [5.032, 5.526]. This means that there is a 
# 95% chance that the population mean of the control and treatment 2 group 
# will fall within the values of this confidence interval. 



