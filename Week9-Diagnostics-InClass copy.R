# Get data from Table 10.1 on p. 387

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2010%20Data%20Sets/CH10TA01.txt", header = FALSE)

data
colnames(data) = c("X1", "X2", "Y")
attach(data)

# Added variable plot of X1 controlling for X2
# Fitting regression with y on X2

regYX2 = lm(Y ~ X2)
summary(regYX2)

# Fitting regression with X1 on X2

regX1X2 = lm(X1 ~ X2)
summary(regX1X2)

# Added-variable plot
eYX2 = resid(regYX2)
eX1X2 = resid(regX1X2)
plot(eX1X2, eYX2)
# Conclusion: X1 is usefule predictor after including X2

# Fitting regression of eYX2 vs. eX1X2 
regres = lm(eYX2 ~ eX1X2)
summary(regres)
abline(regres)
# Note slope is 6.288
# The slope should be the same as b1 in Y ~ X1 + X2

# Fitting multiple regression to check coef. b1

reg = lm(Y ~ X1 + X2)
summary(reg) 
# It is the same as the residual regression!


# Added variable plot with car package
install.packages("car")
library(car)
avPlots(reg)


# Residuals

# Unstandardized
e = resid(reg)


# s can be obtained from the summary
summary(reg)$s

# Semi-studentized
e/summary(reg)$s
# MSE = 160.2756 = 12.66^2

# Obtaining the variances of the residuals
X = cbind(rep(1, nrow(data)), X1, X2)
H = X%*%solve(t(X)%*%X)%*%t(X)
Var.e = ((summary(reg)$s)^2)*(rep(1,18)-diag(H))

# Studentized manually
e/sqrt(Var.e)

# Studentized with built-in function
rstandard(reg)

# Deleted residuals
rstudent(reg)

# Not applying Bonferroni, cut off point is
nrow(data)
# 18
# df = n - p - 1 = 18 - 3 - 1 = 14
qt(0.975, 14)
# [1] 2.144787

# Conclusion: two outliers, 3rd and 7th observation

# Built-in function from car package:
outlierTest(reg)

# Alternative
library(MASS)
studres(reg)


# Leverage points
# Diagonal elements of H matrix
diag(H)
#  [1] 0.06928999 0.10064451 0.18901274 0.13157726 0.07559158 0.34985551
#  [7] 0.62250833 0.13187873 0.06575455 0.10052380 0.12011384 0.29940207
# [13] 0.09441512 0.20960495 0.09569345 0.07752426 0.18175654 0.08485276

# Cut off point = 2*(3/18) = 0.3333

# Observations 6 and 7 are leverage points
# However, 6th observation is barely making it above the cut off point
# 12th observation is almost making the cut off point.

# We can see this on the scatterplot of X1 vs. X2
plot(X1, X2)

# Built-in function for influential observations
influence.measures(reg)

# Cook's distance cut-off point is
 qf(0.5, 3, 15)
# [1] 0.8256845

# 7th observation is clearly influential

# Cut-off point for DFFITS is 
 2*sqrt(3/18)
# [1] 0.8164966

# Observations 3 and 7 are influential

# Cut-off point for DFBETAS is
 2/sqrt(18)
# [1] 0.4714045

# Influencing b1: 3rd and 7th observations
# Influencing b2: 6th, 7th and 14th observations

# Overall: remove 7th for sure, maybe 3rd and 6th

# Remove the 7th obs and refit the model:
 summary(lm(Y ~ X1+X2, data = data[-7,]))
 
# VIF
library(car) 
vif(reg) 

#       V1       V2 
# 1.069249 1.069249 

# With only two predictors both VIF are always the same!
# In general, VIF > 5 is problematic

# In our case no reason for concern
# Also we can check
cor(X1, X2)
# [1] 0.254488
# it is small


# Excercise:
# Obtain the data from Problem 6.18:
# X1 = age of property
# x2 = operating expenses
# X3 = vacancy rate
# X4 = area in square feet
# Y = rental rate

data1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR18.txt", header = FALSE)
colnames(data1) = c("Y", "X1", "X2", "X3", "X4")
data1

#1. Obtain correlation matrix plot and inspect visually if there are strong associations between the predictors
#2. Obtain the VIF factors and decide if you should exclude any predictors and remove them from the dataset
#3. Working with the dataset from part 2 obtain the studentized deleted residuals 
# and identify outliers using alpha = 0.1. Remove any outliers
#4. Using the dataset from part 3 obtain the hat matrix and identify any outlying X observations
#5. Using the dataset from part 3 obtain the raw residuals and plot them against Y-hat, and each predictor variable, 
# and each interaction term. Based on the graphs, should any modifications of the model be made?

#1.

#2.


#3. 

#4. 