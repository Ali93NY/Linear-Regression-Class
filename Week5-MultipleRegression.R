# Get data 

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06FI05.txt", header = FALSE)

# Changing the variables to have more meaningful names
names(data)[1]<-paste("targpop")
names(data)[2]<-paste("dispoinc")
names(data)[3]<-paste("sales")
data
attach(data)

# Creating the observations y and the design matrix X
y = sales
(X = cbind(rep(1, nrow(data)), targpop, dispoinc))

# 3D plots
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(targpop, dispoinc, y)

m1 <- lm(y ~ targpop + dispoinc , data = data)
install.packages("rockchalk")
library(rockchalk)
plotPlane(m1, plotx1 = "targpop", plotx2 = "dispoinc")


# Matrix products that occur in the formulae

t(X)%*%X

# Note, for example, element 2,1 is the sum of x_i1:
sum(targpop)

t(X)%*%y

solve(t(X)%*%X)

# Finally, the estimated slope and intercept

b = solve(t(X)%*%X)%*%t(X)%*%y
b

# Interpretation of b1:
# For every 1000 people increase in target population, the sales 
# increase by $1,454.60, keeping income fixed.


# Interpretation of b2:
# For every $1000 increase in disposable income, the sales 
# increase by $9,365.50, keeping target population fixed.


# Check and compare to built-in lm funcion

reg = lm(sales ~ targpop + dispoinc)
summary(reg)

# Fitted values y-hat

y.hat = X%*%b
y.hat

# Alternatively
predict(reg)

# Residuals
e = y - y.hat
e

resid(reg)

# Residual plot
plot(y.hat, e)
# Seems assumptions are ok

# You can also check against specific predictors
plot(targpop, e)
plot(dispoinc, e)

# Checking for interaction effect
plot(targpop*dispoinc, e)
# If you see straight line pattern include the interaction

# Checking normality
par(mfrow = c(2, 2))
boxplot(e)
hist(e)
qqnorm(e)
qqline(e)

par(mfrow = c(1, 1))

# ANOVA

t(y)%*%y
SSE = t(y)%*%y -t(b)%*%t(X)%*%y
SSE
# Check with built-in function (R calls this SSResiduals)
anova(reg)

n = nrow(data)
J = matrix(rep(1,n^2), n, n)
SSTO = t(y)%*%y - t(y)%*%J%*%y/n
SSTO

# Alternatively:
sum(anova(reg)[,2])


MSE = SSE/(n-ncol(data))
MSE

# To get overall F-test and R-squared

summary(reg)
# F-test statistic = 99.1
# p-value = 1.921e-10
# Reject H0: beta1 = beta2 = 0

# Test the significance of each predictor separately:

# Ho: beta1 = 0
# p-value =  2e-06
# Conclusion: Target population is a significant predictor

# Ho: beta2 = 0
# p-value = 0.033
# Conclusion: Disposable income is a significant predictor

# Confidence intervals for betas
confint(reg)
# Can also be obtained manually with the t distribution

# Variances of the coefficients

(s.sq.b = drop(MSE)*solve(t(X)%*%X))

# St. errors of the coefficients
sqrt(diag(s.sq.b))
# Note they match the lm output

# Manual 95% CI for beta1:
n = nrow(data)
coef(reg)[2] - qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]
coef(reg)[2] + qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]

# R-sq = 0.9167
# That is, 91.67% of sales variation is explained by the regression model

# Alternative formula
1 -SSE/SSTO


# Prediction
x.new = c(1, 65.4, 17.6)
s.sq.new = t(x.new)%*%s.sq.b%*%x.new
s.sq.new
# Manual 95% CI for mean response:
coef(reg)%*%x.new - qt(0.975, 18)*sqrt(s.sq.new)
coef(reg)%*%x.new + qt(0.975, 18)*sqrt(s.sq.new)

# Or with built-in function
predict(reg, data.frame(targpop=65.4,dispoinc=17.6), interval = "conf")


# 95% PI
predict(reg, data.frame(targpop=65.4,dispoinc=17.6), interval = "predict")



# Exercise
# Use skin cancer data and fit a model with both latitude and longitude

# a) Obtain the estimated regression equation
# b) Interpret both slopes
# c) Obtain the fitted values and residuals
# d) Perform the overall F-test and state the conclusion
# e) Obtain R2 and interpret its value
# f) Obtain 96% CI's for the slopes
# g) Estimate the expected mortality rate in states with  latitude = 40 and longitude = 70
# h) Obtain a 95% CI for the mean estimated mortality rate in part g)
# i) Obtain 91% prediction limits for new observations with X1 = 40 and X2 = 70

# Read in data

Data <- read.table(file.choose(), header=T)
attach(Data)

# Obtain scatterplot matrix and correlation matrix:

cor(Data[,c(2,3,5)])
pairs(Data)