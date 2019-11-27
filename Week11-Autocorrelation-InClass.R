# Generate data similar to Table 12.1 on p. 482

# Initializing the error term

# Sample size
n = 100

# Lag 1 autocorrelation
rho = 0.8

# Error terms
e = 0:n
u = 0:n

e[1] = 3
u[1] = 0
u[2:(n+1)] = rnorm(n)

# Generate the correlated errors
for (i in 2:(n+1)) e[i] = rho*e[i-1] + u[i]
e
ts.plot(e, type = "b")

# Similarly, autocorrelated series can be generated with the arima.sim function
arima.sim(model=list(ar=0.8), n = 100)

# Regression coefficients:
b0 = 2
b1 = 0.5
x = 0:n
y = b0 + b1*x + e
y
plot(x,y)
summary(lm(y~x))

# Estimate the autocorrelation
acf(e)$acf


# Durbin-Watson test
# install.packages("lmtest")
library(lmtest)
dwtest(y ~ x)

# Note residuals mimic the autocorrelation of the simulated errors
r= resid(lm(y~x))
acf(r)$acf # if asked to find row


# Manual computation of DW statistic
numerator = 0
for (i in 2:(n+1)) numerator = numerator + (r[i]-r[i-1])^2
(D = numerator/sum(r^2))

# Seems to work on simulated data, so let's try a real data example



# Example p. 488

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2012%20Data%20Sets/CH12TA02.txt", header = FALSE)

data
names(data)[1]<-paste("Y")
names(data)[2]<-paste("X")
attach(data)

reg = lm(Y~X)
summary(reg)
r = resid(reg)

# Time series plot of residuals
ts.plot(r,type="b")
abline(h = 0)

dwtest(Y~X)
# Conclusion: there is strong evidence of autocorrelation!


# Fixing the issue with Cochrane-Orcutt method

# Estimate rho 
# Here we do NOT follow the rho formula from the book!
# Instead, easier:

acf(r, lag.max = 1)$acf

# Estimated rho = 0.626

# If you want to follow book's method:
# Manual estimation of rho
numerator = 0
n = nrow(data)
for (i in 2:n) numerator = numerator + r[i]*r[i-1]
rho2 = numerator/sum(r[2:n]^2)
rho2
# Same as on p. 493 of the textbook!

# Compute transformed variables:
n = nrow(data)
Yprime = 1:n
for (i in 2:n) Yprime[i] = Y[i]- rho2*Y[i-1]
Yprime = Yprime[-1]

Xprime = 1:n
for (i in 2:n) Xprime[i] = X[i]- rho2*X[i-1]
Xprime = Xprime[-1]

Regprime = lm(Yprime ~ Xprime)
summary(Regprime)

# Notice no more autocorrelation!
dwtest(Yprime ~ Xprime)

# Back to original model:
b0 = Regprime$coef[1]/(1-rho2)
b1 = Regprime$coef[2]

# SE(b1) same as SE(b1.prime)
summary(Regprime)$coef[4]
# Compare to OLS
summary(reg)$coef[4]


# Alternatively, use package orcutt

install.packages("orcutt")
library(orcutt)

cochrane.orcutt(reg, convergence = 0)

# Depends on R version to use convergence = 
cochrane.orcutt(reg)


# Alternative method: use the function arima to fit a time series model:
(fit2 <- arima(Y, xreg = X, order=c(1,0,0)))


# Forecasting

# Y.hat = -1.078293 + 0.1738209*X
# Obtain last residual

(e20 = Y[20]-(b0 + b1*X[20]))


# X21 = 175.3
# Obtain Y.hat

(Y.hat21 = b0 + b1*175.3)

# Adjust with correlated residual
(F21 = Y.hat21 + rho2*e20)


# Exercise (12.9 on p. 502)
# A staff analyst for a manufacturer of microcomputer components has compiled monthly data for the past 16 months 
# on the value of industry production of processing that use components (X, in million dollars) and the value of 
# the firm's components used (Y. in thousand dollars). The analyst believes that a simple linear regression relation
# is appropriate but anticipates positive autocorrelation. 

# a) Fit a simple linear regression model by ordinary least squares and obtain the residuals. 
# Also obtain s(b0) and s(b1). 
# b) Plot the residuals against time and explain whether you find any evidence of positive autocorrelation.
# c) Conduct a formal test for positive autocorrelation
# d) Obtain a point estimate of the autocorrelation parameter
# e) Use one iteration to obtain the estimates beta prime. Also obtain s{b.prime}
# f) Test whether any positive autocorrelation remains 
# g) Restate the estimated regression function obtained in part (b) in terms of the original variables. Also obtain s{b0} and s{b1}. 


data2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2012%20Data%20Sets/CH12PR09.txt", header = FALSE)

names(data2)[1]<-paste("Y")
names(data2)[2]<-paste("X")
attach(data2)

# a)
reg <- lm(Y~X)
summary(reg)
reg$coefficients
# (Intercept)           X 
# -7.738516   53.953316 

# s(b0) = 7.175
# s(b1) = 3.520

# b)
r = resid(reg)

ts.plot(r, type = "b")
# the time seires exhibit autcorrelation movement of ups and downs 

# c)
dwtest(Y~X)
# the p-value = 0.002502
# conclusion:  reject the null hypthesis

# d)

acf(r, lag.max = 1)$acf
# 0.5273293

# e)
cochrane.orcutt(reg, convergence = 0)

n = nrow(data2)
Yprime = 1:n
for (i in 2:n) Yprime[i] = Y[i]- 0.578411*Y[i-1]
Yprime = Yprime[-1]

Xprime = 1:n
for (i in 2:n) Xprime[i] = X[i]- 0.578411*X[i-1]
Xprime = Xprime[-1]

Regprime = lm(Yprime ~ Xprime)
summary(Regprime)


# d)
b0 = Regprime$coef[1]/(1-0.527)
b1 = Regprime$coef[2]
b0
b1


