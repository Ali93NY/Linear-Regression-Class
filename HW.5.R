# Q 7.3 pg. 289

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR05.txt")
data

names(data)[1] <- paste("Y")
names(data)[2] <- paste("X1")
names(data)[3] <- paste("X2")

attach(data)

# a)
reg12 <- lm(Y ~ X1 + X2)
anova(reg12)

# Analysis of Variance Table

# Response: Y
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# X1         1 1566.45 1566.45 215.947 1.778e-09 ***
# X2         1  306.25  306.25  42.219 2.011e-05 ***
# Residuals 13   94.30    7.25                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# SSR (X2| X1) = 306.25
# SSR (X1, X2) = 1566.45 + 306.25 = 1872.7

# b)
# Ho: b2 = 0
# Ha: b2 is significant 
# F-value = 42.219
# P-value = 2.011e-05
# Reject Ho, and therefore b2 is significant 
# X2 cannot be dropped from the regression model in addition to X1


# Q 7.12 p.290

# a)
reg1 <- lm(Y ~ X1)
anova(reg1)
# SSR = 1566.45
# SSE = 400.55
# R^2 (Y1) = 1566.45/(1566.45 + 400.55) = 0.7964

rsq.partial(reg1) # to double check
# R^2 (Y1) = 0.796365
# the proportion of variance for Y that's explained by X1

# b) 
reg2 <- lm(Y ~ X2)
rsq.partial(reg2)
# R^2 (Y2) = 0.155694
# the proportion of variance for Y that's explained by X2

# c)
reg <- lm(X1 ~ X2)
rsq.partial(reg)
# R^2 (X1|X2) = 0
# the proportion of variance of X1 when X2 in the model

# d)
reg21 <- lm(Y ~ X2 + X1)
anova(reg21)
# SSR(X1| X2) = 1566.45
anova(reg2)
# SSE(X2) = 1660.75 
# R^2( Y X1|X2) = SSR(X1| X2)/ SSE(X2) = 0.9432184
rsq.partial(reg21, reg2) # To double check
# 0.9432184
# the cofficient of partial determination between Y and X1, when X2 in the model

# e)
reg12 = lm (Y ~ X1 + X2)
anova(reg12)
# SSR(X2|X1) = 306.25
anova(reg1)
# SSE(X1) = 400.55
# R^2(Y , X2| X1) = 0.7646
rsq.partial(reg12, reg1) # To double check
# 0.7645737
# the cofficient of partial determination between Y and X2, when X1 in the model

# f) 
summary(reg12)
# R-squared:  0.9521
# the proportion of varaince for Y that's explained by X1 and X2. 

# Q. 8.16 P. 337

data.1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt")
data.1
data.2 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%208%20Data%20Sets/CH08PR16.txt")
data.2

data <- cbind(data.1,data.2)

names(data)[1] <- paste("Y")
names(data)[2] <- paste("X1")
names(data)[3] <- paste("X2")

attach(data)
data

# a)
# E{Y} = B0 + B1*X1 + B2*X2 + e 
# E{Y} : the linear function
# B0 : y-intercept 
# B1 : for every unit increase in X1, Y is increased by B1, keeping B2 fixed.
# B2 : How much higher (lower) the mean response line is for the class coded 1
# than the line for the class coded 0, for any given level of X1.


reg <- lm(Y ~ X1 + X2)
summary(reg)


# b)
# Estimated regression function:
# Y = 2.19842 + 0.03789 X1 - 0.09430 X2

# c) 
# Ho: B2 is not significant 
# Ha: B2 is significant
# since the p-value of X2 is 0.43341 > 0.01
# We reject Ha 
# X2 is not significant and can be dropped

# d)
plot(X1*X2 ,resid(reg))
# Yes, because there is a linear trend that can be seen 


# 8.20 P. 339
# a)
reg1 <- lm(Y ~ X1 + X2 + X1:X2)
summary(reg1)

# Y = 3.2263 - 0.00276 X1 - 1.6495 X2 + 0.062245 X1*X2

# b)
# Ho: B3 = 0
# Ha: B3 != 0 
# interaction P-value = 0.0205
# Since the interaction p-value is 0.0205 < 0.05
# We reject Ho 
# the interaction effect is significant

# Given X2 = 0 ( undeclared)
# Y = 3.2263 - 0.00276 X1 
# GPA decreased by 0.00276 when the major is undeclared  

# Given X2 = 1 ( declared)
# Y = 3.2263 - 0.00276 X1 - 1.6495 (1) + 0.062245 X1*(1) 
# Y = 1.577 + 0.0594 X1
# GPA increased by 0.0594 when the major is declared

