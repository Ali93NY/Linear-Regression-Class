data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09PR15.txt")

names(data)[1] <- paste("Y")
names(data)[2] <- paste("X1")
names(data)[3] <- paste("X2")
names(data)[4] <- paste("X3")

attach(data)

# a)
pairs(data)
cor(data)
# The scatter plot suggests that each predictor variable (X1,X2,X3) is lineary associated
# with the (Y) with X1 showing the highest degree of association and X3 the lowest. 
# The scatter plot matrix also indicates intercorrelation among the predictor variables. 


# b) 

reg <- lm(Y ~ ., data = data)
summary(reg)

# Yes, all three predictor variables are significant 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  120.0473    14.7737   8.126 5.84e-09 ***
#  X1          -39.9393     5.6000  -7.132 7.55e-08 ***
#  X2           -0.7368     0.1414  -5.211 1.41e-05 ***
#  X3            0.7764     0.1719   4.517 9.69e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# c) 
install.packages("bestglm")
library(bestglm)
X1.sq = X1^2 
X2.sq = X2^2 
X3.sq = X3^2 
X1X2 = X1*X2
X1X3 = X1*X3
X2X3 = X2*X3
all.predictors <- data.frame(cbind(X1,X2,X3,X1.sq,X2.sq,X3.sq, X1X2,X1X3,X2X3, Y))

bs1 <- bestglm(Xy = all.predictors, 
               family = gaussian,
               IC = "BIC")


names(bs1)

bs1$Subsets
# The best model is with subest (X1, X2, X3, X1X2) with lowest BIC = 170.2278.
# and the second best model with subset (X1, X2, X3, X3.sq, X1X2 ) with 
# the second lowest BIC = 171.2267.

# to plot BIC values 
plot(0:9, bs1$Subsets$BIC, type = "b", ylab = "BIC",
     xlab = "Number of Predictors", lwd = 3, pch = 19, main = "BIC", cex.main = 2)
# lowest BIC = 170.2278
# highest BIC = 188.8129

bs1$BestModel
# 1st model coefficients with subset (X1, X2, X3, X1X2):
# Coefficients:
#  (Intercept)           X1           X2           X3         X1X2  
#     181.6978     -95.5593      -1.7536       0.7951       0.8620 

# 2nd model coefficients with subset (X1, X2, X3, X3.sq, X1X2 )
best2 <- lm(Y ~ X1 + X2 + X3 + X3.sq + X1X2)
best2$coefficients

# (Intercept)           X1           X2           X3        X3.sq         X1X2 
# 103.97279806 -93.35746856  -1.64121863   2.77202222  -0.01299075   0.80432836 
  
# d)
bs2 <- bestglm(Xy = all.predictors,
               family = gaussian,
               IC = "AIC")

bs2$Subsets
# The best model is with subsete (X1,X2,X3,X3.sq,X1X2) with lowest AIC = 163.7442. 
# and the second best is with subset (X1, X2, X3, X1X2) with the second
# lowest AIC = 164.2418.

# To plot BIC values:
plot(0:9, bs2$Subsets$AIC, type = "b", ylab = "AIC",
     xlab = "Number of Predictors", lwd = 3, pch = 19, main = "AIC", cex.main = 2)
# lowest AIC = 163.7442
# highest BIC = 187.3164

# 1st model coefficients with subset (X1, X2, X3, X3.sq, X1X2):
bs2$BestModel
#  Coefficients:
#  (Intercept)           X1           X2           X3        X3.sq         X1X2  
#    103.97280    -93.35747     -1.64122      2.77202     -0.01299      0.80433  

# 2nd modelcofficients with subset (X1, X2, X3, X1X2):
best22 <- lm(Y ~ X1 + X2 + X3 + X1X2)
best22$coefficients
# (Intercept)          X1          X2          X3        X1X2 
# 181.6977775 -95.5593423  -1.7535789   0.7951167   0.8620372 


# No, the best model in BIC is the second best model in AIC While the 
# the best modle in AIC is the second best model in BIC.  


# e)
install.packages("leaps")
library("leaps")
all.predictors2 <- data.frame(cbind(X1,X2,X3,X1.sq,X2.sq,X3.sq, X1X2,X1X3,X2X3))
bs3 <- leaps(all.predictors2,Y, method = "adjr2",nbest = 1)
bs3
names(bs3)
bs3$adjr2
max(bs3$adjr2)
bs3$which
#  with = 0.8668497, and 5 predictors
# The best model is with subset (X1, X2, X3, X3.sq, X1X2) with highest value = 0.8668497.
# The second best model is with subset (X1, X3, X1.sq, X2.sq, X3.sq, X2X3) with
# the second highest adj R^2 value = 0.8662531.


# to plot the Adjested R^2 values: 
plot(1:9, bs3$adjr2, type = "b", ylab = "Adjusted R^2",
     xlab = "Number of Predictors", lwd = 3, pch = 19, main = "Adjusted R^2", cex.main = 2)
# lowest adj R^2 = 0.6981099
# highest adj R^2 = 0.8668497

# 1st model coefficients with subset (X1, X2, X3, X3.sq, X1X2):
best3 <- lm (Y ~ X1 + X2 + X3 + X3.sq + X1X2)
best3$coefficients
# (Intercept)           X1           X2           X3        X3.sq         X1X2 
# 103.97279806 -93.35746856  -1.64121863   2.77202222  -0.01299075   0.80432836 

# # 2nd best model coefficients with subset (X1, X3, X1.sq, X2.sq, X3.sq, X2X3)
best33 <- lm (Y ~ X1 + X3 + X1.sq + X2.sq + X3.sq + X2X3)
best33$coefficients
# (Intercept)           X1           X3        X1.sq        X2.sq        X3.sq         X2X3 
# 15.93030620 -89.77085462   3.77134692  15.93373704   0.00789713  -0.01143438  -0.02091087

# f)
install.packages("MASS")
library(MASS)

# Forward selection with BIC
min.model <- lm(Y ~ 1, data = all.predictors)
max.model <- lm(Y~ ., data = all.predictors)
scp <- list(lower = min.model, upper = max.model)
fwd1 <- stepAIC(min.model,
                direction = 'forward', 
                scope = scp,
                k = log(nrow(all.predictors)))
fwd1$coefficients
fwd1$
# The best model with subset (X1X2, X3)
# best model coefficients
# (Intercept)        X1X2          X3 
#  73.6149703  -0.5964808   0.7648475 

# The second best model wtih subset (X1X2, X3, X3.sq)
best44 <- lm (Y ~ X1X2 + X3 + X3.sq)
best44$coefficients
# (Intercept)         X1X2           X3        X3.sq 
#-16.20594947  -0.59715466   3.18911906  -0.01585003 


# Forward selection with AIC
fwd2 <- stepAIC(min.model,    direction = 'forward', scope = scp)
fwd2$coefficients
# The best model with subset ( X1X2, X3, X3.sq)
# The best model coefficients:
# (Intercept)         X1X2           X3        X3.sq 
# -16.20594947  -0.59715466   3.18911906  -0.01585003 

# The second best model with subset ( X1X2, X3, X3.sq, X1)
best55 <- lm (Y ~ X1X2 + X3 + X3.sq + X1)
best55$coefficients
#  (Intercept)         X1X2           X3        X3.sq           X1 
# -20.75223087  -0.45633311   3.57570040  -0.01858308 -15.22483794 


# There are less predictors using the BIC and AIC forward selection than 
# using the BIC, AIC, Adjusted R^2 alone in part c to e.  

