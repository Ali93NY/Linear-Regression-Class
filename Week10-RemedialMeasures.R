# Get data from Table 11.1 on p. 427

data <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2011%20Data%20Sets/CH11TA01.txt", header = FALSE)
names(data)[1]<-paste("X")
names(data)[2]<-paste("Y")
attach(data)

# Fitting regression with y on x
reg = lm(Y ~ X)
summary(reg)

# Obtaining the residuals
(e = resid(reg))
plot(X,e)

abse = abs(e)

plot(X, abse)

# Regression of |e| vs. X

reg2 = lm (abse ~ X)
summary(reg2)
abline(reg2)

# Obtaining estimated st. dev.
(si = predict(reg2))

# Weights
(w = 1/si^2)


Xm = cbind(rep(1, nrow(data)), X)
(bnew = solve(t(Xm)%*%diag(w)%*%Xm)%*%t(Xm)%*%diag(w)%*%Y)

#         [,1]
#   55.5657664
# X  0.5963417
# 
# This completes the first loop of the IRWLS method

# MSE(w)

MSEw = t(Y-Xm%*%bnew)%*%diag(w)%*%(Y-Xm%*%bnew)/((nrow(data)-2))
sbnew = drop(MSEw)*solve(t(Xm)%*%diag(w)%*%Xm)
sbnew
#                         X
#    6.3550256 -0.189363636
# X -0.1893636  0.006278666
sqrt(diag(sbnew))
#                     X 
# 2.52091762 0.07923803 

# For example, without assuming constant var, SE(b1) = 0.079

# Alternatively: use function irls from package msme


# Shrinkage methods
# ECLS-K Example 
###############################################################################
### Load the eclsk_c data from Week 8 by opening a browser window
load(file.choose())

### Or call the path directly (fill in your path)
load(file = file.choose())
head(eclsk_c)
dim(eclsk_c)

# Or just double-click on file name

### Remove outcomes and selection
eclsk1 <- eclsk_c[, !names(eclsk_c) %in% c("S1_ID", "CHILDID", "C5R4RSCL",
                                           "C6R4RSCL", "C5R4MSCL", "C6R4MSCL", "F5SPECS")]

### Standardize the predictors by dividing each by its standard deviation.
sds <- apply(eclsk1, 2, sd)
matsds <- matrix(rep(sds, times = nrow(eclsk1)), nrow(eclsk1), byrow = TRUE)
eclsk1 <- eclsk1/matsds

### Put selection and math outcome at end of data frame
eclsk1 <- cbind(eclsk1, F5SPECS = eclsk_c$F5SPECS, 
                C6R4MSCL = eclsk_c$C6R4MSCL)
head(eclsk1)

?apply
### OLS regression of outcome on other variables
lm1 <- lm(C6R4MSCL ~ ., data = eclsk1)
summary(lm1)

library(MASS)
# Plots from lecture notes
### Function to get coefficients from ridge regression with given lambda
fridge <- function(lam) {
  lmr <- lm.ridge(C6R4MSCL ~ ., data = eclsk1, lambda = lam)
  out <- coef(lmr)
  out }

dom <- c(10^c(-1:6))
outs <- sapply(dom, fridge)[-1,]
range(outs)
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(log(dom), outs[1,], type = "l", lty = 1, col = 1, ylim = range(outs), 
     xlab = "log(lambda)", ylab = "Ridge Betas", main = "Ridge regression 
     coefficients for ECLSK data\nexample for different values of lambda")
for (i in 1:dim(outs)[1]) {points(log(dom), outs[i,], type = "l", 
                                  col = rainbow(dim(outs)[1])[i])}

l2 <- function(vec) {vec%*%vec}
plot(x = log(dom), apply(outs, 2, l2), type = "l", xlab = "log(lambda)", 
     ylab = "l2 Norm of Ridge Betas")


?sapply
### Ridge example with ECLSK
summary(lm1)
install.packages("glmnet")
library(glmnet)
# Rather than accepting a formula and data frame, it requires a vector input and matrix of predictors
# Use alpha = 0 for ridge
# Because, unlike OLS regression done with lm(), ridge regression involves tuning a hyperparameter, lambda, 
# the function runs the model many times for different values of lambda. We can automatically find a value 
# for lambda that is optimal 
set.seed(2911)
cvridge <- cv.glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], 
                     family = "gaussian", alpha = 0)

plot(cvridge)
# The lowest point in the curve indicates the optimal lambda: the log value of lambda that best minimized 
# the error in cross-validation. We can extract this values as
(opt_lambda = cvridge$lambda.min)


coef(cvridge)
coefs1 <- cbind(coef(lm1), as.numeric(coef(cvridge)))
colnames(coefs1) <- c("OLS", "Ridge")
coefs1

# predicting values and computing an R2 value for the data we trained on:
y_predicted <- predict(cvridge, s = opt_lambda, newx = as.matrix(eclsk1[,1:35]))

# Sum of Squares Total and Error
y = eclsk1[,36]
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

### Scale equivariance of OLS.
summary(lm(C6R4MSCL ~ MIRT + RIRT, data = eclsk1))
MIRT2 <- eclsk1$MIRT/100
summary(lm(C6R4MSCL ~ MIRT2 + RIRT, data = eclsk1))


### Function to get coefficients from lasso regression with given lambda
library(glmnet)
flasso <- function(lam) {
  lml <- glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], 
                family = "gaussian", alpha = 1, lambda = lam)
  out <- coef(lml)
  out }
dom2 <- c(10^seq(-2, 1.5, .5))
outs <- sapply(dom2, flasso)
outs2 <- cbind(as.numeric(outs[[1]][-1]), as.numeric(outs[[2]][-1]), 
               as.numeric(outs[[3]][-1]), as.numeric(outs[[4]][-1]),
               as.numeric(outs[[5]][-1]), as.numeric(outs[[6]][-1]), 
               as.numeric(outs[[7]][-1]), as.numeric(outs[[8]][-1]))
range(outs2)
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(log(dom2), outs2[1,], type = "l", lty = 1, col = 1, ylim = range(outs2), 
     xlab = "log(lambda)", ylab = "Lasso Betas", main =  
       "Lasso regression coefficients for ECLSK data\nexample for different values of lambda")
for (i in 1:dim(outs2)[1]) {points(log(dom2), outs2[i,], type = "l", 
                                   lwd = 2, col = rainbow(dim(outs2)[1])[i])}

l1 <- function(vec) {sum(abs(vec))}
plot(x = log(dom2), apply(outs2, 2, l1), type = "l", xlab = "log(lambda)", 
     ylab = "l1 Norm of Lasso Betas", lwd = 3)


### Lasso example with ECLSK
summary(lm1)
library(glmnet)
### Use alpha = 1 for lasso
set.seed(1080)
cvlasso <- cv.glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], 
                     family = "gaussian", alpha = 1)
coef(cvlasso)

coefs2 <- cbind(coef(lm1), as.numeric(coef(cvridge)), as.numeric(coef(cvlasso)))
colnames(coefs2) <- c("OLS", "Ridge", "Lasso")
coefs2
round(coefs2, 2)

### Principal components example with ECLSK data
### Raw plot
plot(eclsk1$RIRT, eclsk1$MIRT, xlab = "Reading Pretest Score (RIRT)",
     ylab = "Math Pretest Score (MIRT)", main = "Raw Data Values")
### Centered plot
plot(eclsk1$RIRT - mean(eclsk1$RIRT), eclsk1$MIRT - mean(eclsk1$MIRT), 
     xlab = "Mean-Centered Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered Math Pretest Score (MIRT)", main = "Mean-Centered Data Values")

### Centered and scaled
plot(MIRT ~ RIRT, data = scale(eclsk1),#[sample(1:7362, size = 1000, replace = FALSE),],
     xlab = "Mean-Centered and Scaled Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered and Scaled Math Pretest Score (MIRT)", 
     main = "Mean-Centered and Scaled Data Values")
### Principal components
ec.pca <- prcomp( ~ MIRT + RIRT, data = eclsk1,
                  center = TRUE, scale. = TRUE)
ec.pca

### With PCA fit
plot(MIRT ~ RIRT, data = scale(eclsk1),#[sample(1:7362, size = 1000, replace = FALSE),],
     xlab = "Mean-Centered and Scaled Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered and Scaled Math Pretest Score (MIRT)", 
     main = "Principal Components")
abline(a = 0, b = 1, col = 3, lwd = 3)
abline(a = 0, b = -1, col = 4, lwd = 3, lty = 2)



### Run PCR with package pls.
library(pls)
pcr.fit <- pcr(C6R4MSCL ~ ., data = eclsk1, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")


# Exercise:
# Use the dataset from Q11.23 on p. 477
data1 <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%2011%20Data%20Sets/CH11PR23.txt", header = FALSE)
names(data1)[1]<-paste("Y")
names(data1)[2]<-paste("X1")
names(data1)[3]<-paste("X2")
names(data1)[4]<-paste("X3")
names(data1)[5]<- paste("X4")

# a) Fit the traditional OLS and state the equation
# b) Obtain ridge regression standardized coefficients and R^2 for lambda = 0, 0.002, 0.06, and 0.1
# c) Obtain the optimal lambda with the CV package


# a)

# b)
# lambda = 0.1
data2 <- as.data.frame(scale(data1))

lm.ridge(Y ~ ., data = data2, lambda = 0.1)

lm.ridge(Y ~ ., data = data2, lambda = 0.0)





sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
