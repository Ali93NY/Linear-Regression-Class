# 1)
#To get the data of the 120 students for question 1.19
data = read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt", header = FALSE)
# To name both columns  GPA, SAT
names(data)[1] = paste("GPA")
names(data)[2] = paste("SAT")
# To see the data
data
attach(data)
# a)
boxplot(SAT)
# The plot seems symmetrical without outliers and no notewarthy features on the plot.

# b)
reg = lm(GPA~SAT)
summary(reg)
e = resid(reg)
hist(e)
# The residuals seem to be cenrted around the zero with few outliers. 

# c) 
y.hat = predict(reg)
plot (y.hat, e)
# Random cloud around the zero with few outliers. 
# This is a good indication for a linear regression model.

# d)
qqnorm(e)
qqline(e)
ks.test(e, "pnorm")
# Ho: the normality assumption doesn't hold
# Ha: the normality assumption holds. 
# since 0.0049 is less than 0.005,
# we reject the Ha, and the normality assumption doesn't hold

# e)
install.packages("lmtest")
library(lmtest)
bptest(reg)
# Since the p-value (0.5877) is not less than 0.01,
# We don't reject the Ha, and the constast variance assumption holds. 
# The conclusion of the bptest supports the preliminary findings in part C. 

# 2)
# Q 3.10 on p.149
# To get the data for the 12 cities
data_cities = read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%203%20Data%20Sets/CH03PR10.txt")
# To name the columns
names (data_cities) [1] = paste("Yi")
names (data_cities) [2] = paste("e")
data_cities
attach(data_cities)

# a)
plot(Yi,e)
# the plot suggests the errors are independent with one pronounced outlier -3.78

# b)
plot(Yi,e)
abline(h = -1)
abline(h = 1)
# 4 semistudnetized residuals are outsied +/-1 standard deviation.
# If the error model is appropriate, we cannot see more than 2 obeservations outside 
# +/-1 standard deviation. 




# 3)
attach(data)

# a)
plot(x,y)
d.reg = lm(y~x)
abline(d.reg)

# b)
cor(x,y)
# correlation = 0.6642; the correlation is not strong since it is below 0.7

# c)
new.x = sqrt(x)
# d)
plot(new.x,y)
n.reg = lm(y~new.x)
abline(n.reg)

# e)
cor(new.x,y)
# correlation = 0.7171; the correlation is strong since it is above 0.7

# f)
summary(n.reg)
# y = 1.7636 + 9.3497 x

# g)
y.hat = predict(n.reg)
e = resid(n.reg)
plot(y.hat,e)

# h) 
# random cloud around zero with +/- 5 standard deviation and many outliers. 


