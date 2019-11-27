# To get the data of the 120 students for question 1.19
data = read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR19.txt", header = FALSE)
# To name both columns Y = GPA, X = SAT
names(data)[1] = paste("y")
names(data)[2] = paste("x")
# To see the data
data
attach(data)

# Answering a.
reg = lm(y~x)
summary(reg)
# beta_0 = 2.11405, beta_1 = 0.03883
# the estimate regression function Y = 2.11405 + 0.03883 X

# Answering b.
plot(x,y)
abline (reg)
# the data is too spread out, the estimated regression function is going to have
# issues due to the high variance in the data.

# Answering c.
2.11405 + 0.03883 *(30)
# Y = 3.27895

# Or

newx <- data.frame( x = 30) 
predict(reg, newdata =  newx)


# Answerign d.
# When the entrance mean score increased by one, the mean response increase by 0.03883

