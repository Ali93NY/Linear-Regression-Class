# 5.24

Y = matrix(c(16,5,10,15,13,22),,)
x <- c(4,1,2,3,3,4)
X <- cbind(rep(1, 6), x)

# 1)

# X'X
t(X)%*%X

# (X'X)^(-1)
solve(t(X)%*%X)

# X'Y
t(X)%*%Y
          
# (X'X)^(-1)*(X'Y)
b = solve(t(X)%*%X) %*% t(X)%*%Y
b
# the estimated regression cofficients are 
# B0 = 0.4390
# B1 = 4.6098

# 2)

# predicated values:
y.hat = X %*% b
# Residuals
e = Y - y.hat  
e

# 3)

