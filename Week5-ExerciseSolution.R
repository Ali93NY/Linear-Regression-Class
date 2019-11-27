# Multiple Regression 

# Exercise
# Use skin cancer data and fit a mode with both latitude and longitude

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
#              Lat       Mort        Long
# Lat   1.00000000 -0.8245178  0.09897372
# Mort -0.82451779  1.0000000 -0.14618812
# Long  0.09897372 -0.1461881  1.00000000


# a)

reg = lm(Mort ~ Lat + Long)
summary(reg)


# Residuals:
#     Min      1Q  Median      3Q     Max 
# -36.551 -13.525  -0.757  14.055  41.700 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 400.6755    28.0512  14.284  < 2e-16 ***
# Lat          -5.9308     0.6038  -9.822 7.18e-13 ***
# Long         -0.1467     0.1873  -0.783    0.438    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.19 on 46 degrees of freedom
# Multiple R-squared:  0.684,     Adjusted R-squared:  0.6703 
# F-statistic: 49.79 on 2 and 46 DF,  p-value: 3.101e-12

# Predicted mortality rate = 400.68 - 5.93*Lat - 0.15*Long

# b)
# For every 1 degree increase in latitude, the mortality rate decrease by 5.93 people per 100,00
# keeping longitude fixed

# For every 1 degree increase in longitude, the mortality rate decrease by 0.15 people per 100,00
# keeping latitude fixed

# c)

predict(reg)
#        1        2        3        4        5        6        7        8 
# 192.1990 179.6364 179.5307 160.7440 153.9009 142.0901 158.3005 158.0805 
#        9       10       11       12       13       14       15       16 
# 222.5864 192.7123 120.0347 150.3165 149.6143 136.6380 157.8929 164.0243 
#       17       18       19       20       21       22       23       24 
# 202.1706 122.4826 158.1538 139.8644 130.2918 113.9982 192.9452 158.8461 
#       25       26       27       28       29       30       31       32 
# 105.7209 139.9537 152.2143 130.4191 151.3301 177.5509 134.5771 178.4718 
#       33       34       35       36       37       38       39       40 
# 104.2220 150.1129 175.8760 122.0469 147.2877 142.2808 188.3342 120.3086 
#       41       42       43       44       45       46       47       48 
# 174.5238 199.4820 150.0555 129.0863 166.7568 101.2156 158.7094 123.5251 
#       49 
# 129.8842 


resid(reg)
#           1           2           3           4           5           6 
#  26.8010134 -19.6363756  -9.5307154  21.2560406  -4.9008643  16.9098834 
#           7           8           9          10          11          12 
#  41.6995080  18.9194893 -25.5864400  21.2877235  -4.0347029 -26.3164960 
#          13          14          15          16          17          18 
# -21.6142878  -8.6380426   8.1071376 -17.0242803 -12.1705517  -5.4825589 
#          19          20          21          22          23          24 
#   3.8461622   3.1355637 -13.2918399   2.0017938  14.0548089 -27.8461150 
#          25          26          27          28          29          30 
#   3.2790983 -17.9536988  38.7856596  -1.4190943   7.6698574 -36.5508829 
#          31          32          33          34          35          36 
#  17.4228536  20.5281975  10.7779739 -19.1129123   6.1239779  13.9531316 
#          37          38          39          40          41          42 
# -15.2876817  -5.2807671 -10.3342430 -34.3086115  11.4761992  29.5179556 
#          43          44          45          46          47          48 
#  -8.0555206  23.9137273  -0.7567839  15.7843862 -22.7093918 -13.5250742 
#          49 
#   4.1157898 


# d) 
# F-test = 49.79, p-value = 3.101e-12

# Conclusion: Since p-value < 0.05 we reject H0
# That is, at least one predictor is useful for predicting mortality

# Which one(s)?

# p-value for latitude = 7.18e-13 < 0.05, so latitude is significant predictor
# p-value for longitude = 0.438 > 0.05, so longitude is NOT significant predictor

# e)
# R-square = 0.684
# That, is 68.4% of the mortality rate variability is explained by the regression equation


# f)
confint(reg, level = 0.96)
#                     2 %        98 %
# (Intercept) 341.3855358 459.9654817
# Lat          -7.2070787  -4.6545942
# Long         -0.5424837   0.2491752
# 

# g)

newdata = data.frame(Lat=40, Long = 70) 

# We now apply the predict function 
predict(reg, newdata)
 
#        1 
# 153.1763

# h)
predict(reg, newdata, interval="confidence")
#         fit      lwr      upr
# 1 153.1763 143.4828 162.8697



# i)
predict(reg, newdata, interval="prediction", level = 0.91)
#        fit      lwr      upr
# 1 153.1763 118.9043 187.4482




