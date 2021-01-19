dat=read.table("separation.dat",header=TRUE)

dat
#   x t1 t2
# 1 1 29 62
# 2 1 30 83
# 3 1 31 74
# 4 1 32 68
# 5 0 29 41
# 6 0 30 44
# 7 0 31 21
# 8 0 32 50
# 9 0 33 33

attach(dat)

plot(t1[x==1],t2[x==1], col="blue", pch=1, xlab="t1",ylab="t2", ylim=c(20,85))
points(t1[x==0],t2[x==0], col="red", pch=2)

abline(a=58, b=0) 

fit=glm(x~t1+t2, family="binomial")
# Warning message:
# In glm.fit(x = X, y = Y, weights = weights, start = start, etastart = etastart,  :
#   fitted probabilities numerically 0 or 1 occurred

summary(fit)
# 
# Call:
# glm(formula = x ~ t1 + t2, family = "binomial")
# 
# Deviance Residuals:
#        Min          1Q      Median          3Q         Max
# -1.610e-05  -1.058e-06  -2.107e-08   2.107e-08   1.456e-05
# 
# Coefficients:
#               Estimate Std. Error   z value Pr(>|z|)
# (Intercept)  3.604e+01  1.727e+06  2.09e-05        1
# t1          -5.808e+00  5.372e+04 -1.08e-04        1
# t2           2.541e+00  4.292e+03     0.001        1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 1.2365e+01  on 8  degrees of freedom
# Residual deviance: 5.0320e-10  on 6  degrees of freedom
# AIC: 6
# 
# Number of Fisher Scoring iterations: 24

## Notice that the likelihood for this model is zero!!!