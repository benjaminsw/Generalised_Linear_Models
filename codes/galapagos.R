# Poisson regression for the Galapagos data
library(faraway)
# ?gala

gal1 <- glm(Species ~ Area + Elevation +Nearest +Scruz +Adjacent, family=poisson, data=gala)
summary(gal1)
# Call:
# glm(formula = Species ~ Area + Elevation + Nearest + Scruz + 
#     Adjacent, family = poisson, data = gala)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.2752  -4.4966  -0.9443   1.9168  10.1849  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.155e+00  5.175e-02  60.963  < 2e-16 ***
# Area        -5.799e-04  2.627e-05 -22.074  < 2e-16 ***
# Elevation    3.541e-03  8.741e-05  40.507  < 2e-16 ***
# Nearest      8.826e-03  1.821e-03   4.846 1.26e-06 ***
# Scruz       -5.709e-03  6.256e-04  -9.126  < 2e-16 ***
# Adjacent    -6.630e-04  2.933e-05 -22.608  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 3510.73  on 29  degrees of freedom
# Residual deviance:  716.85  on 24  degrees of freedom
# AIC: 889.68

# Number of Fisher Scoring iterations: 5

par(mfrow=c(1,2))

# Normal probability plot of Pearson residuals:

qqnorm(resid(gal1,type="pearson"), main="QQ-plot of Pearson residuals")
qqline(resid(gal1,type="pearson"))

# Plot of variance against mean:

plot(log(fitted(gal1)),log((gala$Species-fitted(gal1))^2), xlab=expression(hat(mu)),ylab=expression((y-hat(mu))^2), main="Variance v mean")
abline(0,1)

# Estimate the dispersion parameter:
X2 <- sum(resid(gal1,type="pearson")^2)
dp <- X2/gal1$df.res
dp
# [1] 31.74914

# The standard errors in the summary are then adjusted as follows:

summary(gal1,dispersion=dp)
# 
# Call:
# glm(formula = Species ~ Area + Elevation + Nearest + Scruz + 
#     Adjacent, family = poisson, data = gala)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.2752  -4.4966  -0.9443   1.9168  10.1849  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.1548079  0.2915897  10.819  < 2e-16 ***
# Area        -0.0005799  0.0001480  -3.918 8.95e-05 ***
# Elevation    0.0035406  0.0004925   7.189 6.53e-13 ***
# Nearest      0.0088256  0.0102621   0.860    0.390    
# Scruz       -0.0057094  0.0035251  -1.620    0.105    
# Adjacent    -0.0006630  0.0001653  -4.012 6.01e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# (Dispersion parameter for poisson family taken to be 31.74914)
# 
#     Null deviance: 3510.73  on 29  degrees of freedom
# Residual deviance:  716.85  on 24  degrees of freedom
# AIC: 889.68
# 
# Number of Fisher Scoring iterations: 5

# Use F-test to compare models with an estimated dispersion parameter:

drop1(gal1,test="F")