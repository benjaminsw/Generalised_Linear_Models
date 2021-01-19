# Hair and eye colour example from Faraway Ch. 4

library(faraway)

haireye
#      y   eye  hair
# 1    5 green BLACK
# 2   29 green BROWN
# 3   14 green   RED
# 4   16 green BLOND
# 5   15 hazel BLACK
# 6   54 hazel BROWN
# 7   14 hazel   RED
# 8   10 hazel BLOND
# 9   20  blue BLACK
# 10  84  blue BROWN
# 11  17  blue   RED
# 12  94  blue BLOND
# 13  68 brown BLACK
# 14 119 brown BROWN
# 15  26 brown   RED
# 16   7 brown BLOND

(ct <- xtabs(y ~ hair+eye, data=haireye))
#        eye
# hair    green hazel blue brown
#   BLACK     5    15   20    68
#   BROWN    29    54   84   119
#   RED      14    14   17    26
#   BLOND    16    10   94     7

# Chi-squared test of independence:

summary(ct)

# Call: xtabs(formula = y ~ hair + eye, data = haireye)
# Number of cases in table: 592 
# Number of factors: 2 
# Test for independence of all factors:
# 	Chisq = 138.29, df = 9, p-value = 2.325e-25

ind.mod <- glm( y ~ hair + eye, family=poisson, data=haireye)
summary(ind.mod)
# Call:
# glm(formula = y ~ hair + eye, family = poisson, data = haireye)
# 
# Deviance Residuals: 
#    Min      1Q  Median      3Q     Max  
# -7.326  -2.065  -0.212   1.235   6.172  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.4575     0.1523  16.136  < 2e-16 ***
# hairBROWN     0.9739     0.1129   8.623  < 2e-16 ***
# hairRED      -0.4195     0.1528  -2.745  0.00604 ** 
# hairBLOND     0.1621     0.1309   1.238  0.21569    
# eyehazel      0.3737     0.1624   2.301  0.02139 *  
# eyeblue       1.2118     0.1424   8.510  < 2e-16 ***
# eyebrown      1.2347     0.1420   8.694  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 453.31  on 15  degrees of freedom
# Residual deviance: 146.44  on  9  degrees of freedom
# AIC: 241.04
# 
# Number of Fisher Scoring iterations: 5

