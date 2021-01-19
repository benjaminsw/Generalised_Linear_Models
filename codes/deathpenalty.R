deathpenalty <- data.frame (D=rep(c("white","white","black","black"),2),
                            V=rep(c("white","black"),4),
                            P=c(rep("yes",4),rep("no",4)),
                            freq = c(19,0,11,6,132,9,52,97))

xtabs(freq ~ D + V+ P, data=deathpenalty)

# Loglinear models:

l1 <- glm(freq ~ D + V+ P, family=poisson, data=deathpenalty)
summary(l1)
# Call:
# glm(formula = freq ~ D + V + P, family = poisson, data = deathpenalty)
# 
# Deviance Residuals:
#       1        2        3        4        5        6        7        8
#  1.9881  -3.4843  -0.3023  -0.1196   3.7542  -7.0237  -5.0100   5.7623
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)  3.92657    0.11075  35.455  < 2e-16 ***
# Dwhite      -0.03681    0.11079  -0.332     0.74
# Vwhite       0.64748    0.11662   5.552 2.83e-08 ***
# Pyes        -2.08636    0.17671 -11.807  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 395.92  on 7  degrees of freedom
# Residual deviance: 137.93  on 4  degrees of freedom
# AIC: 181.61
# 
# Number of Fisher Scoring iterations: 5

l2 <- glm(freq ~ P + D*V, family=poisson, data=deathpenalty)
summary(l2)

l3 <- glm(freq ~ D+ P*V, family=poisson, data=deathpenalty)
summary(l3)

l4 <- glm(freq ~ V + D*P, family=poisson, data=deathpenalty)
summary(l4)

l5 <- glm(freq ~ V*P + D*P, family=poisson, data=deathpenalty)
summary(l5)

l6 <- glm(freq ~ D*V + D*P, family=poisson, data=deathpenalty)
summary(l6)

# the following model appears to be a good fit:
l7 <- glm(freq ~ D*V + V*P, family=poisson, data=deathpenalty)
summary(l7)


# Pearson's chi-squared:
sum(resid(l7, type="pearson")^2)

l8 <- glm(freq ~ D*V + V*P +D*P, family=poisson, data=deathpenalty)
summary(l8)

# Pearson's chi-squared:
sum(resid(l8, type="pearson")^2)


# saturated model:
l9 <- glm(freq ~ D*V*P, family=poisson, data=deathpenalty)
summary(l9)

# Observed values and fitted values for the last three models:
cbind(deathpenalty$freq, round(fitted(l7),2), round(fitted(l8),2), round(fitted(l9)))

