# London Olympics data

olympics0 <- read.csv("OlympicMedals2012.csv")

olympics <- data.frame(country=olympics0$Country, medals=olympics0$Medals, population=olympics0$Population, gold=olympics0$Gold.Medal, GDP=olympics0$GDP..US.Billion)

# Create a variable for GDP per capita in 1000 US dollars:
olympics$GDPpercapita <- olympics$GDP*10^6/olympics$population

head(olympics)

plot(log(olympics$population), log(olympics$medals), xlab="log(Population)", ylab="log(Olympic Medals)")

plot(olympics$GDPpercapita, log(olympics$medals), xlab="GDPpercapita", ylab="log(Olympic Medals)")

plot(log(olympics$GDPpercapita), log(olympics$medals), xlab="log(GDPpercapita)", ylab="log(Olympic Medals)")

mod.p1 <- glm(medals ~ log(population) +GDPpercapita, family=poisson, data=olympics)
summary(mod.p1)

# Call:
# glm(formula = medals ~ log(population) + GDPpercapita, family = poisson, 
#     data = olympics)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.8957  -2.2193  -0.5103   1.3699   9.7661  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -7.969828   0.373133  -21.36   <2e-16 ***
# log(population)  0.574035   0.019835   28.94   <2e-16 ***
# GDPpercapita     0.021908   0.001415   15.48   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 1567.70  on 84  degrees of freedom
# Residual deviance:  652.18  on 82  degrees of freedom
# AIC: 954.01

X2 <- sum(resid(mod.p1,type="pearson")^2)
X2
# [1] 705.7355

dp1 <- X2/mod.p1$df.res
dp1
# [1] 8.606531

summary(mod.p1,dispersion=dp1)
# glm(formula = medals ~ log(population) + GDPpercapita, family = poisson, 
#     data = olympics)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -8.8957  -2.2193  -0.5103   1.3699   9.7661  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -7.969828   1.094656  -7.281 3.32e-13 ***
# log(population)  0.574035   0.058189   9.865  < 2e-16 ***
# GDPpercapita     0.021908   0.004152   5.276 1.32e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for poisson family taken to be 8.606531)
# 
#     Null deviance: 1567.70  on 84  degrees of freedom
# Residual deviance:  652.18  on 82  degrees of freedom
# AIC: 954.01
# 
# Number of Fisher Scoring iterations: 5

drop1(mod.p1, test="F")
# Single term deletions
# 
# Model:
# medals ~ log(population) + GDPpercapita
#                 Df Deviance     AIC F value    Pr(>F)    
# <none>               652.18  954.01                      
# log(population)  1  1498.24 1798.07 106.376 < 2.2e-16 ***
# GDPpercapita     1   867.52 1167.34  27.074 1.418e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# Warning message:
# In drop1.glm(mod.p1, test = "F") : F test assumes 'quasipoisson' family

qqnorm(resid(mod.p1,type="pearson"))
qqline(resid(mod.p1,type="pearson"))


mod.p2 <- glm(medals ~ log(population) + log(GDPpercapita), family=poisson, data=olympics)
summary(mod.p2)

dp2 <- sum(resid(mod.p2,type="pearson")^2)/mod.p2$df.res
dp2
# [1] 7.352356


summary(mod.p2,dispersion=dp2)
# Call:
# glm(formula = medals ~ log(population) + log(GDPpercapita), family = poisson, 
#     data = olympics)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -6.0028  -2.2661  -0.3188   1.1572   8.2493  
# 
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -9.1125     1.0811  -8.429  < 2e-16 ***
# log(population)     0.5948     0.0549  10.835  < 2e-16 ***
# log(GDPpercapita)   0.4976     0.0806   6.173 6.69e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for poisson family taken to be 7.352356)
# 
#     Null deviance: 1567.7  on 84  degrees of freedom
# Residual deviance:  547.5  on 82  degrees of freedom
# AIC: 849.33



qqnorm(resid(mod.p2,type="pearson"))
qqline(resid(mod.p2,type="pearson"))


# Negative binomial model:

library(MASS)
mod.n1 <- glm.nb(medals ~ log(population) +GDPpercapita, data=olympics)
summary(mod.n1)
# 
# Call:
# glm.nb(formula = medals ~ log(population) + GDPpercapita, data = olympics, 
#     init.theta = 1.497652506, link = log)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.9905  -1.0653  -0.2358   0.5214   2.1060  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -6.525208   1.072827  -6.082 1.19e-09 ***
# log(population)  0.500594   0.062664   7.988 1.37e-15 ***
# GDPpercapita     0.014278   0.004492   3.179  0.00148 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for Negative Binomial(1.4977) family taken to be 1)
# 
#     Null deviance: 175.876  on 84  degrees of freedom
# Residual deviance:  85.789  on 82  degrees of freedom
# AIC: 529.73
# 
# Number of Fisher Scoring iterations: 1
# 
# 
#               Theta:  1.498 
#           Std. Err.:  0.263 
# 
#  2 x log-likelihood:  -521.729

qqnorm(resid(mod.n1,type="pearson"))
qqline(resid(mod.n1,type="pearson"))

# Let's see which countries have large residuals:
pres.n1 <- resid(mod.n1,type="pearson")
expmedals <- round(fitted(mod.n1)[abs(pres.n1)>1],2)
cbind(olympics[abs(pres.n1)>1,],expmedals)


mod.n2 <- glm.nb(medals ~ log(population) + log(GDPpercapita), data=olympics)
summary(mod.n2)

# Call:
# glm.nb(formula = medals ~ log(population) + log(GDPpercapita), 
#     data = olympics, init.theta = 1.65008023, link = log)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.9940  -1.1049  -0.2763   0.4575   2.2539  
# 
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -7.17156    1.07756  -6.655 2.83e-11 ***
# log(population)    0.50984    0.06078   8.389  < 2e-16 ***
# log(GDPpercapita)  0.31739    0.07645   4.152 3.30e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# (Dispersion parameter for Negative Binomial(1.6501) family taken to be 1)

#     Null deviance: 190.561  on 84  degrees of freedom
# Residual deviance:  85.484  on 82  degrees of freedom
# AIC: 522.95
# 
# Number of Fisher Scoring iterations: 1
# 
# 
#               Theta:  1.650 
#           Std. Err.:  0.300 
# 
#  2 x log-likelihood:  -514.946 

pres.n2 <- resid(mod.n2,type="pearson")

par(mfrow=c(2,2))
par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
qqnorm(pres.n2, main="QQ-plot of Pearson residuals")
qqline(pres.n2)
plot(predict(mod.n2), pres.n2, xlab="Linear predictors", ylab="Pearson residuals")
abline(h=0)
plot(log(olympics$GDPpercapita),pres.n2, xlab="log(GDP per capita)", ylab="Pearson residuals")
plot(log(olympics$population),pres.n2, xlab="log(population)", ylab="Pearson residuals")


expmedals2 <- round(fitted(mod.n2)[abs(pres.n2)>1],2)
cbind(olympics[abs(pres.n2)>1,1:2],expmedals2)

# One could consider more complicated models, e.g. interaction:
# mod.n3 <- glm.nb(medals ~ log(population)* log(GDPpercapita), data=olympics)
# summary(mod.n3)
