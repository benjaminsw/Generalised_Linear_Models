cars <- read.csv("cars.csv")
cars$resp <- factor(cars$response, levels=c("no/little", "important", "very important"))
cars

# Plot the data
library(ggplot2)
library(gridExtra)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p1 <- ggplot(cars, aes(x = cars$age, y = cars$frequency, fill = response)) +
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Age groups") + ylab("Frequency") +
  theme(legend.position = "none")

p2 <- ggplot(cars, aes(x = cars$sex, y = cars$frequency, fill = response)) +
  geom_bar(stat = "identity", position = "dodge")+
  xlab("Sex") + ylab("Frequency") +
  scale_fill_discrete(name = "Response category") + 
  theme(legend.position = "bottom")

carsLeg <- g_legend(p2)
grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         p2 + theme(legend.position="none"),
                         nrow=1), nrow = 2, carsLeg, heights=c(10, 1))

# Nominal logistic regression

library(nnet)

m1 <- multinom(resp~age+sex, weight=frequency, data=cars)
summary(m1)

# Call:
# multinom(formula = resp ~ age + sex, data = cars, weights = frequency)
# 
# Coefficients:
#                (Intercept) agelin4-40  age> 40  sexwomen
# important       -0.9789276 1.128266 1.587703 0.3881269
# very important  -1.8521016 1.478109 2.916752 0.8130197
# 
# Std. Errors:
#                (Intercept)  agelin4-40   age> 40  sexwomen
# important        0.2563336 0.3416447 0.4028985 0.3005111
# very important   0.3307065 0.4009262 0.4229274 0.3210382
# 
# Residual Deviance: 580.7022
# AIC: 596.7022

# Null model:
m0 <- multinom(resp~1, weight=frequency, data=cars)
summary(m0)
# Call:
# multinom(formula = resp ~ 1, data = cars, weights = frequency)
# 
# Coefficients:
#                (Intercept)
# important      -0.11066559
# very important -0.03883986
# 
# Std. Errors:
#                (Intercept)
# important        0.1419933
# very important   0.1393729
# 
# Residual Deviance: 658.544
# AIC: 662.544

# Plot of coefficient estimates on odds ratio scale:
library(sjPlot)
plot_model(m1)

# Plot of predicted probabilities (fitted values):
library(ggeffects)
p1 <- ggpredict(m1, c("sex", "age"))
plot(p1)


# Maximal (saturated model):


m2 <- multinom(resp~age*sex, weight=frequency, data=cars)
summary(m2)
# Call:
# multinom(formula = resp ~ age * sex, data = cars, weights = frequency)
# 
# Coefficients:
#                (Intercept)  agelin4-40  age> 40   sexwomen agelin4-40:sexwomen
# important        -0.855691 0.7305883 1.484325 0.08253857         0.8898474
# very important   -1.609489 1.2612152 2.420439 0.29736599         0.5616963
#                age> 40:sexwomen
# important             0.3183712
# very important        0.9957248
# 
# Std. Errors:
#                (Intercept)  agelin4-40   age> 40  sexwomen agelin4-40:sexwomen
# important        0.2895236 0.4575083 0.5248736 0.4534513         0.6998827
# very important   0.3873041 0.5405208 0.5749454 0.5756020         0.8070024
#                age> 40:sexwomen
# important             0.8177397
# very important        0.8580499
# 
# Residual Deviance: 576.7635
# AIC: 600.7635

anova(m1,m2)
# Likelihood ratio tests of Multinomial Models
# 
# Response: resp
#       Model Resid. df Resid. Dev   Test    Df LR stat.   Pr(Chi)
# 1 age + sex        28   580.7022
# 2 age * sex        24   576.7635 1 vs 2     4 3.938713 0.4143637


# Linear term for age:

cars$agelin <- 0
cars$agelin[cars$age=="24-40"] <- 1
cars$agelin[cars$age=="> 40"] <- 2
cars

m3 <- multinom(resp~agelin+sex, weight=frequency, data=cars)
summary(m3)

# Coefficients:
#                (Intercept)      agelin  sexwomen
# important       -0.8908983 0.8303799 0.3889732
# very important  -1.9053456 1.5214463 0.8130386
# 
# Std. Errors:
#                (Intercept)      agelin  sexwomen
# important        0.2402679 0.1946354 0.2991328
# very important   0.3089657 0.2114578 0.3211055
# 
# Residual Deviance: 582.1003
# AIC: 594.1003

# Odds ratios:

plot_model(m3)

# Fitted values (predicted probabilities):
p3 <- ggpredict(m3, c("sex", "agelin"))
plot(p3)

# Ordinal logistic regression: proportional odds model

library(MASS)

m4 <- polr(resp ~ sex + age, data=cars, weight=frequency)
summary(m4)

# Re-fitting to get Hessian
# 
# Call:
# polr(formula = resp ~ sex + age, data = cars, weights = frequency)
# 
# Coefficients:
#           Value Std. Error t value
# sexwomen 0.5762     0.2262   2.548
# agelin4-40 1.1471     0.2776   4.132
# age> 40  2.2325     0.2915   7.659
# 
# Intercepts:
#                          Value  Std. Error t value
# no/little|important      0.6198 0.2168     2.8588
# important|very important 2.2312 0.2546     8.7625
# 
# Residual Deviance: 581.2956
# AIC: 591.2956

# Could also try a model with a linear term for age
m5 <- polr(resp ~ sex + agelin, data=cars, weight=frequency)
summary(m5)

# Re-fitting to get Hessian
# 
# Call:
# polr(formula = resp ~ sex + agelin, data = cars, weights = frequency)
# 
# Coefficients:
#          Value Std. Error t value
# sexwomen 0.577     0.2261   2.552
# agelin     1.116     0.1457   7.660
# 
# Intercepts:
#                          Value  Std. Error t value
# no/little|important      0.6101 0.2034     2.9999
# important|very important 2.2214 0.2430     9.1426
# 
# Residual Deviance: 581.3124
# AIC: 589.3124

# Note that the deviances are similar to the corresponding nominal regression models.

# Predicted probabilities:
p5 <- ggpredict(m5)
plot(p5)