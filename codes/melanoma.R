# Read in the data:
meldata<-read.table("melan.dat")
meldata<-data.frame(survived=meldata[,1], tumour.thickness=meldata[,2])


table(meldata$survived)
# survived
#   0   1
#  62 282

# Fit a logistic regression model:
melan.fit<-glm(survived~tumour.thickness,family=binomial,data=meldata)
summary(melan.fit)

# Call:
# glm(formula = survived ~ tumour.thickness, family = binomial, 
#     data = meldata)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.2842   0.4062   0.4446   0.5406   1.9754  
# 
# Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        2.5848     0.2401  10.767   <2e-16 ***
# tumour.thickness  -0.5255     0.0845  -6.219    5e-10 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 324.56  on 343  degrees of freedom
# Residual deviance: 271.17  on 342  degrees of freedom
# AIC: 275.17
# 
# Number of Fisher Scoring iterations: 4

# Boxplot of tumour thickness by outcome:
boxplot(meldata$tumour.thickness~meldata$survived, 
        names=c("Dead", "Alive"),ylab="Tumour thickness in mm", 
        xlab="5 year survival")

thickness <- seq(from=min(meldata$tumour.thickness),to=max(meldata$tumour.thickness),by=0.01)


# Get fitted values on probability scale: 
#  pred=predict(melglm, newdata = data.frame(tumour.thickness=thickness),type="response",se.fit = TRUE)

# Get fitted values and approx 95% CI on log odds scale: 
pred <- predict(melan.fit, newdata = data.frame(tumour.thickness=thickness),type="lin",se.fit = TRUE)
l <- pred$fit-qnorm(0.975)*pred$se.fit
u <- pred$fit+qnorm(0.975)*pred$se.fit

# Transform to probabilities using the inverse logistic transform:
fitted <- exp(pred$fit)/(1+exp(pred$fit))
lower <- exp(l)/(1+exp(l))
upper <- exp(u)/(1+exp(u))

# Plot the fitted probabilities with confidence bounds:

plot(thickness, fitted, type="l",xlab="Tumour thickness in mm", ylab="Predicted probability of being alive after 5 years")
lines(thickness,lower, lty=2)
lines(thickness,upper, lty=2)

# Obtain the covariance between the parameter estimates:
vcov(melan.fit,method='glm')
#                  (Intercept) tumour.thickness
# (Intercept)       0.05763448     -0.015406165
# tumour.thickness -0.01540617      0.007139447

# Get the prediction for tumour.thickness=1mm:
 predict(melan.fit, newdata=data.frame(tumour.thickness=1), type="lin", se.fit=TRUE)
# $fit
#        1
# 2.059363
# 
# $se.fit
# [1] 0.1842867

# Lower and upper limits on the log odds scale:
2.059363-qnorm(0.975)*0.1842867
# [1] 1.698168

2.059363+qnorm(0.975)*0.1842867
# [1] 2.420558

# Transform to probabilities:
exp(2.059363-qnorm(0.975)*0.1842867)/(1+exp(2.059363-2*0.1842867))
# [1] 0.8505806

exp(2.059363+qnorm(0.975)*0.1842867)/(1+exp(2.059363+2*0.1842867))
# [1] 0.9121779

# Point estimate for the probability of survival at tumour.thickness=1mm:
exp(2.059363)/(1+exp(2.059363))
# [1] 0.8868903

# Hosmer-Lemeshow test:
source('AllGOFTests.R')
HLTest(melan.fit, g=10)
# 
# Hosmer and Lemeshow goodness-of-fit test with 10 bins
# 
# data:  melan.fit
# X2 = 21.852, df = 8, p-value = 0.005197
# 
# Warning message:
#   In HLTest(melan.fit, g = 10) :
#   Some expected counts are less than 5. Use smaller number of groups

HLTest(melan.fit, g=6)
# Hosmer and Lemeshow goodness-of-fit test with 6 bins
# 
# data:  melan.fit
# X2 = 19.005, df = 4, p-value = 0.000784
# 
# Warning message:
#   In HLTest(melan.fit, g = 6) :
#   Some expected counts are less than 5. Use smaller number of groups


