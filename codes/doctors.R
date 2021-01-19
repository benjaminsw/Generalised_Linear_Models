par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)


doctors <- read.csv("doctors.csv")

# Death rate per 100,000 person-years:
doctors$rates <- doctors$deaths/doctors$person.years*100000

# Plotted death rates by smoking status:
png("deathrates.png", width = 7, height = 5.5, units="in", res=1024)

plot(doctors$rates[doctors$smoking=="smoker"], pch=17, xlab="Age", xaxt="n", ylab="Deaths per 100,000 person-years", ylim=c(0,2200))
points(doctors$rates[doctors$smoking=="non-smoker"], pch=19)

axis(side=1, at=1:5,labels=doctors$age[doctors$smoking=="smoker"])

legend("topleft", legend=c("smokers", "non-smokers"), pch=c(17,19))

dev.off()

# Define an ordinal age variable and its square:

doctors$agecat<-1
doctors$agecat[doctors$age=="45 to 54"] <-2
doctors$agecat[doctors$age=="55 to 64"] <-3
doctors$agecat[doctors$age=="65 to 74"] <-4
doctors$agecat[doctors$age=="75 to 84"] <-5

doctors$agesq <- doctors$agecat^2

doctors

# Define smkage variable equal to agecat for smokers and zero for non-smokers (this is the same as including the iteraction term agecat:smoking in the model):

doctors$smkage <- 0
doctors$smkage[doctors$smoking=="smoker"] <- doctors$agecat[doctors$smoking=="smoker"]

doctors

# Fit a Poisson regression model:

m1 <- glm(deaths~ agecat+agesq+ smoking+smkage+offset(log(person.years)), family=poisson, data=doctors)

summary(m1)

# Call:
# glm(formula = deaths ~ agecat + agesq + smoking + smkage + offset(log(person.years)),
#     family = poisson, data = doctors)
# 
# Deviance Residuals:
#        1         2         3         4         5         6         7         8
#  0.43820  -0.27329  -0.15265   0.23393  -0.05700  -0.83049   0.13404   0.64107
#        9        10
# -0.41058  -0.01275
# 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
# (Intercept)   -10.79176    0.45008 -23.978  < 2e-16 ***
# agecat          2.37648    0.20795  11.428  < 2e-16 ***
# agesq          -0.19768    0.02737  -7.223 5.08e-13 ***
# smokingsmoker   1.44097    0.37220   3.872 0.000108 ***
# smkage         -0.30755    0.09704  -3.169 0.001528 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 935.0673  on 9  degrees of freedom
# Residual deviance:   1.6354  on 5  degrees of freedom
# AIC: 66.703
# 
# Number of Fisher Scoring iterations: 4

# Note that you could fit the model without smkage, but with an interaction term between agecat and smoking:

summary(glm(deaths~ agecat+agesq+ smoking +agecat:smoking+offset(log(person.years)), family=poisson, data=doctors) )
# Call:
# glm(formula = deaths ~ agecat + agesq + smoking + agecat:smoking +
#     offset(log(person.years)), family = poisson, data = doctors)
# 
# Deviance Residuals:
#        1         2         3         4         5         6         7         8
#  0.43820  -0.27329  -0.15265   0.23393  -0.05700  -0.83049   0.13404   0.64107
#        9        10
# -0.41058  -0.01275
# 
# Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)
# (Intercept)          -10.79176    0.45008 -23.978  < 2e-16 ***
# agecat                 2.37648    0.20795  11.428  < 2e-16 ***
# agesq                 -0.19768    0.02737  -7.223 5.08e-13 ***
# smokingsmoker          1.44097    0.37220   3.872 0.000108 ***
# agecat:smokingsmoker  -0.30755    0.09704  -3.169 0.001528 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
#     Null deviance: 935.0673  on 9  degrees of freedom
# Residual deviance:   1.6354  on 5  degrees of freedom
# AIC: 66.703
# 
# Number of Fisher Scoring iterations: 4


# Fitted probabilities
fitted(m1)

pearsonres <- (doctors$deaths-fitted(m1))/sqrt(fitted(m1))
pearsonres
# You can also get these directly:

pres <- resid(m1, type="pearson")
pres

# Combine together the age category, smoking status, observed and expected number of deaths and Pearson and deviance residuals

round(cbind(doctors$agecat, doctors$smoking, doctors$deaths, fitted(m1), pearsonres, resid(m1)), 3)

# library(xtable)
# xtable(cbind(doctors$agecat, doctors$smoking, doctors$deaths, fitted(m1), pearsonres, resid(m1)), digits=c(0,0,0,0,2,3,3))

# Pearson chi-squared statistic:

X2 <- sum(pres^2)
X2
# [1] 1.550251


