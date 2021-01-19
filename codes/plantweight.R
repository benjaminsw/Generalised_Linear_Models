plant <- read.csv("plant.csv")

m1 <- lm(weight ~ group, data =plant)
summary(m1)

anova(m1)

m0 <- lm(weight ~ 1, data =plant)
summary(m0)

anova(m0,m1)

# The F-test tells us that not all means are the same, 
# but it does not tell us which means differ. To find out, 
# we can do pairwise comparisons, with a multiple 
# comparison adjustment.

# Here there are three levels of the factor, and therefore 
# three possible pairwise comparisons, so we adjust 
# the significance level by dividing by 3.

# To compare Control with Treatment A, let's obtain 
# a confidence interval for alpha2-mu. For that we use
# the parameter estimate and standard error of (alpha2-mu), 
# which are given in the lm output.

# The confidence interval is of the form 
# alpha2 +- t * se(alpha2)
# where t is the appropriate quantile of the 
# t(n-p) distribution.

-0.3710 -qt(df=27,1-0.025/3)*0.2788
# [1] -1.082626

-0.3710 +qt(df=27,1-0.025/3)*0.2788
# [1] 0.3406255

# This CI includes 0, so there is no significant difference 
# between Control and Treatment A.

# Similarly we can compare Control and Treatment B by
# using alpha3 and se(alpha3)

0.4940-qt(df=27,1-0.025/3)*0.2788
#[1] -0.2176255

0.4940+qt(d=27,1-0.025/3)*0.2788
#[1] 1.205626

# Again, this interval includes zero (just about), so the difference 
# between Control and Treatment B isn't quite significant.

# Now to get a confidence interval for alpha3-alpha2, we need to do 
# a bit more work:

# Var(alpha3-alpha2)=Var(alpha3)+Var(alpha2)-2Cov(alpha3,alpha2) 
# where the values can be obtained from the variance-covariance matrix 
# of the parameter estimates:
vcov(m1)

#                 (Intercept) groupTreatmentA groupTreatmentB
# (Intercept)      0.03885959     -0.03885959     -0.03885959
# groupTreatmentA -0.03885959      0.07771919      0.03885959
# groupTreatmentB -0.03885959      0.03885959      0.07771919

# So, Var(alpha3-alpha2) is

0.07771919+0.07771919-2*0.03885959
# [1] 0.0777192

# and its square root is 
# [1] 0.2787816

# which in this case is the same as the standard errors 
# for alpha2 and alpha3

# The confidence interval is

0.4940-(-0.3710)-qt(df=27,1-0.025/3)*0.2787816
# [1] 0.1534215

0.4940-(-0.3710)+qt(df=27,1-0.025/3)*0.2787816
# [1] 1.576579

# In this case the confidence interval is entirely positive, i.e.
# the difference in means is between Treatment A and Treatment B.

# Another possibility for multiple comparison adjustment is 
# Tukey's method, which is slightly less conservative than 
# Bonferroni:

TukeyHSD(aov(m1))
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = m1)
# 
# $group
#                         diff        lwr       upr     p adj
# TreatmentA-Control    -0.371 -1.0622161 0.3202161 0.3908711
# TreatmentB-Control     0.494 -0.1972161 1.1852161 0.1979960
# TreatmentB-TreatmentA  0.865  0.1737839 1.5562161 0.0120064