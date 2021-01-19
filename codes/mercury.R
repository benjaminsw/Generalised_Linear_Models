####################################################################
################################################################
# Mercury exposure in dentists

merc <- read.csv("mercuryfull.csv")
head(merc)

# Graphically summarise the difference in log mercury concentration
# between randomly sampled dentists, volunteer dentists and controls
# and comment on assumptions for subsequent analysis:

boxplot(UriMerc ~ Group, data=merc)

qqnorm(merc$UriMerc[merc$Group==1])
qqline(merc$UriMerc[merc$Group==1])

qqnorm(merc$UriMerc[merc$Group==2])
qqline(merc$UriMerc[merc$Group==2])

qqnorm(merc$UriMerc[merc$Group==3])
qqline(merc$UriMerc[merc$Group==3])

# Do the three groups appear roughly normal, and with the same variance?

# Compare randomly sampled dentists and volunteer dentists:

dentists <- merc[merc$Group!=3,]
dim(dentists)
# Age comparison:
d1 <- lm(Age ~ Group, data=dentists)
summary(d1)
# Mercury comparison:
d2 <- lm(UriMerc ~ Group, data=dentists)
summary(d2)
# Reaction time comparison:
d3 <- lm(ReacTim ~ Group, data=dentists)
summary(d3)

# Any differences?

# Combine the two groups of dentists into one:

merc$Dentist <- 0
merc$Dentist[merc$Group!=3] <-1
head(merc)
tail(merc)

# Fit a linear model to compare dentists and controls 
# after adjusting for age:

l1 <- lm(UriMerc ~ Dentist + Age, data=merc)
summary(l1)

# Fit a linear model for dentists only, to see if there is a 
# mercury effect on reaction times after we control for age:

l2 <- lm(ReacTim ~ UriMerc + Age, data=dentists)
summary(l2)

# Diagnostic plots:
plot(l2)

# Fit a model to compare reaction times of dentists and controls,
# adjusting for age

# Interaction model:

l3a <- lm(ReacTim ~ Dentist*Age, data=merc)
summary(l3a)

# Additive model:
l3b <- lm(ReacTim ~ Dentist +Age, data=merc)
summary(l3b)

# Model with Age only:
l3c <- lm(ReacTim ~ Age, data=merc)
summary(l3c)

# Model with Group only:
l3d <- lm(ReacTim ~ Dentist, data=merc)
summary(l3d)

# Model with intercept only:
l3e <- lm(ReacTim ~ 1, data=merc)
summary(l3e)

plot(merc$Age, merc$ReacTim, xlab="Age (years)", 
    ylab="Reaction time (milliseconds)", pch=merc$Dentist+20)
    
#######################################    
# Using rpanel to visualise the different models:

attach(merc)
library(rpanel)
rp.ancova(ReacTim, Age, Dentist)



