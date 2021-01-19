# Read in achievement score data
dat <- read.csv("achievement.csv")

# Scatterplot
plot(dat$x[dat$method=="A"], dat$y[dat$method=="A"], xlab="Initial aptitude score", 
    ylab="Achievement score", pch="A", xlim=c(min(dat$x)-1, max(dat$x)+1),
    ylim=c(min(dat$y)-1, max(dat$y)+1))
points(dat$x[dat$method=="B"], dat$y[dat$method=="B"], pch="B")
points(dat$x[dat$method=="C"]+0.1, dat$y[dat$method=="C"]+0.1, pch="C")

# Fit the models
m1 <- lm(y~ method+x, data=dat)
summary(m1)
anova(m1)

m2 <- lm(y~ x, data=dat)
summary(m2)
anova(m2)

# Compare models using an F-test:
anova(m2,m1)
