carb <- read.csv("CarbohydrateDiet.csv")

pairs(carb)

y <- carb$carbohydrate


# Model with all three covariates:

X <- as.matrix(cbind(rep(1, 20), carb[,2:4]))

t(X)%*%X

solve(t(X)%*%X)

t(X)%*%y

beta <- solve(t(X)%*%X)%*%t(X)%*%y

beta

# For residual sum of squares:

t(y)%*%y
20*mean(y)^2
t(b)%*% t(X)%*%y

RSS <- t(y)%*%y-t(b)%*% t(X)%*%y
RSS

sigmahatsq <- as.numeric(1/(20-4)*RSS)
sigmahatsq

# Variance-covariance matrix of b:
Vcov <- solve(t(X)%*%X)*sigmahatsq

# Take square root to obtain standard errors:

sqrt(diag(Vcov))

################################
# Model without age:


X0 <- as.matrix(cbind(rep(1, 20), carb[,3:4]))

t(X0)%*%X0

solve(t(X0)%*%X0)

t(X0)%*%y

beta0 <- solve(t(X0)%*%X0)%*%t(X0)%*%y

beta0

# For residual sum of squares:

t(beta0)%*% t(X0)%*%y

RSS0 <- t(y)%*%y-t(beta0)%*% t(X0)%*%y
RSS0

sigmahatsq0 <- as.numeric(1/(20-3)*RSS0)
sigmahatsq0

# Variance-covariance matrix of b:
Vcov0 <- solve(t(X0)%*%X0)*sigmahatsq0

# Take square root to obtain standard errors:

sqrt(diag(Vcov0))

# F-test comparing model without age to model including age:

F <- ((RSS0-RSS)/(4-3))/(RSS/(20-4))

pf(F,1,16, lower=FALSE)

qf(p=0.95,1,16)

# Using lm:

m1 <- lm(carbohydrate ~ age + weight+protein, data=carb)
summary(m1)
anova(m1)

# change the order in which we include variables in the model:
m1b <- lm(carbohydrate ~ weight+protein+age, data=carb)
anova(m1b)

m0 <- lm(carbohydrate ~ weight+protein, data=carb)
summary(m0)
anova(m0)

anova(m0,m1)

# Could also use the glm() function for these:
summary(glm(carbohydrate ~ age + weight+protein, data=carb, family="gaussian"))



# Diagnostic plots:
par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
png("carb.png", width = 7, height = 5.5, units="in", res=1024)
par(mfrow=c(2,2))
plot(m0, which=c(1,2,4))
dev.off()

plot(m1)

# Can also compute various versions of the residuals and diagnostics for influential points as follows:

resid(m1)
rstandard(m1)

dffits(m1)
cooks.distance(m1)
dfbetas(m1)

# tabulated:
influence.measures(m1)
