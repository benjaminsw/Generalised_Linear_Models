dat <- read.csv("twofactordata.csv")

# Interaction model:
m.int <- lm(data ~ factorA*factorB, data=dat)
summary(m.int)

# Additive model:
m.add <- lm(data ~ factorA+factorB, data=dat)
summary(m.add)

# Compare interaction model with additive model:

anova(m.add, m.int)

# Interaction term not necessary

# Model with just factor A:
m.a <- lm(data ~ factorA, data=dat)
summary(m.a)

# Compare with additive model:

anova(m.a, m.add)

# Factor B not necessary

# Model with just factor B:
m.b <- lm(data ~ factorB, data=dat)
summary(m.b)