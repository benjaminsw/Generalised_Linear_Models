stairs.mat <- cbind(c(25,41,28),c(215,176,239))
a <- c("before", "during", "after")
when <- factor(a, levels=c("before", "during", "after"))

m1 <- glm( stairs.mat ~ when, family=binomial(link="logit"))
summary(m1)
