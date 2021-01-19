ulc<- data.frame(ulcer=factor(rep(c(rep("gastric",2), rep("duodenal",2)),2)),
                 group=factor(rep(c("control","case"),4)),
                 aspirin= factor(c(rep("no",4),rep("yes",4))),
                 freq= c(62,39,53,49,6,25,8,8))

asptab <- xtabs(freq ~ group+aspirin+ulcer, data=ulc)

# Any log-linear model of these data should include terms corresponding
# to the fixed marginal totals.
# Here we need to include group, ulcer and their interaction.

# Minimal model:
asp1 <- glm(freq~ ulcer*group, family=poisson, data=ulc)
summary(asp1)

# Models including aspirin too:
asp2 <- glm(freq~ ulcer*group+aspirin, family=poisson, data=ulc)
summary(asp2)

asp3 <- glm(freq~ ulcer*group+aspirin*group, family=poisson, data=ulc)
summary(asp3)

asp4 <- glm(freq~ ulcer*group+aspirin*ulcer+aspirin*group, family=poisson, data=ulc)
summary(asp4)

anova(asp3,asp4)

fitted(asp4)