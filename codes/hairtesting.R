rawdata<-read.csv("hairbiomarkers.csv",header = TRUE)
y<- ifelse(rawdata[, 2] == "Positive", 1, 0)
dat<- as.data.frame(cbind(y, rawdata[, 3:10]))

set.seed(0956)

  # Training-test splitting
  # 60% of samples -> training
  # 40% of samples -> testing
  smp_size <- floor(0.6 * nrow(dat))
  index <- sample(seq_len(nrow(dat)),size=smp_size)
  train <- dat[index, ]
  test <- dat[-index, ]
  # Fitting a model with a single biomarker
  model<- glm(y~CDT,family=binomial,data=train)
  summary(model)
  # Predict results
  results_prob <- predict(model,newdata=test,type = "response")
  # If prob > 0.5 then 1 (chronic), else 0
  results <- ifelse(results_prob > 0.5,1,0)
  # Actual answers
  answers <- test$y
  # Accuracy calculation
  misClassificError <- mean(answers != results)
  # Collecting results
  acc <- 1-misClassificError
  acc
  # False positive and false negative error rates:
  all <- data.frame(answers=answers, results=results)
  trueneg <- all[answers==0,]
  truepos <- all[answers==1,]
  fpr <- mean(trueneg$answers!=trueneg$results)
  fpr
  fnr <- mean(truepos$answers!=truepos$results)
  fnr
  

library(ROCR)
# ROC and AUC
p <- predict(model, test, type="response")
pr <- prediction(p, test$y)
# TPR = sensitivity, FPR=specificity
prf<- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Repeat with EtG now:
model<- glm(y~EtG,family=binomial,data=train)
summary(model)
# Notice the warning -> separation issues and perfect prediction
# Predict results
results_prob <- predict(model,newdata=test,type = "response")
# If prob > 0.5 then 1 (chronic), else 0
results <- ifelse(results_prob > 0.5,1,0)
# Actual answers
answers <- test$y
# Accuracy calculation
misClassificError <- mean(answers != results)
# Collecting results
acc <- 1-misClassificError
acc

# False positive and false negative error rates:
all <- data.frame(answers=answers, results=results)
trueneg <- all[answers==0,]
truepos <- all[answers==1,]
fpr <- mean(trueneg$answers!=trueneg$results)
fpr
fnr <- mean(truepos$answers!=truepos$results)
fnr

library(ROCR)
# ROC and AUC
p <- predict(model, test, type="response")
pr <- prediction(p, test$y)
# TPR = sensitivity, FPR=specificity
prf<- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






