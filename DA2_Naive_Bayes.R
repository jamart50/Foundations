library(tidyverse)
library(ISLR)
library(e1071)
library(caret)

dfDefault <- Default

tst <- subset(dfDefault, default == 'No')
dfDefault <- Default

ProbDef = nrow(filter(dfDefault, default == "Yes"))/nrow(dfDefault)

p = ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)
p



testSplit <- .4
totalSampleSize <- nrow(dfDefault)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(dfDefault), testSampleSize)
indexes <- sample(1:nrow(dfDefault[-tindexes,]), trainSampleSize)
xTrain <- dfDefault[indexes, ]
xTest <- dfDefault[tindexes,]

model <- naiveBayes(default ~ student + balance + income,  data = xTrain)
xTest$pred <- predict(model, xTest[,-1], prob = TRUE)





confusionMatrix((xTest$default), factor(xTest$pred), positive = "Yes")


probs <- predict(model, xTest[,-1], type = 'raw')

library(ROCR)

# ROC Curve
yTest <- xTest$default
pred <- prediction(probs[, "Yes"], xTest$default)
perf_nb <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf_nb)
auc<- performance(pred,"auc")
auc

# this all looks good, but fp rate is 80%, 
# so 80% of the people we would predit would default, would not
# we're losing too much business!


# -----------------------  multiclass -----------------------#

dfDefault <- Default

model <- naiveBayes(default ~ student + balance + income,  data = dfDefault)
probs <- predict(model, dfDefault[,-1], type = 'raw')
dfDefault <- cbind(dfDefault, probs)
multiAnalysis <- mutate(dfDefault, route = ifelse((Yes < 0.2), "Accept", ifelse((Yes >= .2 & Yes <= .5), "Review", "Reject")))
multiAnalysis  %>% group_by(route) %>% summarize(count = n())
dfRouting <- dplyr::select(multiAnalysis, route, student, balance, income)
dfRouting$route <- as.factor(dfRouting$route)
  
testSplit <- .4
totalSampleSize <- nrow(dfRouting)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(dfRouting), testSampleSize)
indexes <- sample(1:nrow(dfRouting[-tindexes,]), trainSampleSize)
xTrain <- dfRouting[indexes, ]
xTest <- dfRouting[tindexes,]

model <- naiveBayes(route ~ student + balance + income,  data = xTrain)
model

xTest$pred <-  predict(model, xTest[,-1], prob = TRUE)

xTest %>% group_by(pred, route) %>% summarize(count = n())


