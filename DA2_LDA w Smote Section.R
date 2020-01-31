library(tidyverse)
library(MASS)
library(ISLR)
library(caret)


dfDefault <- Default

dfDefault %>% dplyr::count(default)

p <- ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)
p

pl1 <- ggplot(dfDefault, aes(balance, fill = default)) 
pl1 <- pl1 + geom_density(alpha = 0.2, adjust = 5 )
pl1


# -------------  Break for Bayesian Analysis going back to EDA



pg <- ggplot_build(p)
# this creates an R list, which is a little different data structure:(http://www.r-tutor.com/r-introduction/list)
# basically a way to store a bunch of different objects
pgData <- pg$data[[1]]
pgData %>%  dplyr::select(x, density)

pgData <- pgData %>% mutate(prob = (xmax- xmin)*density)
ProbAnalysis = pgData %>% group_by(group, xmax) %>% summarize(GrpProb = sum(prob, na.rm = T))
sum(ProbAnalysis$GrpProb)

# default = 2

p2 = ggplot(ProbAnalysis, aes(xmax, y = GrpProb, fill = factor(group))) +
  geom_bar(stat = "identity", position = "dodge")
p2

DefaultPop = filter(ProbAnalysis, group == 1)

# ----

p3 = ggplot(dfDefault, aes(balance)) +
  geom_histogram(binwidth = 500)
p3


pg <- ggplot_build(p3)
# this creates an R list, which is a little different data structure:(http://www.r-tutor.com/r-introduction/list)
# basically a way to store a bunch of different objects
pgData <- pg$data[[1]]
pgData %>%  dplyr::select(x, density)

pgData <- pgData %>% mutate(prob = (xmax- xmin)*density)
sum(pgData$prob)
pgData %>% filter(xmax == 1250) %>% dplyr::select(xmax, prob)

# ----------------- 


lda.fit <- lda(default ~ balance, data = dfDefault) 
lda.fit

lda.pred <- predict(lda.fit)
dfPred <- data.frame(lda.pred)
dfPred %>% dplyr::count(class)

pl1 <- pl1 + geom_vline(xintercept = mean(lda.fit$means) )
pl1
p <- p + geom_vline(xintercept = mean(lda.fit$means) )
p

# get decision rule (don't worry about doing this - just FYI)
A <- A <- mean(lda.fit$means)
B <- log(lda.fit$prior[2]) - log(lda.fit$prior[1])
s2.k <- t(tapply(dfDefault$balance, dfDefault$default, var)) %*% lda.fit$prior
C <- s2.k/(lda.fit$means[1] - lda.fit$means[2])
dr <- A + B * C
dr

p <- p + geom_vline(xintercept = dr )
p

confusionMatrix(factor(lda.pred$class) , factor(dfDefault$default), positive = "Yes")


# look at the data again
firstAnalysis <- as_tibble(cbind(as.character(lda.pred$class), as.character(dfDefault$default), lda.pred$posterior))
firstAnalysis <- cbind(firstAnalysis,dplyr::select(dfDefault, student, balance, income))
write_csv(firstAnalysis, "firstAnalysis.csv")

# let's adjust the threshold

pred <- rep('No', nrow(dfDefault))
pred[lda.pred$posterior[,2] >= 0.2] <- 'Yes' 
dfPred <- data.frame(pred)
dfPred %>% dplyr::count(pred)


confusionMatrix(factor(pred) , factor(dfDefault$default), positive = "Yes")


# now let's do this for real

testSplit <- .4
totalSampleSize <- nrow(dfDefault)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(dfDefault), testSampleSize)
indexes <- sample(1:nrow(dfDefault[-tindexes,]), trainSampleSize)
xTrain <- dfDefault[indexes, ]
xTest <- dfDefault[tindexes,]

#-------------------  SMOTE ----------------#

library(DMwR)
prop.table(table(xTrain$default))
smoteData <- SMOTE(default ~ ., data = xTrain, perc.over = 350, perc.under=130) # SMOTE only works with facdtors
prop.table(table(smoteData$default))
lda.fit <- lda(default ~ ., smoteData) 
lda.pred <- predict(lda.fit, xTest)
confusionMatrix((lda.pred$class), factor(xTest$default), positive = "Yes")

#----------------------------------



lda.fit <- lda(default ~ balance, xTrain) 
lda.fit

lda.pred <- predict(lda.fit, xTest)


# get decision rule

A <- A <- mean(lda.fit$means)
B <- log(lda.fit$prior[2]) - log(lda.fit$prior[1])
s2.k <- t(tapply(xTest$balance, xTest$default, var)) %*% lda.fit$prior
C <- s2.k/(lda.fit$means[1] - lda.fit$means[2])
dr <- A + B * C
dr

p <- p + geom_vline(xintercept = dr, color = 'red' )
p

# same place

confusionMatrix((lda.pred$class), factor(xTest$default), positive = "Yes")

# add more predictors (p)
# remember, visualization is gone in p>2

lda.fit <- lda(default ~ ., xTrain) 
lda.fit

lda.pred <- predict(lda.fit, xTest)


confusionMatrix((lda.pred$class), factor(xTest$default), positive = "Yes")


# get back orignial and look at it:
finalAnalysis <- as_tibble(cbind(as.character(lda.pred$class), as.character(xTest$default), lda.pred$posterior))
finalAnalysis <- cbind(finalAnalysis,dplyr::select(xTest, student, balance, income))

write_csv(finalAnalysis, "finalAnalysis.csv")





