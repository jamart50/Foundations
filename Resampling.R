library(DBI)
library(tidyverse)
library(caret)
library(kernlab)
library(e1071)
library(ISLR)
library(MLmetrics)
library(klaR)
library(naivebayes)
library(car)
library(ipred)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

set.seed(1015)

# ----------- starting with a familiar dataset ------------ #

Advertising <- dbGetQuery(con2,"
SELECT 
[TV]
,[Radio]
,[Newspaper]
,[Sales]
FROM [dbo].[Advertising]
")

mFit <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
summary(mFit)

control <- trainControl(method="repeatedcv", number=10, repeats = 3) # explain number and repeats values
# train the model
model <- train(Sales ~ TV + Radio + Newspaper, data = Advertising, method="lm", preProcess="scale", trControl=control)
summary(model)



# ------------------ let's try mroe complex dataset

Auto <- dbGetQuery(con2,"
                    SELECT [make]
                   ,[body-style]
                   ,CAST([wheel-base]AS FLOAT) AS 'wheel-base'
                   ,CAST([engine-size] AS FLOAT) AS 'engine-size'
                   ,CAST([horsepower] AS FLOAT) AS 'horsepower'
                   ,CAST([peak-rpm] AS FLOAT) AS 'peak-rpm'
                   ,CAST([highway-mpg] AS FLOAT) AS 'highway-mpg'
                   ,CAST([price] AS FLOAT) AS 'price'
                   FROM [dbo].[AutoPricePrediction]
                   ")

Auto$`body-style` <- as.factor(Auto$`body-style`)
Auto$make <- as.factor(Auto$make)
Auto <- rowid_to_column(Auto, var="SampleID")

by_MakeStyle <- Auto %>% group_by(make, `body-style`) %>% dplyr::mutate(cnt = n()) %>% filter(cnt > 2) 
xTrain <- sample_frac(by_MakeStyle, .5) %>% ungroup()
xTest <- anti_join(by_MakeStyle, xTrain, by = "SampleID") %>% ungroup()

# chart out the data in terms of horsepower
# just focusing on scales
maxPrice <- round(max(Auto$price), -3)
minPrice <- round(min(Auto$price), -3)
interval <- round(((maxPrice-minPrice)/10), -2)
p <- ggplot(Auto, aes(x=horsepower, y=price))+geom_point() +
  scale_y_continuous(breaks=seq(minPrice, maxPrice, interval))
p

# run a single variable linear regression

model1 <- lm( price ~ horsepower, xTrain)
xTest1 <-  dplyr::select(xTest, price, horsepower)
xTest1$newPrice <- predict(model1, xTest1)

# chart the single variable LR and summarize performance / error

p <- p +geom_point() + geom_point(data=xTest1, aes(horsepower, newPrice), color = "blue")  + 
  geom_smooth(data=xTest1, aes(horsepower, newPrice), se=FALSE, color = "blue")
p

rmse(model1$residuals)

# let's see if we can improve with cv

ctrl<-trainControl(method = 'cv',number = 10) # you have to be careful with this and startified sampling
lmCVFit<-train(price ~ horsepower, data = Auto, method = 'lm', trControl = ctrl, metric='RMSE')
lmCVFit$results[2]


# and with Bootsrapping?

lmCVFit<-train( price ~ horsepower, data = Auto, method = 'lm', metric='RMSE')
lmCVFit$results[2]

# ------------------ let's expand to multivariate and see if we can improve RMSE -------------#

xTrain <- dplyr::select(xTrain, -SampleID)
xTest <- dplyr::select(xTest, -SampleID)

model3 <- lm(price ~., xTrain)
xTest$newY2 <- predict(model3, xTest)

# chart the MV model too, and compare statisics
p <- p + geom_point(data=xTest, aes(horsepower, newY2), color = 'green') + 
  geom_smooth(data=xTest, aes(horsepower, newY2), se=FALSE, color = "green")
p
rmse(model3$residuals)

Auto <- dplyr::select(Auto, -SampleID)

# we're just trying to create a partition for bootstrapping
cvIndex1 <- createFolds(paste(Auto$make, 
                              Auto$`body-style`,
                              list = FALSE))
ctrl<-trainControl(method = 'boot', index = cvIndex1, number = 2)
lmCVFit<-train(price ~., data = Auto, method = 'lm', trControl = ctrl, metric='RMSE')

# OK, lets simplify and understand what folds have what

cvIndex <- createFolds(Auto$make, 2, returnTrain = T)
fold1 <- Auto[cvIndex$Fold1,] %>% dplyr::count(make, `body-style`)
fold2 <- Auto[cvIndex$Fold2,] %>% dplyr::count(make, `body-style`)
test <- fold1 %>% left_join(fold2, by = c("make" = "make", "body-style" = "body-style"))

# this isn't going to work - see slide -----------------------------

# New subject Combinging CV with Tuning


library(ranger)
library(e1071)
library(randomForest)

Premiums <- read.csv("C:/Users/ellen/OneDrive/Documents/UH/Spring 2020/DA2/Section 1/Resampling/Data/Premiums2.csv")


# just to level-set with lm

testSplit <- .02 # we're going to use a small 'holdout' dataset for test (bc we're going to cross validate)
totalSampleSize <- nrow(Premiums)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(Premiums), testSampleSize)
indexes <- sample(1:nrow(Premiums[-tindexes,]), trainSampleSize)
PremiumTrain <- Premiums[indexes, ]
PremiumTest <- Premiums[tindexes,]


model4 <- lm(Premium ~., PremiumTrain)
PremiumTest$PredPremium <- predict(model4, PremiumTest)
rmse(PremiumTest$PredPremium - PremiumTest$Premium)

# so we're using a baseline RMSE of 280 (a little unfair to lm bc the holdout is unbalanced when it's not cv'd - but that's not the point here)

results <- matrix(ncol = 2, nrow=3)
results[1,1] <- 'svmPoly'
results[2,1] <- 'svmRadial'
results[3,1] <- 'rf'

cntrl <- trainControl(method = "cv", number = 50)

# just a single cv using svmPoly in case you don't want to process all the models
#caretMod <- train(Premium ~ . ,trControl = cntrl,  data = Premiums, method = 'svmPoly')
#PremiumTest$caretPred <- predict(caretMod, PremiumTest)
#results <- rmse(PremiumTest$caretPred - PremiumTest$Premium)

i <- 1
time1 <- Sys.time()
for(i in 1:nrow(results))
{
  caretMod <- train(Premium ~ . ,trControl = cntrl,  data = Premiums, method = results[i,1])
  PremiumTest$caretPred <- predict(caretMod, PremiumTest)
  results[i,2] <- round(rmse(PremiumTest$caretPred - PremiumTest$Premium),0)
}
time2 <- Sys.time()
time2 - time1 # Time difference of 6 mins

results

# ------------------------------ OK, now to classification data --------------------- #


quoteData <- dbGetQuery(con2,"
                        Select
                        ([dbo].[Quote].[Competitor_Quote] - [dbo].[Quote].[Quote]) AS QuoteDiff
                        ,[dbo].[Customer].[RSF]
                        ,[dbo].[Quote].[Result]
                        ,DATEDIFF(d, [dbo].[Quote].[Date_Submitted], [dbo].[Quote].[Date_Due]) AS RFPDiff
                        ,DATEDIFF(d, [dbo].[Quote].[ATP], [dbo].[Quote].[Date_Required] ) AS ATPDiff 
                        FROM [dbo].[Quote]
                        INNER JOIN 
                        [dbo].[Customer] ON [dbo].[Quote].[Customer_ID] = [dbo].[Customer].[Customer_ID]
                        ")

quoteData <- filter(quoteData, Result %in% c("W", "L"))
quoteData <- quoteData %>% rownames_to_column("SampleID")
quoteData$SampleID  <- as.numeric(quoteData$SampleID)
quoteData$QuoteDiff <- quoteData$QuoteDiff
quoteData$RSF <- as.integer(quoteData$RSF) # its really ordinal, but to make easier

testSplit <- .4
totalSampleSize <- nrow(quoteData)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(quoteData), testSampleSize)
indexes <- sample(1:nrow(quoteData[-tindexes,]), trainSampleSize)
xTrain <- quoteData[indexes, ]
xTest <- quoteData[tindexes,]
xTrain <- dplyr::select(xTrain, -SampleID) 
xTest <- dplyr::select(xTest, -SampleID) 
xTest$Result <- factor(xTest$Result)
xTrain$Result <- factor(xTrain$Result)


# ------------------------- Model Tuning  -----------------  #

# baseline with LDA

lda.fit <- lda(Result ~ QuoteDiff, xTrain) 
lda.fit
lda.pred <- predict(lda.fit, xTest)

confusionMatrix(lda.pred$class , xTest$Result, positive = "W")


# k fold manual (ugly - just for understanding)


xTrainkf <- dplyr::select(quoteData, QuoteDiff, RSF, Result, RFPDiff, ATPDiff)
xTrainkf$Result <- factor(xTrainkf$Result)
k <- 5
xTrainkf$id <- sample(1:k, nrow(xTrainkf), replace = TRUE)
prediction <- data.frame()
testsetCopy <- data.frame()
sumTab <- data.frame()
list <- 1:k

for(i in 1:k){
  trainingset <- subset(xTrainkf, id %in% list[-i])
  testset <- subset(xTrainkf, id %in% c(i))
  Quotemodel <- svm(Result ~., data = trainingset[,-6], kernel = 'radial')
  temp <- as.data.frame(predict(Quotemodel, testset[,-6]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,3]))
  result <- cbind(prediction, testsetCopy[, 1])
  names(result) <- c("Predicted", "Actual")
  tab <- table(result$Predicted, result$Actual, dnn = c('Predicted', 'Actual')) 
  TP <- tab[2,2]
  FP <- tab[2,1]
  FN <- tab[1,2]
  Precision <- TP/(TP+FP)  
  Recall <- TP / (TP+FN)
  F1 <- 2*(Precision*Recall)/(Precision+Recall)
  sumTab <- rbind(sumTab, F1)
}

# ------------------ end k fold manual (ugly - just for understanding)

cvAvg <- mean(sumTab[,1])
cvAvg # going to be around 80% which is a good appoximation

# now some caret cv

# create custom function to measure F1 - this is just so you're aware, you probabaly won't need this
#(you can measure AUC and others without creating a function, but since we're comparing f1...)

f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

tGrid <-  expand.grid(sigma=(1:10)*0.01, C= (5:10)*1)

cvCtrl <- trainControl(method = "repeatedcv", 
                       repeats = 10, 
                       summaryFunction = f1, 
                       classProbs = TRUE) 

mQuote <- data.matrix(dplyr::select(quoteData, QuoteDiff, RSF, RFPDiff, ATPDiff))
yQuote <- data.matrix(factor(quoteData$Result))


time1 <- Sys.time()
svmTune <- train(x = mQuote, y = yQuote, 
                 method = "svmRadial", 
                 preProc = c("center", "scale"), 
                 metric = "F1", 
                 trControl = cvCtrl,
                 tuneGrid = tGrid)

time2 <- Sys.time()
time2 - time1 # Time difference of 13 mins


svmTune
svmTune$finalModel
dfResults <- svmTune$results
plot(svmTune)
plot(svmTune, plotType = "level")

forecastCaret <- data.frame(forecast= predict(svmTune, mQuote), result  = factor(yQuote) )
confusionMatrix(forecastCaret$forecast , forecastCaret$result, positive = "W")








# ------------------- get student credit data for minority classification ------------------------- #

dfDefault <- Default

tst <- subset(dfDefault, default == 'No')
dfDefault <- Default
ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)


testSplit <- .4
totalSampleSize <- nrow(dfDefault)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(dfDefault), testSampleSize)
indexes <- sample(1:nrow(dfDefault[-tindexes,]), trainSampleSize)
xTrain2 <- dfDefault[indexes, ]
xTest2 <- dfDefault[tindexes,]

model <- naiveBayes(default ~ student + balance + income,  data = xTrain2)

xTest2$pred <- predict(model, xTest2[,-1], prob = TRUE)
confusionMatrix(xTest2$default, xTest2$pred, positive = "Yes")

# OK, this is really low - let's see if cv can help


k <- 5

cvTrain <- dfDefault

cvTrain$id <- sample(1:k, nrow(cvTrain), replace = TRUE)

prediction <- data.frame()
testsetCopy <- data.frame()
sumTab <- data.frame()
list <- 1:k

for(i in 1:k){
  trainingset <- subset(cvTrain, id %in% list[-i])
  testset <- subset(cvTrain, id %in% c(i))
  model <- naiveBayes(default ~ student + balance + income,  data = cvTrain)
  temp <- as.data.frame(predict(model, testset[,-1]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  result <- cbind(prediction, testsetCopy[, 1])
  names(result) <- c("Predicted", "Actual")
  tab <- table(result$Predicted, result$Actual, dnn = c('Predicted', 'Actual')) 
  TP <- tab[2,2]
  FP <- tab[2,1]
  FN <- tab[1,2]
  Precision <- TP/(TP+FP)  
  Recall <- TP / (TP+FN)
  F1 <- 2*(Precision*Recall)/(Precision+Recall)
  sumTab <- rbind(sumTab, TP)
}

tab


# how about up sampling?

dataset2 <- filter(xTrain2, default == "Yes")
smplSize <- 3 * nrow(dataset2)
tindexes <- sample(1:nrow(dataset2), smplSize, replace = TRUE)
Smpl1 <- dataset2[tindexes,]
xTrain2 = rbind(xTrain2, Smpl1);

model <- naiveBayes(default ~ student + balance + income,  data = xTrain2)

xTest2$pred <- predict(model, xTest2[,-1], prob = TRUE)

confusionMatrix(xTest2$default, xTest2$pred, positive = "Yes")

# that's an improvement.

# let's look at some other methods (bootstrap first)

train_control <- trainControl(method="boot", number=10)
# train the model
mod1 <- train(default ~ student + balance + income,  data = xTrain2, trControl=train_control, method="nb")
test3 <- data.frame(default = xTrain2$default, pred = factor(predict(mod1, xTrain2)))
confusionMatrix(test3$default, test3$pred, positive = "Yes")

warnings()
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
mod2 <- train(default ~ student + balance + income, data = xTest2, trControl=train_control, method="nb")
# summarize results
test4 <- data.frame(default = xTrain2$default, pred = factor(predict(mod2, xTrain2)))
confusionMatrix(test4$default, test4$pred, positive = "Yes")


