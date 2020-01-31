library(tidyverse)
library(stringr)
library(kernlab)
library(caret)



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
quoteData$Result <- as.integer(factor(quoteData$Result))-1
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


mQuote <- data.matrix(dplyr::select(xTrain,QuoteDiff, RSF, RFPDiff, ATPDiff))
mQuoteTest <- data.matrix(dplyr::select(xTest,QuoteDiff, RSF, RFPDiff, ATPDiff))
yQuote <- data.matrix(dplyr::select(xTrain,Result))
yQuoteTest <- data.matrix(dplyr::select(xTest,Result))

# ------------ testing linear kernel ------------------- #

t2 <- ksvm(mQuote, yQuote,type="C-svc", C=10, kernel=vanilladot(), scale=c())
result2 <- data.frame(predict(t2, mQuoteTest))
result2 <- cbind(result2, yQuoteTest)
confusionMatrix(factor(result2[,1]) , factor(result2[,2]))

# ------------ rbf kernel ------------------- #

# create rbf  kernel function
rbf <- function(x,y) exp(-0.1 * sum((x-y)^2))
class(rbf) <- "kernel"
#just for reference - you don't need to actaully create the kernel
k2 <- kernelMatrix(rbf, mQuote)
dim(k2)
X <- k2
Y <- yQuote
t3 <- ksvm(mQuote, yQuote, type="C-svc", C=10, kernel=rbf, scale=c())
result3 <- data.frame(predict(t3, mQuoteTest))
result3 <- cbind(result3, yQuoteTest)
confusionMatrix(factor(result3[,1]) , factor(result3[,2]))


# ------------------- quadratic kernel --------------------#

kfunction <- function(linear =0, quadratic=0)
{
  k <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  k
}

#just for reference - you don't need to actaully create the kernel
X3 <- kernelMatrix(kfunction(0,1), mQuote)
dim(X3)
t4 <- ksvm(mQuote, yQuote, type="C-svc", C=10, kernel=kfunction(0,1), scale=c())
result4 <- data.frame(svm = predict(t4, mQuoteTest))
result4 <- cbind(result4, yQuoteTest)
confusionMatrix(factor(result4$svm) , factor(result4$Result))


# ------------------- polynomial kernel --------------------#

X3 <- kernelMatrix(kfunction(0,1), mQuote)
dim(X3)

t4 <- ksvm(mQuote, yQuote, type="C-svc", C=10, kernel='polydot', scale=c())
result4 <- data.frame(predict(t4, mQuoteTest))
result4 <- cbind(result4, yQuoteTest)

confusionMatrix(factor(result4[,1]) , factor(result4[,2]))


# ------------------------- Model Tuning -----------------  #

library(e1071)

tuneQuote <- dplyr::select(quoteData, Result, QuoteDiff, RSF, RFPDiff, ATPDiff)

tunedModel <- tune.svm(Result ~., data = tuneQuote, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tunedModel)
gParam <- tunedModel$best.parameters[1]
gParam
CParam <- tunedModel$best.parameters[2]
CParam

# Apply tuned paramters to ksvm

t5 <- ksvm(mQuote, yQuote, type="C-svc", C=CParam, kernel=rbf, scale=c(), gamma = gParam)
result5 <- data.frame(predict(t5, mQuoteTest))
result5 <- cbind(result5, yQuoteTest)
confusionMatrix(factor(result5[,1]) , factor(result5[,2]))


# ------------------- you can also use the svm in the e1071 package ---------------#
# a little easier when dealing with dataframe based data 
# when you're  not going to use custom kernels 


dfTuneQuote <- data.frame(tuneQuote)
dfTuneQuote$Result <- factor(dfTuneQuote$Result)
model <- svm(Result ~., data = dfTuneQuote, gamma = gParam, cost = CParam)
dfTest <- data.frame(xTest[,-1])
dfTest$Predict <- predict(model, dfTest)
confusionMatrix(factor(dfTest$Result) , factor(dfTest$Predict))


# so it appears that 80% accuracy is the best we'll get from svm.
# however, this is before resampling and extended tuning (next section)

Coef <- data.frame(model$coefs)
