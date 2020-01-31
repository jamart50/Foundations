library(tidyverse)
library(rstan)
library(shinystan)
library(gridExtra)
library(caret)
library(cowplot)

set.seed(116)

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
quoteData$QuoteDiff <- quoteData$QuoteDiff/1000
quoteData$RSF <- as.integer(quoteData$RSF) # its really ordinal, but to make easier
train <- sample_n(quoteData, nrow(quoteData)-100)
test <- quoteData %>% anti_join(train, by = "SampleID")

glm.fit <- glm(Result ~ QuoteDiff + RSF + RFPDiff + ATPDiff, data = train, family = binomial)
summary(glm.fit)


testPred <- predict(glm.fit, type = "response", newdata = test, se.fit = T)

test$Prob <- testPred$fit
test$lcl <- test$Prob - testPred$se.fit
test$ucl <- test$Prob + testPred$se.fit

#test$Prob <- predict(glm.fit, type = "response", newdata = test, se.fit = T)

ggplot(test, aes(x=QuoteDiff, y=Prob)) + geom_point() + 
  geom_smooth(method="glm", method.args=list(family=quasibinomial)) +
  geom_smooth(aes(x = QuoteDiff, y = lcl), method="glm", method.args=list(family=quasibinomial)) +
  geom_smooth(aes(x = QuoteDiff, y = ucl), method="glm", method.args=list(family=quasibinomial))


glm.fit$coefficients
alpha <- glm.fit$coefficients[1]
beta <- glm.fit$coefficients[2:5]

# ------------- this gives you the confidence intervals for the coefficients ------------------ #

confint(glm.fit)

# ------------- developing matrix algebra equations  ------------------ #

test$Prob <- predict(glm.fit, type = "response", newdata = test)

# just comparing matrix algebra answer for reference
tst1 <- data.matrix(select(test, QuoteDiff, RSF, RFPDiff, ATPDiff))
bet1 <- as.numeric(beta)
test$laProb <- exp(alpha[1] + t(bet1%*%t(tst1)))/(1+exp(alpha[1] + t(bet1%*%t(tst1))))

# score results
test$PResult <- ifelse(test$Prob < .5, 0, 1)
# check metrics
confusionMatrix(factor(test$PResult) , factor(test$Result))



