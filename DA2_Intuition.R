library(tidyverse)
library(rstan)
library(shinystan)
library(gridExtra)
library(caret)
library(cowplot)

set.seed(116)

quoteData <- dbGetQuery(con2,"
Select
                        [dbo].[Quote].[Competitor_Quote]
                        ,[dbo].[Quote].[Quote]
                        ,([dbo].[Quote].[Competitor_Quote] - [dbo].[Quote].[Quote]) AS QuoteDiff
                        ,[dbo].[Customer].[RSF]
                        ,[dbo].[Quote].[Result]
                        ,[dbo].[Quote].[Date_Submitted]
                        , [dbo].[Quote].[Date_Due]
                        ,DATEDIFF(d, [dbo].[Quote].[Date_Submitted], [dbo].[Quote].[Date_Due]) AS RFPDiff
                        ,[dbo].[Quote].[ATP]
                        ,[dbo].[Quote].[Date_Required]
                        ,DATEDIFF(d, [dbo].[Quote].[ATP], [dbo].[Quote].[Date_Required] ) AS ATPDiff 
                        FROM [dbo].[Quote]
                        INNER JOIN 
                        [dbo].[Customer] ON [dbo].[Quote].[Customer_ID] = [dbo].[Customer].[Customer_ID]
                        ")

plotQuote <- quoteData
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
train$Prob <- predict(glm.fit, type = "response")
ggplot(train, aes(x=QuoteDiff, y=Prob)) + geom_point() + geom_smooth(se = F)


plotQuote <- filter(plotQuote, Result %in% c("W", "L"))

p1 <- ggplot(plotQuote) + 
  geom_jitter(aes(x = Result, y = ATP),color = 'red', width = .03)+
  geom_jitter(aes(x = Result, y = Date_Required),color = 'black', width = .03)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

p2 <- ggplot(plotQuote) + 
  geom_jitter(aes(x = Result, y = ATPDiff), width = .03)


p3 <- ggplot(plotQuote) + 
  geom_jitter(aes(x = Result, y = Competitor_Quote),color = 'red', width = .03)+
  geom_jitter(aes(x = Result, y = Quote),color = 'black', width = .03)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

p4 <- ggplot(plotQuote) + 
  geom_jitter(aes(x = Result, y = QuoteDiff), width = .03)

plot_grid(p1, p2, p3, p4, align = 'h')



