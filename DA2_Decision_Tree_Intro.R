# this is a look at the workings of a simple tree
# you'll have to run this on your computer 
# server doesn't do graphics outside RServer

library(tidyverse)
library(rpart.plot)
library(RColorBrewer)

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

fit <- rpart(Result ~ QuoteDiff + ATPDiff,
             data=quoteData,
             method="class")

rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE)

