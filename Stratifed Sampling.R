library(DBI)
library(tidyverse)
library(caret)
library(kernlab)
library(e1071)
library(ISLR)
library(klaR)
library(naivebayes)
library(car)

rmse <- function(error)
{
  sqrt(mean(error^2))
}


# ---------------------- Stratified Sampling in tidyverse -----------------------#


homeSales <- read_csv(file="C:/Users/ellen/OneDrive/Documents/UH/Spring 2020/DA2/Section 1/Resampling/Data/HomeSales2.csv")
homeSales$ZIP <- as.factor(homeSales$ZIP) # Zip Codes are not numberical - there's no meaning to the quantity
homeSales %>% dplyr::count(ZIP) # we can see that we'll have problems
xTrain <- sample_frac(homeSales, .5)
xTest <- anti_join(homeSales, xTrain, by = "LISTID")
model <- lm( SALE_PRICE~ZIP + SQF + YEAR_BUILT, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)


# so we have to stratify so that the training and test set are balanced

by_Zip <- homeSales %>% group_by(ZIP) %>% dplyr::mutate(cnt = n()) %>% filter(cnt > 2) 
xTrain <- sample_frac(by_Zip, .5)
xTest <- anti_join(by_Zip, xTrain, by = "LISTID")
model <- lm( SALE_PRICE~ZIP + SQF + YEAR_BUILT, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)
p <- ggplot(data=xTest, aes(SQF, PREDSALEPRICE)) + geom_point(alpha = 0.2)
p
p <- p + geom_smooth(data=xTest, aes(SQF, PREDSALEPRICE), se=FALSE)
p




summary(model)
rmse(model$residuals)

# so we have to be aware of the balance within variables (esp dummy variables)
# as well as classes in classification (like LDA previously)

# you can use dplyr to balance multiple variables too:

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

Auto$`body-style` <- as.factor(Auto$`body-style`) # factors easier than character
Auto$make <- as.factor(Auto$make)
Auto %>% group_by(make, `body-style`) %>% dplyr::count(make) 
# we can see that we'll have problems
# anytime obs < folds or groups, you have a problem!!!

Auto <- rowid_to_column(Auto, var="SampleID") # this creates a primary key (you have to be careful with rownames)
by_MakeStyle <- Auto %>% group_by(make, `body-style`) %>% dplyr::mutate(cnt = n()) %>% filter(cnt > 2) 
xTrain <- sample_frac(by_MakeStyle, .5)
xTest <- anti_join(by_MakeStyle, xTrain, by = "SampleID")
model <- lm(price ~ make + `body-style`+ horsepower, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)
p <- ggplot(data=xTest, aes(horsepower, PREDSALEPRICE)) + geom_point(alpha = 0.2)
p
p <- p + geom_smooth(data=xTest, aes(horsepower, PREDSALEPRICE), se=FALSE)
p
summary(model)
rmse(model$residuals)
