# normal equations - R
# compare with python and slides

library(tidyverse)

Advertising <-  dbGetQuery(con2,"
SELECT 
                           [TV]
                           ,[Radio]
                           ,[Sales]
                           FROM [dbo].[Advertising]
                           ")

mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)


p = ggplot (aes(x = TV, y = Sales), data = Advertising) + 
  geom_point(aes(x = TV, y = yhat))
p

vY <- as.matrix(dplyr::select(Advertising, Sales)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV, Radio))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
vBeta

# using beats to predict

vBeta2 <- as.numeric(vBeta)
vBeta2
Advertising$neY <- t(vBeta2%*%t(mX)) # 3 columns on left * 3 rows on right (after transpose)
Advertising

p = p + 
  geom_point(aes(x = TV, y = neY), data = Advertising, color = "red")
p

