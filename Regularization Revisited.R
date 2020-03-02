library(tidyverse)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# get some data

dfMPG <- mpg 

dfMPG = select(dfMPG, manufacturer, hwy, displ)

# create polynomial model

ModMatrix = model.matrix(hwy ~ manufacturer + displ + I(displ^2), data = dfMPG)

# pull the y values

vY  = dfMPG$hwy

# solve for Betas

mBeta = as.numeric(solve(t(ModMatrix) %*% ModMatrix) %*% (t(ModMatrix) %*% vY))

# error estimated from training

rmse(t(as.numeric(mBeta)%*%t(ModMatrix)) - dfMPG$hwy)

# add regularization

# you don't need to set up a loop and vector of thetas, you can just add any number you want
# (10 in this example - you can just arbitrarily pick this to start) 
# You still need the diagonal (d) so that you can mulitply the regularization factor by the model matrix 
# (which is why you get the dimensions from the model matrix - 
# except for the first one which is the intercept) 
# doing this one at a time is sometimes easier than looping through because you know 
# which way to go (is the error getting better or worse?)

n = ncol(ModMatrix)
d = diag(1,n,n)
d[1,1] = 0

vBetaReg1 = as.numeric(solve(t(ModMatrix) %*% ModMatrix + (10 * d)) %*% (t(ModMatrix) %*% vY))

# error estimated from training

rmse(t(as.numeric(vBetaReg1)%*%t(ModMatrix)) - dfMPG$hwy)

# here, the error in the regularized model is greater 
# that's what you want as the  model tends to less variable 
# so that when new data is acquired, the model will perform better.

# generate a random new dataset

dfMPG2 = mpg %>% mutate(hwy = 35 + (-3*displ) + rnorm(nrow(mpg), 0, 3))

# now create a new model matrix from the new data:
ModMatrix2 = model.matrix(hwy ~ manufacturer + displ + I(displ^2), data = dfMPG2)
# poly model error
rmse(t(as.numeric(mBeta)%*%t(ModMatrix2)) - dfMPG2$hwy)
# regularized model error
rmse(t(as.numeric(vBetaReg1)%*%t(ModMatrix2)) - dfMPG2$hwy)

