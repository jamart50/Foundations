suppressMessages(library(tidyverse))
suppressMessages(library(plotly))

Intercept <- 3.5
b_1 <- 1.4
X = 3
y = Intercept + (b_1 * X)


# squared error cost function - Cost2 forces an intercept
cost2 <- function(X, y, theta) {
  sum ((((X %*% theta)+Intercept) - y)^2 ) / (2*length(y))
}

alpha <- 0.1 
# .3 is too much
# .001 and .2 are nice - .22 is interesting
# alpha controls the learning rate (step size) 
# try 0.2 (and note the spaces and how it jumps side to side 
# and also 0.005 wich is more typical of relative step 

num_iters <- 100
theta <- 0
chartPts <- NULL

epsilon <- .1
# .01
# epsion controls the threshold - try 2 (note how it's likely to totally miss
# or .01


tracerMat <- matrix(numeric(0), nrow = num_iters, ncol = 6)
colnames(tracerMat) <- c("i", "error", "delta", "theta", "cost", "sqrt(sum(delta^2))")
for (i in 1:num_iters) {
  error <- ((X %*% theta)+Intercept - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - (alpha * delta)
  cost <- cost2(X, y, theta)
  chartPts <- rbind(chartPts, c(theta, cost)) 
  if ((sqrt(sum(delta^2)))<=epsilon) {break} 
  tracerMat[i,] <- c(i, error, delta, theta, cost, sqrt(sum(delta^2)))
}

#thetaV <- seq(lmMod$coefficients[2]-range,lmMod$coefficients[2]+range, by=.1)
thetaV <- seq(0,3, by=.1)

FunchartPts <- NULL
for(i in 1:length(thetaV)){
  rwt <- cost2(X, y, thetaV[i])
  FunchartPts <- rbind(FunchartPts, c(thetaV[i], rwt)) 
}

dfFunChartPts <- data.frame(FunchartPts)
colnames(dfFunChartPts)[1] <- "Theta2"
colnames(dfFunChartPts)[2] <- "Cost"

dfChartPts <- data.frame(chartPts)
colnames(dfChartPts)[1] <- "Theta2"
colnames(dfChartPts)[2] <- "Cost"


round(theta,1)

p = plot_ly(dfFunChartPts, x = ~Theta2, y = ~Cost, type = 'scatter', mode = 'lines+markers', name = 'function') %>%
  add_trace(data = dfChartPts, y = ~Cost, mode = 'markers', name = 'GD pts')
p

p = plot_ly(dfFunChartPts, x = ~Theta2, y = ~Cost, type = 'scatter', mode = 'lines+markers', name = 'function') 
p



