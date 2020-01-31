library(tidyverse)
library(MASS)
library(ISLR)

# from book

dfDefault <- Default
glm.fit <- glm(default ~ student, data = dfDefault, family = binomial)
summary(glm.fit)

dfDefault$Prob <- predict(glm.fit, type = "response")
ggplot(dfDefault, aes(x=balance, y=Prob)) + geom_point()  


#--

glm.fit <- glm(default ~ balance, data = dfDefault, family = binomial)
summary(glm.fit)

dfDefault$Prob <- predict(glm.fit, type = "response")
ggplot(dfDefault, aes(x=balance, y=Prob)) + geom_point()  

# ----------------------  multiple logistic regression ---------------- #

mglm.fit <- glm(default ~ student + balance + income, data = dfDefault, family = binomial)
summary(mglm.fit)
dfDefault$mProb <- predict(mglm.fit, type = "response")

alpha <- mglm.fit$coefficients[1]
beta <- mglm.fit$coefficients[2:4]

test <- dfDefault
test$student <- as.integer(dfDefault$student)-1

test$tProb <-  (exp(alpha[1]+(beta[1]*test[,2]+ beta[2]*test[,3]+beta[3]*test[,4])))/
  (1+(exp(alpha[1]+(beta[1]*test[,2]+ beta[2]*test[,3]+beta[3]*test[,4]))))
# or using matrix algebra to make this easier:
tst1 <- data.matrix(test[,2:4])
bet1 <- as.numeric(beta)
test$tmProb <- exp(alpha[1] + t(bet1%*%t(tst1)))/(1+exp(alpha[1] + t(bet1%*%t(tst1))))
# looks like just as much effort, but it's not when you're working!!
ggplot(test, aes(x=balance, y=tmProb, color = factor(student))) + geom_point() 

