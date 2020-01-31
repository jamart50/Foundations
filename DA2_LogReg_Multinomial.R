library(tidyverse)
library(VGAM)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2020/DA2/Section 1/Classification and SVM/Data")
prog <- read.csv("programs.csv")
prog$prog2 <- relevel(prog$prog, ref = "academic")

fit.prog <- vglm(prog ~ math, family = multinomial, data = prog)
coef(fit.prog, matrix = TRUE)
summary(fit.prog)
vglmP <- predictvglm(fit.prog, type = "response")

tstRec <- prog[1,]
L1 <- fit.prog@coefficients[1] + fit.prog@coefficients[3]*tstRec[8]
L2 <- fit.prog@coefficients[2] + fit.prog@coefficients[4]*tstRec[8]
denom <- 1 + exp(L1) + exp(L2)
pihat1 <- exp(L1)/denom
pihat2 <- exp(L2)/denom
pihat3 <- 1/denom

tst <- rbind(vglmP[1,], c(pihat1, pihat2, pihat3))
tst

# -----------------------------------

fit.prog <- vglm(prog ~ ses + write, family = multinomial, data = prog)
vglmP <- predictvglm(fit.prog, type = "response")
prog$Predict <-  colnames(vglmP)[max.col(vglmP,ties.method="first")]
table(prog$Predict, prog$prog2)

