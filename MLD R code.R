#Setting work directory
setwd("C:/Users/Yahya/Desktop/MLD poject")

#Loading packages
library(stargazer)
library(tidyverse)

##Importing dataset
MLD<- read.csv("MLD Dataset .csv", header = T)

##Converting loan to value to percentage terms
MLD$LOANPRC<- MLD$LOANPRC * 100

##Creating subsample
MLDsubsample<- subset(MLD, MARRIED != "." & MALE != "." & LOANPRC <= 100 & (GDLIN == 1 | GDLIN == 0))
summary(MLDsubsample)
## Subset descriptive Statistics
stargazer(MLDsubsample, type = "text", title="Descriptive statistics", digits=3)


library(aod)
library(Rcpp)
library(ggplot2)


##Estimate Probit model
ProbitModel = glm(APPROVE ~ GDLIN + LOANPRC + OBRAT + BLACK + HISPAN , data = MLDsubsample, family = "binomial" (link = "probit"))
summary(ProbitModel)


#Estimate Full Logit Model
Full.LogitModel = glm(APPROVE ~  GDLIN + LOANPRC + OBRAT + BLACK + HISPAN, data = MLDsubsample, family = "binomial")
summary(Full.LogitModel)

#Estimate Reduced Logit Model
Reduced.LogitModel = glm(APPROVE ~  GDLIN + BLACK + HISPAN, data = MLDsubsample, family = "binomial")
summary(Reduced.LogitModel)


#Generate Odds Ratios
exp(coef(Full.LogitModel))


#Define prototypical loan applicants
Blackprot <- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 1, HISPAN = 0, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN=0)
Blackprot.Guidline <- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 1, HISPAN = 0, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN=1) 
Hispanicprot <- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 0, HISPAN = 1, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN= 0) 
Hispanicprot.Guidline <- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 0, HISPAN = 1, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN=1)
Whiteprot<- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 0, HISPAN = 0, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN=0)  
Whiteprot.Guidline <- data.frame(OBRAT=mean(MLDsubsample$OBRAT),BLACK = 0, HISPAN = 0, LOANPRC=mean(MLDsubsample$LOANPRC), GDLIN=1)

#predict probabilities for prototype individuals (logit model)

Blackprot$predictedprob <- predict (Full.LogitModel, newdata = Blackprot, type ="response")
Blackprot.Guidline$predictedprob <- predict (Full.LogitModel, newdata = Blackprot.Guidline, type ="response")
Hispanicprot$predictedprob <- predict (Full.LogitModel, newdata = Hispanicprot, type ="response")
Hispanicprot.Guidline$predictedprob <- predict (Full.LogitModel, newdata = Hispanicprot.Guidline, type ="response")
Whiteprot$predictedprob <- predict (Full.LogitModel, newdata = Whiteprot, type ="response")
Whiteprot.Guidline$predictedprob <- predict (Full.LogitModel, newdata = Whiteprot.Guidline, type= "response")

#summary of prototypes

Blackprot
Hispanicprot
Whiteprot
Blackprot.Guidline
Hispanicprot.Guidline
Whiteprot.Guidline


#predict probabilities for prototype individuals (probit model)
Blackprot$predictedprob <- predict (ProbitModel, newdata = Blackprot, type ="response")
Blackprot.Guidline$predictedprob <- predict (ProbitModel, newdata = Blackprot.Guidline, type ="response")
Hispanicprot$predictedprob <- predict (ProbitModel, newdata = Hispanicprot, type ="response")
Hispanicprot.Guidline$predictedprob <- predict (ProbitModel, newdata = Hispanicprot.Guidline, type ="response")
Whiteprot$predictedprob <- predict (ProbitModel, newdata = Whiteprot, type ="response")
Whiteprot.Guidline$predictedprob <- predict (ProbitModel, newdata = Whiteprot.Guidline, type= "response")

#summary
Blackprot
Hispanicprot
Whiteprot
Blackprot.Guidline
Hispanicprot.Guidline
Whiteprot.Guidline


#Generate Log-Likelihood
logLik(Full.LogitModel)
logLik(Reduced.LogitModel)

#Conduct Overall LR Test for Full Model
library(lmtest)
lrtest(Full.LogitModel)
lrtest(Reduced.LogitModel)

#Conduct LR Test Comparing Full and Reduced Models
library(lmtest)
lrtest(Full.LogitModel,Reduced.LogitModel)

