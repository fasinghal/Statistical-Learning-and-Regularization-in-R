
install.packages("ISLR", dependencies = T)
library(ISLR)
summary(Hitters)

#getting rid of insignificant variables & NAs

data = na.omit(Hitters)
summary(data)

dim(Hitters)
dim(data)

#lets predict salary
# using best subset selection 
install.packages("leaps", dependencies = T)
library(leaps)

#1. to evaluate all best subseu models

?regsubsets


regfitfull<-regsubsets(Salary~. , data=data) # considers up-to 8 variables by default
summary(regfitfull)

regfit.full<-regsubsets(Salary~., data=data, nvmax = 19)
reg.summary<-summary(regfit.full)
names(reg.summary)


#Use CP - Mellow's Cp measure - lower for better model

plot(reg.summary$cp ,xlab="Number of variables", ylab="CP Statistic")

#library(ggplot2)
#ggplot()+geom_point(aes(x = (1:length(reg.summary$cp)) ,y = reg.summary$cp ))
which.min(reg.summary$cp) # at 10 , better to use 6 (~1 SD less than cp)
min(reg.summary$cp)
#visualize Cp resulting choosing different variables
plot(regfit.full, scale = "Cp")
coef(regfit.full,10) #Coefficients related to best subset selection of degree 10

#Using BIC - min is better

plot(reg.summary$bic ,xlab="Number of variables", ylab="BIC Statistic")
plot(regfit.full, scale="bic")
which.min(reg.summary$bic)
min(reg.summary$bic)
coef(regfit.full, id = 6)

#using Adj R_sq : Higher is better
plot(reg.summary$adjr2 ,xlab="Number of variables", ylab="Adjusted R_sq Statistic")
plot(regfit.full, scale="adjr2")
which.max(reg.summary$adjr2)
max(reg.summary$adjr2)
coef(regfit.full, id = 10)

#Forward/Backward Selection
regfit.fwd<-regsubsets(Salary~. , data=data, nvmax = 19, method = "forward") 
reg.fwd.summary<-summary(regfit.fwd)
names(reg.fwd.summary)
plot(reg.fwd.summary$bic)

regfit.bwd<-regsubsets(Salary~. , data=data, nvmax = 19, method = "backward") 
reg.bwd.summary<-summary(regfit.fwd)
names(reg.bwd.summary)
plot(reg.bwd.summary$bic)
coef(regfit.bwd, 6)


