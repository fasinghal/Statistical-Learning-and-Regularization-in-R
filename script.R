
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

which.min(reg.summary$cp)


