
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

#Validation Techniques 

set.seed(1)
num.obs <- dim(data)[1]
train = sample( num.obs, 180, replace = F)
test = -train


regfit.best = regsubsets(Salary~., data[train,], nvmax =19 )

#lests build of testdataset

test.mat <- model.matrix(Salary~., data = data[test,])
head(test.mat)

val.err<-rep(0,19)

for(i in 1:19){
  coefi=coef(regfit.best,id = i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.err[i] = mean((data$Salary[test] - pred)^2)
}

which.min(val.err)
min(val.err)

coef(regfit.best, 5)


#Ridge and lasso regularization
# alpha = 0 Ridge
# alpha =1 Lasso

install.packages("glmnet", dependencies = T)
library(glmnet)


grid = 10^(seq(from =10, to =-2, length=100))


x = model.matrix(Salary~., data = data)[,-1] # exclude intercept
y = data$Salary

ridge.model <- glmnet(x = x , y = y, alpha = 0, lambda = grid) # ridge regression
dim(coef(ridge.model)) # 20 * 100 for 100 lambda  it generated 19 coef for 19 Xi's and 1 intercept

#cross validation
set.seed(10)
cv.out = cv.glmnet(x = x, y = y, alpha = 0,nfolds = 7)
plot(cv.out)

names(cv.out)
best.lambda<-cv.out$lambda.min
best.lambda # best lambda

model.ridge <- glmnet(x = x, y = y, alpha = 0)
predict(model.ridge, type = "coefficients", s = best.lambda)[1:20,]
            

#Lasso Model - chooses the coefficients automatically
cv.out = cv.glmnet(x = x, y = y, alpha = 1,nfolds = 10)
plot(cv.out)

names(cv.out)
best.lambda2<-cv.out$lambda.min
best.lambda2
model.lasso <- glmnet(x = x, y = y, alpha = 1)
predict(model.lasso, type = "coefficients", s = best.lambda2)[1:20,]


