
library(ISLR2)
library(tree)
library(gbm)
data0 = data.frame(Hitters)
?Hitters
summary(Hitters)
View(data0)
dim(data0)

#a)
data1 = na.omit(data0)
data1=data.frame(data1)
dim(data1)

logsal = log(data1$Salary)
data1$Salary = logsal
View(data1)
attach(data1)

#b) Create a training set and test set 
train = data.frame(data1[1:200,])
test = data.frame(data1[-c(1:200),])

train$Salary = NULL
test$Salary = NULL
#View(test)

# Perform boosting on the training set
boost_Hit = gbm(logsal[1:200]~.,data=train,distribution = "gaussian",
                n.trees = 1000,shrinkage = 0.01)
yhat.boost_Hit = predict(boost_Hit,newdata = train, n.trees = 1000)
mean((yhat.boost_Hit - logsal[1:200])^2)

#c) Training set MSE 
# Perform boosting on the training set for different values of shrinkage parameter.

# The shrinkage parameter is known as the learning rate that controls the rate 
# at which the model learns from each iteration.

mse_tr = c()
lam = seq(0,1,length=100)
for(i in 1:length(lam))
{
  boost_Hit = gbm(logsal[1:200]~.,data=train,distribution = "gaussian",
                  n.trees = 1000, shrinkage = lam[i])
  yhat.boost_Hit = predict(boost_Hit,newdata = train, n.trees = 1000)
  mse_tr[i] = mean((yhat.boost_Hit - logsal[1:200])^2)
}
mse_tr
plot(lam,mse_tr,xlim=c(0,1),ylim=c(0,0.8),xlab = "Shrinking parameter",type="l")

#d) Testing set MSE 
mse_te=c()
lam = seq(0,1,length=100)
for(i in 1:length(lam))
{
  boost_Hit=gbm(logsal[1:200]~.,data=train,distribution = "gaussian",n.trees = 1000, 
                shrinkage = lam[i])
  yhat.boost_Hit = predict(boost_Hit,newdata = test, n.trees = 1000)
  mse_te[i] = mean((yhat.boost_Hit - logsal[-c(1:200)])^2)
}
mse_te
plot(lam,mse_te,xlim=c(0,1),xlab = "Shrinking parameter",type="l")

#e) Regression fit.

fit = lm(logsal[1:200]~.,data=train)
av = anova(fit)
#dim(data.frame(av))
mse_rtr = av$`Mean Sq`[20]
mse_rtr
min(mse_tr)

pred = predict(fit,test)
mse_rte = sum((pred-logsal[-c(1:200)])^2)/42
mse_rte
min(mse_te)
lam[which.min(mse_te)]

# Comparing 
View(data.frame(logsal[-c(1:200)],pred))

#e) 
summary(boost_Hit)
x=summary(boost_Hit)$ rel.inf
y=summary(boost_Hit)$ var       
sum(x)
barplot(x,names.arg = y,las = 2,
        cex.names = 0.6)

