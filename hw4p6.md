hw4p6 
========================================================  

### Initialize data


```r
library(rpart)

setwd("./data")
son_train<-read.csv("sonar_train.csv",header=F)
son_test<-read.csv("sonar_test.csv",header=F)
setwd("../")

y_train<-son_train[,61]
x_train<-son_train[,1:60]

y_test<-son_test[,61]
x_test<-son_test[,1:60]
```

### Add vectors to track results on each iteration


```r
## track the training and test errors for each of 500 revs.
train_error<-rep(0,500)
test_error<-rep(0,500)

## "f", the weighted vote of the decision trees applied to train and test data. 
f_train<-rep(0,130)
f_test<-rep(0,78)

i<-1
```

### Set up loop


```r
set.seed(123)

while(i<=500) {

     w<-exp(-y_train*f_train) ## Initialize vector of weights

     w<-w/sum(w) ## Normalize weights
     
     fit<-rpart(y_train~.,x_train,w,method="class") 
     ## Fit tree classifier to training data using weights
     
     g_train<--1+2*(predict(fit,x_train)[,2]>0.5)
     ## Calc g flagging successful (1) and unsuccessful (-1) x[i] predictions
           
     g_test<--1+2*(predict(fit,x_test)[,2]>0.5)

     e<-sum(w*(y_train*g_train<0)) ## Compute weighted (w[m]) misclass errors (e[m]) 
     
     alpha<-0.5*log((1-e)/e)
     ## 1/2 log odds of the sum of the misclass errors (e[m])
     
     f_train<-f_train+alpha*g_train
     ## Take prior f_train[m] vect, add tree weight (alpha) * the indiv observation values (g[m]) combining tree and observation weights
     
     f_test<-f_test+alpha*g_test
     
     train_error[i]<-sum(1*f_train*y_train<0)/130 ## Compute and save training error 
     
     test_error[i]<-sum(1*f_test*y_test<0)/78

     i<-i+1
}
print(min(test_error)) # minimum test error
```

```
## [1] 0.141
```

```r
print(which.min(test_error)) # minimum test error iteration
```

```
## [1] 280
```

```r
print(test_error[iter]) # final test error
```

```
## Error: object 'iter' not found
```

### Output graphic


```r
plot(seq(1,500),test_error,type="l",ylim=c(0,0.5),ylab="Error Rate",
     xlab="Iterations",lwd=2,col="black")
lines(train_error,lwd=2,col="purple")
legend(4,0.5,c("Training Error","Test Error"),col=(c("purple","black",lwd=2)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
