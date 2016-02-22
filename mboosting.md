Adaboost algo using rpart() decision tree as weak learner. 
========================================================

AdaBoost is the most popular boosting algo. Boosting is the best off-the-self-classifier. 

Start by setting a vector f = zero for all xi. At the end, if the value for an observation in this vector is >1, it will be positive, else negative. 

Initialize the weights for the observations. Initally they will be set at 1/N for all obs. (They are equal and normalized to sum to 1.)

Set m, your number of observations. 
Fit a classifier (decision tree) to the unweighted training data using the weights wi where gm maps each xi to -1 or 1. **Make sure you re-map class labels to -1 or 1 as algo won't work w/out them.**

Compute the weighted error rate. This is the weight we will use for the majority vote. So, after m terations, we'll weight trees that performed best overall. 

Take 1/2 log odds of the misclass error as the weight for the tree. So, if weighted misclass is large, the weight will be small and vice versa. 

Let fm = fm-1 + alpham*gm. This is how we're cumulating our weighted vote of each of our trees. 

Replace previous weights wi with wi*e^-(alpham)(gm(xi))*yi. This will help tree focus on observations we've been misclassifying so far. Key points:
- If yi = gm(xi), then test class = predicted class and the product gm(xi) * yi will = 1. If not (i.e., an incorrect classifiation) it will be -1. 
- b/c i'm taking e^-alpham if it's correct, I'm adding e^-alpham. Otherwise, I'm deducting e^-alpham.
- Recall that if the prediction is correct, e^-alpham is less than one and it will downweight the weight. If incorrect, it's greater than one and you increase weight. 

Now, we've re-weighted, normalilze so weights sum to 1. Replace each wi w/ wi/sum(wi).

Go to top and fit a new classifer using the new weights.

Now as it runs thru tree and seeks to minimize Gini index, it does so including the observation weight resulting in a weighted Gini index. (You can give weights to Rpart so that it knows how to weight.)


### Initialize data


```r
library(rpart)

setwd("./data")

mtrain<-read.csv("sonar_train.csv",header=F)
mtest<-read.csv("sonar_test.csv",header=F)

setwd("../")

my_train<-mtrain[,61]
mx_train<-mtrain[,1:60]

my_test<-mtest[,61]
mx_test<-mtest[,1:60]
```

### Add vectors to track results on each iteration


```r
## Track the training and test errors for each of 500 revs.
mtrain_error<-rep(0,500)
mtest_error<-rep(0,500)

## Track "f", the weighted vote of the decision trees applied to train and test data. 
mf_train<-rep(0,130)
mf_test<-rep(0,78)

i<-1 ## iteration number
```

### Set up your loop


```r
while(i<=500) {
     mw<-exp(-my_train*mf_train)
     ## compute weights as e(-y*f) where f=∑(i=0,M)am*gm for the prior iterations
     
     mw<-mw/sum(mw)
     ## renormalize weights to sum to 1
     
     fit<-rpart(my_train~.,mx_train,mw,method="class")
     ## fit a classifiation tree
     
     mg_train<--1+2*(predict(fit,mx_train)[,2]>0.5)
     ## force result to be 1 or -1 b/c R gives 1's and 0's for class labels
     
     mg_test<--1+2*(predict(fit,mx_test)[,2]<0.5)
     ## Do the same confersion for the test set
     
     me<-sum(mw*(my_train*mg_train<0))
     ## Compute misclass errors i.e., sum of weights for each obs classed incorrectly
     
     malpha<-0.5*log((1-me)/me)
     ## compute alpha wich is 1/2 the log of logs

     mf_train<-mf_train+malpha*mg_train
     ## update the prediction on training data set.
     
     mf_test<-mf_test+malpha*mg_test
     ## update the prediction on the test data set.
     
     mtrain_error[i]<-sum(1*mf_train*my_train<0)/130
     ## compute training error, which obs I would misclassify if I stop at this iter.
     ## If f*y<0, then incorrect. If f*y>0, then correct. 
     
     mtest_error[i]<-sum(1*mf_test*my_test<0)/78
     ## Same as above, but applied to test data set.
     
     i<-i+1     
}
```

### Plot your graphic


```r
plot(seq(1,500),mtest_error,type="l",ylim=c(0,0.9),ylab="Error Rate",
     xlab="Iterations",lwd=2,col="black")
lines(mtrain_error,lwd=2,col="purple")
legend(4,0.5,c("Training Error","Test Error"),col=(c("purple","black",lwd=2)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
