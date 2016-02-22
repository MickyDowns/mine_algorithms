hw4p5
========================================================

### Load files / libraries


```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
setwd("./data")
train<-read.csv("sonar_train.csv",header=F)
test<-read.csv("sonar_test.csv",header=F)
setwd("../")
```

### Format training data set. Fit model. Calculate training error. 


```r
y_train<-as.factor(train[,61])
x_train<-train[,1:60]

fit<-randomForest(x_train, y_train, )

1-sum(y_train==predict(fit,x_train))/length(y_train)
```

```
## [1] 0
```
