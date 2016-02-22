Decision Tree code/examples
========================================================

This is an R Markdown document tracks R examples, exercises used in Stats202 lectures.

## Decision trees

Function "rpart" generated decision trees in R. Does classification and regression. For classification, ensure rpart() knows you're predicting (y) a factor, not numeric, variable.

```r
library(rpart)
```

Read sonar datasets of 60 attr and 100 obs.


```r
setwd("./data")
##download.file("nice redirects google","sonar_train.csv", method="curl")
##download.file("https://sites.google.com/site/stats202/data/sonar_train.csv",
##              "sonar_test.csv",method="curl")

train<-read.csv("sonar_train.csv",header=F)
test<-read.csv("sonar_test.csv",header=F)

setwd("../")
```

Fit a decision tree to predict where obj is metal or a rock.


```r
y<-as.factor(train[,61])
x<-train[,1:60]
fit<-rpart(y~.,x)

## or fit1<-rpart(as.factor(train$V61)~.,data=train)
```

How accurate did predict y

```r
sum(y!=predict(fit,x,type="class"))/length(y)
```

```
## [1] 0.1154
```

```r
## predict(fit,x,type="class") yields what you were trying to predict e.g., column 61. 
## So, take sum of instances where prediction != col61 and divide by total obs.
## Yields training set error rate. 
```
