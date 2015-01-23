#Quiz 2\3 - Practical Machine Learning

setwd("~/Projects/Practical Machine Learning/pml")

#Q1 

# ANSWER = 

#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
#b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

# a = PS
# b = WS
# c = PS
# d = NA

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

#adData = data.frame(segmentationOriginal)
#trainIndex = createDataPartition(adData$Case,list=FALSE)
#training = adData[trainIndex,]
#testing = adData[-trainIndex,]

training <- segmentationOriginal[segmentationOriginal$Case=="Train",]
testing <- segmentationOriginal[segmentationOriginal$Case=="Test",]

set.seed(125)

modfit <- train(Class ~ .,method="rpart",data=training)

modfit$finalModel
plot(modfit$finalModel)
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#Q2 If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set)
# accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) 
# accuracy smaller or bigger. Is K large or small in leave one out cross validation?

# ANSWER = bigger, smaller, equal to sample size


#Q3  These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
# Fit a classification tree where Area is the outcome variable. Then predict the value of area for the 
# following data frame using the tree command with all defaults

# ANSWER = 2.875 , It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata.

library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]


modfit <- tree(Area ~ .,data=olive)
modfit$frame

newdata = as.data.frame(t(colMeans(olive[,-1])))

predict(modfit,newdata)

# try again for fun with Area as factor
olive$Area <- as.factor(olive$Area)

modfit <- tree(Area ~ .,data=olive)
modfit$frame

newdata = as.data.frame(t(colMeans(olive[,-1])))

predict(modfit,newdata)


#Q4  

# ANSWER: test misclass = 0.31, train misclass = 0.27

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modfit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl  ,method="glm",family="binomial",data=trainSA)

modfit$finalModel

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

predTrainSA <- predict(modfit,trainSA)
predTestSA <- predict(modfit,testSA)


missClass(testSA$chd,predTestSA)

# Q5

# ANSWER = x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10
# NOTE: x.9 and x.3 are essentially equal, and so their ranks are interchangeable


library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 


vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

modelfit <- train(y ~ .,data = vowel.train)
varImp(modelfit)
