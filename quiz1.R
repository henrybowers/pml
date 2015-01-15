#Quiz 2 - Practical Machine Learning

setwd("~/Projects/Practical Machine Learning/pml")

#Q1

## ANSWER = 
# adData = data.frame(diagnosis,predictors)
# testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
# training = adData[-testIndex,]
# testing = adData[testIndex,]

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

summary(predictors)
summary(diagnosis)

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]

#Q2

# ANSWER = There is a step-like pattern in the plot of outcome versus index in the training set that isn't explained by any of the predictor variables 
# so there may be a variable missing.

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(ggplot2)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(CompressiveStrength,data=training)

library(Hmisc)
cutVar <- cut2(training$FlyAsh,g=5)
qplot(CompressiveStrength,colour=cutVar,fill=cutVar,data=training)


#Q3

#ANSWER  = the log transform is not a monotone transformation of the data.

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))
mean(training$Superplasticizer)
sd(training$Superplasticizer)
summary(log(training$Superplasticizer))
sd(training$Superplasticizer)


#Q4

#ANSWER = 7

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#names(training)
index <- grepl("^IL_*",names(training))
training.IL <- training[,index]
#prcomp(training.IL)
preProc <- preProcess(training.IL,method="pca",thresh=0.8)
preProc$numComp

#or

index <- grepl("^IL_*",names(predictors))
predictors.IL <- predictors[,index]
#prcomp(training.IL)
preProc <- preProcess(predictors.IL,method="pca",thresh=0.8)
preProc$numComp


#Q5

#ANSWER = Non-PCA Accuracy: 0.65 , PCA Accuracy: 0.72

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)

index <- grepl("^IL_*",names(predictors))
predictors.IL <- predictors[,index]

adData = data.frame(diagnosis,predictors.IL)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#GLM prediction without PCA
modelFit <- train(training$diagnosis ~ .,method="glm",data=training)
cm <- confusionMatrix(testing$diagnosis,predict(modelFit,testing))
cm

#GLM prediction with PCA
preProc <- preProcess(training[,-1],method="pca",thresh=0.8)
trainPC <- predict(preProc,training[,-1])
modelFitPCA <- train(training$diagnosis ~ .,method="glm",data=trainPC)
testPC <- predict(preProc,testing[,-1])
cmPCA<-confusionMatrix(testing$diagnosis,predict(modelFitPCA,testPC))
cmPCA

#compare accuracy of w/o PCA vs. w/ PCA
cm$overall
cmPCA$overall

