#PML Project - Henry Bowers - Jan 2015

setwd("~/Projects/Practical Machine Learning/pml")
setwd("~/R-projects/Practical Machine Learning/pml")

#load libraries
library(caret)
library(ggplot2) #redundant - caret loads ggplot2
library(plyr)


#load source data
trainRaw <- read.csv("../data/pml-training.csv",header = TRUE,)
testRaw <- read.csv("../data/pml-testing.csv",header = TRUE)

#clean data - remove summary rows, cols
trainCleaned <- trainRaw[trainRaw$new_window=="no",]
summaryColIndex <- grepl("var|avg|stddev|min|max|ampl|kurt|skew|new",names(trainCleaned))
trainCleaned <- trainCleaned[,!summaryColIndex]

windowCount<-as.data.frame(table(trainCleaned$num_window))
windowCountIndex <- windowCount[windowCount$Freq>9,]
trainCleaned <- trainCleaned[trainCleaned$num_window %in% windowCountIndex$Var1,]

trainSummary <- trainCleaned[,8:59]


#set up cross-validation sets - split training set into training and testing
trainIndex <- createDataPartition(trainSummary$classe,p=0.75,list=FALSE)
training <- trainSummary[trainIndex,]
testing <- trainSummary[-trainIndex,]


#set cross-validation method to use during training
ctrl <- trainControl(method = "repeatedcv", repeats = 5)

#### build random forest ####
set.seed(1599)
modfit <- train(classe ~ . ,method="rf",data=training,trControl = ctrl)
varImpPlot(modfit$finalModel)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Random Forest - Class Error ", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on test
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)

### build decision tree ###
set.seed(1599)
modfit <- train(classe ~ . ,method="rpart",data=training,trControl = ctrl)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on testing
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)


#################################################################
# run against official test set to get realistic predictor performance
#################################################################

#transform and summarize test data set
testCleaned <- testRaw[testRaw$new_window=="no",]
summaryColIndex <- grepl("var|total|avg|stddev|min|max|ampl|kurt|skew|new",names(testCleaned))
testCleaned <- testCleaned[,!summaryColIndex]
testCleaned <- testCleaned[,9:54]

testResults<-predict(modfit,testCleaned)
confusionMatrix(testResults,testCleaned$classe)

source("pml_write_files.R")

pml_write_files(testResults)
