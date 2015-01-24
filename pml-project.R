#PML Project - Henry Bowers - Jan 2015

setwd("~/Projects/Practical Machine Learning/pml")

#load libraries
library(caret)
library(ggplot2) #redundant - caret loads ggplot2


#load source data
trainRaw <- read.csv("../data/pml-training.csv",header = TRUE)
testRaw <- read.csv("../data/pml-testing.csv",header = TRUE)


#subset data to summary features
trainRawWindow <- trainRaw[trainRaw$new_window=="yes",]
trainRawIndex <- grepl("avg",names(trainRawWindow))
trainRawWindowCo <- trainRawWindow[,grepl("avg|std|min|max|user|class|total",names(trainRawWindow))]

#set up cross-validation sets - split training set into training and testing
trainIndex = createDataPartition(trainRawWindowCo$classe,p=0.5,list=FALSE)
training = trainRawWindowCo[trainIndex,]
testing = trainRawWindowCo[-trainIndex,]

#explore relationships
#1. total acceleration
featurePlot(x=training[,c("total_accel_belt","total_accel_arm","total_accel_dumbbell")],y = as.numeric(training$classe),plot="pairs")
# not a good discriminant

#2. 
qplot()

set.seed(1599)

