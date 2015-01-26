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
summaryColIndex <- grepl("var|total|avg|stddev|min|max|ampl|kurt|skew|new",names(trainCleaned))
trainCleaned <- trainCleaned[,!summaryColIndex]

windowCount<-as.data.frame(table(trainCleaned$num_window))
windowCountIndex <- windowCount[windowCount$Freq>9,]
trainCleaned <- trainCleaned[trainCleaned$num_window %in% windowCountIndex$Var1,]

#extract features
trainSummary<-ddply(trainCleaned,.(num_window,classe),summarize,
                    #beltPitchVariance = var(pitch_belt),
                    #beltMaxPitch = max(pitch_belt),
                    beltRollVariance = var(roll_belt),
                    armRollVariance = var(roll_arm),
                    #forearmRollVariance = var(roll_forearm),
                    forearmPitchVar = var(pitch_forearm),
                    beltPitchVariance = var(pitch_belt),
                    #armPitchVariance = var(pitch_arm),
                    dumbbellVarianceY = var(magnet_dumbbell_y),
                    #armVarianceY = var(magnet_arm_y),
                    dumbbellVarianceZ = var(magnet_dumbbell_z),
                    #armVarianceZ = var(magnet_arm_z),
                    dumbbellAccelVarianceY = var(accel_dumbbell_y),
                    dumbbellAccelVarianceX = var(accel_dumbbell_x),
                    dumbbellAccelVarianceZ = var(accel_dumbbell_z),
                    dumbbellAccelMeanX = abs(mean(accel_dumbbell_x)),
                    dumbbellAccelMeanY = abs(mean(accel_dumbbell_y)),
                    dumbbellAccelMeanZ = abs(mean(accel_dumbbell_z)),
                    #armMagnetRangeX = abs(mean(magnet_arm_x)/(max(magnet_arm_x)-min(magnet_arm_x))),
                    #armMagnetRangeZ = abs(mean(magnet_arm_z)/(max(magnet_arm_z)-min(magnet_arm_z))),
                    #armMagnetRangeY = abs(mean(magnet_arm_y)/(max(magnet_arm_y)-min(magnet_arm_y))),
                    dumbbellMagnetMaxY = max(magnet_dumbbell_y),
                    dumbbellMagnetMaxX = max(magnet_dumbbell_x),
                    dumbbellMagnetMaxZ = max(magnet_dumbbell_z)
                    #dumbbellMagnetRangeY = abs(mean(magnet_dumbbell_y)/(max(magnet_dumbbell_y)-min(magnet_dumbbell_y))),
                    #dumbbellMagnetRangeX = abs(mean(magnet_dumbbell_x)/(max(magnet_dumbbell_x)-min(magnet_dumbbell_x))),
                    #dumbbellMagnetRangeZ = abs(mean(magnet_dumbbell_z)/(max(magnet_dumbbell_z)-min(magnet_dumbbell_z)))
                    #dumbbellMagnetMeanY = mean(magnet_dumbbell_y),
                    #armAccelMaxZ = abs(mean(accel_arm_y))
                    )


#qplot(dumbbellAccelMeanY,num_window,colour=classe,data=trainSummary)

#remove num_window to make predictor formula shorter
trainSummary <- trainSummary[,-1]


#set up cross-validation sets - split training set into training and testing
trainIndex <- createDataPartition(trainSummary$classe,p=0.75,list=FALSE)
training <- trainSummary[trainIndex,]
testing <- trainSummary[-trainIndex,]


#set cross-validation method to use during training
ctrl <- trainControl(method = "repeatedcv", repeats = 20)

#### build random forest ####
set.seed(1599)
modfit <- train(classe ~ . ,method="rf",data=training,trControl = ctrl)
varImpPlot(modfit$finalModel)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on test
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)

### build decision tree ###
set.seed(1599)
modfit <- train(classe ~ . + -num_window,method="rpart",data=training,trControl = ctrl)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Random Forest - Class Error", margin=0.2)
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

#extract features
testSummary<-ddply(testCleaned,.(num_window,classe),summarize,
                    #beltPitchVariance = var(pitch_belt),
                    #beltMaxPitch = max(pitch_belt),
                    beltRollVariance = var(roll_belt),
                    armRollVariance = var(roll_arm),
                    #forearmRollVariance = var(roll_forearm),
                    forearmPitchVar = var(pitch_forearm),
                    beltPitchVariance = var(pitch_belt),
                    #armPitchVariance = var(pitch_arm),
                    dumbbellVarianceY = var(magnet_dumbbell_y),
                    #armVarianceY = var(magnet_arm_y),
                    dumbbellVarianceZ = var(magnet_dumbbell_z),
                    #armVarianceZ = var(magnet_arm_z),
                    dumbbellAccelVarianceY = var(accel_dumbbell_y),
                    dumbbellAccelVarianceX = var(accel_dumbbell_x),
                    dumbbellAccelVarianceZ = var(accel_dumbbell_z),
                    dumbbellAccelMeanX = abs(mean(accel_dumbbell_x)),
                    dumbbellAccelMeanY = abs(mean(accel_dumbbell_y)),
                    dumbbellAccelMeanZ = abs(mean(accel_dumbbell_z)),
                    #armMagnetRangeX = abs(mean(magnet_arm_x)/(max(magnet_arm_x)-min(magnet_arm_x))),
                    #armMagnetRangeZ = abs(mean(magnet_arm_z)/(max(magnet_arm_z)-min(magnet_arm_z))),
                    #armMagnetRangeY = abs(mean(magnet_arm_y)/(max(magnet_arm_y)-min(magnet_arm_y))),
                    dumbbellMagnetMaxY = max(magnet_dumbbell_y),
                    dumbbellMagnetMaxX = max(magnet_dumbbell_x),
                    dumbbellMagnetMaxZ = max(magnet_dumbbell_z)
                    #dumbbellMagnetRangeY = abs(mean(magnet_dumbbell_y)/(max(magnet_dumbbell_y)-min(magnet_dumbbell_y))),
                    #dumbbellMagnetRangeX = abs(mean(magnet_dumbbell_x)/(max(magnet_dumbbell_x)-min(magnet_dumbbell_x))),
                    #dumbbellMagnetRangeZ = abs(mean(magnet_dumbbell_z)/(max(magnet_dumbbell_z)-min(magnet_dumbbell_z)))
                    #dumbbellMagnetMeanY = mean(magnet_dumbbell_y),
                    #armAccelMaxZ = abs(mean(accel_arm_y))
)
