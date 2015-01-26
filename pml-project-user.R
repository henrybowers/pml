#PML Project - Henry Bowers - Jan 2015

setwd("~/Projects/Practical Machine Learning/pml")
setwd("~/R-projects/Practical Machine Learning/pml")

#load libraries
library(caret)
library(ggplot2) #redundant - caret loads ggplot2
library(plyr)


#load source data
trainRaw <- read.csv("../data/pml-training.csv",header = TRUE)
testRaw <- read.csv("../data/pml-testing.csv",header = TRUE)


#subset data to summary features
trainRawWindow <- trainRaw[trainRaw$new_window=="yes",]
trainRawWindowCo <- trainRawWindow[,grepl("user|class|var|total|avg",names(trainRawWindow))]
#avg|std|min|max|user|class|total
#trainRawWindowCo$ardS <- (trainRawWindowCo$amplitude_roll_dumbbell - mean(trainRawWindowCo$amplitude_roll_dumbbell))/sd(trainRawWindowCo$amplitude_roll_dumbbell)

#testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 


#set up cross-validation sets - split training set into training and testing
trainIndex = createDataPartition(trainRawWindowCo$classe,p=0.5,list=FALSE)
training = trainRawWindowCo[trainIndex,]
testing = trainRawWindowCo[-trainIndex,]

trainingA <- training[training$classe=="A",]
trainingB <- training[training$classe=="B",]
trainingC <- training[training$classe=="C",]
trainingD <- training[training$classe=="D",]
trainingE <- training[training$classe=="E",]

#table(training$user_name,training$classe)

#trainingCarlitos<-training[training$user_name=="carlitos",]

set.seed(1599)

modfit <- train(classe ~ . ,method="rpart",data=training)

1#preProcess=c("center","scale"),
#modfit <- train(classe ~ . ,preProcess="pca",method="rpart",data=trainingCarlitos)



#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#predict on train
predResults<-predict(modfit,training)
confusionMatrix(predResults,training$classe)
#predict on test
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)


#explore relationships
#1. total acceleration

featurePlot(x=trainingCarlitos[,c("max_accel_belt","max_accel_arm","max_accel_dumbbell")],y = trainingCarlitos[trainingCarlitos$classe=="A"]$classe,plot="pairs")
# varies by user_name

#2. 
featurePlot(x=training[,c("avg_roll_dumbbell","avg_pitch_dumbbell","avg_yaw_dumbbell")],y=training$user_name,plot="pairs")
# discriminant

featurePlot(x=training[,c("amplitude_pitch_dumbbell","amplitude_roll_dumbbell","classe")],y=training$user_name,plot="pairs")


set.seed(1599)

#1###decision tree on all statistical variables
modfit <- train(classe ~ . ,method="rpart",data=training)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#measure performance
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)

#2###preprocess to standardize variables


###random forest on same variables
modfit <- train(classe ~ . ,method="rf",data=training,prox=FALSE)

#examine model
modfit$finalModel
plot(modfit$finalModel, uniform=TRUE,main="Classification Tree", margin=0.2)
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

#measure performance
predResults<- predict(modfit,testing)
confusionMatrix(predResults,testing$classe)

#3### Naive Bayes
## train(Species ~ ., data=training,method="nb")


mutate(training)