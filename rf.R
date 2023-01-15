adhd = read.csv("C:/Users/yagmu/Desktop/Auburn/Research/project/datasetADHD/train_data.csv")
adhd = t(adhd)
colnames(adhd) = as.character(adhd[2,])
adhd = adhd[-(1:2),]
X = apply(adhd[,-1], 2, as.numeric)
adhd = data.frame(adhd[,1], X)
adhd = as.data.frame(adhd)

rownames(adhd) = 1:nrow(adhd)

adhd[adhd == "Controls"]=0
adhd[adhd == "ADHD-C" | adhd == "ADHD-H" | adhd == "ADHD-I"]=1

predictors = as.data.frame(adhd[,-1])
y = as.factor(adhd[,1])

set.seed(2705) 
ind = sample(1:dim(predictors)[1],dim(predictors)[1]*0.2)  
y_test = y[ind]
y_train = y[-ind]
x_test = predictors[ind,]
x_train = predictors[-ind,]

### loading test data
test_data = read.csv("test_data.csv")
str(test_data)
test_data = t(test_data)
colnames(test_data) = as.character(test_data[2,])
test_data = test_data[-(1:2),]
X = apply(test_data[,-1], 2, as.numeric)
test_data = data.frame(test_data[,1], X)
test_data = as.data.frame(test_data)
str(test_data)


rownames(test_data) = 1:nrow(test_data)
colnames(test_data)[1]="y_test"

test_data[test_data == "Controls"]=0
test_data[test_data == "ADHD-C" | test_data == "ADHD-H" | test_data == "ADHD-I"]=1
test_data$y_test = as.factor(test_data$y_test)
library(randomForest)
library(caret)

train = data.frame(y_train,x_train)
set.seed(12)
rf <- randomForest(y_train~., data=train, proximity=TRUE) 

floor(sqrt(ncol(train) - 1))
mtry <- tuneRF(train[-1],train$y_train, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
set.seed(71)
rf <-randomForest(y_train~.,data=train, mtry=best.m[1], importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$y_train)


p2 <- predict(rf, test_data)
confusionMatrix(p2, test_data$y_test)



varImpPlot(rf)
#Examine our Variable importance plot

to.remove<-c(which(data.frame(rf$importance)$MeanDecreaseAccuracy==min(data.frame(rf$importance)$MeanDecreaseAccuracy)))
#Remove the variable with the lowest decrease in Accuracy (Least relevant variable)

#Rinse, wash hands, repeat

var.predict<-paste(names(train)[-c(5,to.remove)],collapse="+")
rf.form <- as.formula(paste(names(train)[5], var.predict, sep = " ~ "))

rf<-randomForest(rf.form,data=train,importance=TRUE,ntree=100)

varImpPlot(rf)
