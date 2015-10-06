require(caret)
require(MASS)
require(boot)
require(leaps)
training <- read.csv("pml-training.csv",na.strings=c("","NA"))
set.seed(1214)

## This cleans out all of the averages, stddevs, etc that leave a lot of NAs.
## The second line clears out the extraneous data; timestamps, names, etc.
training <- training[,!apply(is.na(training[1,]),2,any)]
training <- training[,8:60]

RFfit <- train(classe~.,data=training,method="rf",trControl=trainControl(method="cv",number=5),prox=TRUE,allowParallel=TRUE)
## Need to come up with a way to separate the training data into a Training Set
## and a Test Set.
inTrain <- createDataPartition(y=training[,1],p=0.6,list=FALSE)
test <- training[-inTrain,]
test.true <- test[,53]
train <- training[inTrain,]

## Standardize data set for KNN - Not working right
# preObj <- preProcess(train[,-53],method=c("center","scale"))
# train.stand <- predict(preObj,train[,-53])
# test.stand <- predict(preObj,test[,-53])

## Exploring the correlation of variables
M <- abs(cor(training[,-53]))
diag(M) <- 0
which(M > 0.8, arr.ind=T)

## Some preProcessing. Note: Need to validate that this is a good idea.
## Saw a decrease in ~ 3%. On this size dataset, no reason to sacrifice.
# traincor <- cor(train[,-53])
# highcor <- findCorrelation(traincor,cutoff = .85)
# train <- train[,-highcor]
# test <- test[,-highcor]

## Create an LDA Model
LDAfit <- lda(classe~., data=train)

## Testing the LDA Model using a confusion matrix
## Computes the percent of correct predictions
LDApred <- predict(LDAfit,test[,1:52])
table(LDApred$class,test.true)
mean(LDApred$class==test.true)

## Create a QDA Model
QDAfit <- train(train$classe~., method = "qda",data=train)

## Testing the QDA Model using a confusion matrix
## Computes the percent of correct predictions
## Note: This appears WAY better than LDA
QDApred <- predict(QDAfit,newdata=test)
confusionMatrix(test$classe,predict(QDAfit,test))

## Random Forest
RFfit <- train(classe~.,data=training,
               method="rf",trControl=trainControl(method="cv",
                                      number=5),prox=TRUE,allowParallel=TRUE)
# To show information:
print(RFfit)

## PCA work
# QDA - No good, accuracy = .76
PCAfit <- train(train$classe ~ ., method="qda",preProcess="pca",data=train)
confusionMatrix(test$classe,predict(PCAfit,test))


## Create KNN Model with k=5 and k=10
KNN5pred <- knn(train.scale,test[,1:52],train[,53],k=5)
KNN10pred <- knn(train.scale,test[,1:52],train[,53],k=10)

## KNN5 Model Confusion Matrix
## Computes the percent of correct predictions
table(KNN5pred,test.true)
mean(KNN5pred==test.true)

## KNN10 MOdel Confusion Matrix
## Computes the percent of correct predictions
table(KNN10pred,test.true)
mean(KNN10pred==test.true)
