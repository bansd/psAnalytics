census = read.csv("census.csv")
set.seed(2000)
library(caTools)
split = sample.split(census$over50k,SplitRatio = 0.6)
train = subset(census,split == TRUE)
test = subset(census,split == FALSE)

#Making model and calculating its accuracy
censusglm = glm( over50k ~ . , family="binomial", data = train)
summary(censusglm)
pred1 = predict(censusglm, type="response",newdata=test)
table(test$over50k,pred1>0.5)
(9051+1888)/(9051+1888+662+1190)
table(test$over50k)
9713/nrow(test)
library(ROCR)
r = prediction(pred1,test$over50k)
auc = as.numeric(performance(r, "auc")@y.values)
auc


library(rpart)
library(rpart.plot)

CARTmodel = rpart(over50k~., data=train,method = "class")
prp(CARTmodel)

plot(CARTmodel)
text(CARTmodel)

predictCART = predict(CARTmodel,newdata = test,type="class")
table(test$over50k,predictCART)
(9243+1596)/(9243+470+1482+1596)

predictCART = predict(CARTmodel,newdata = test)
pred = prediction(predictCART[,2],test$over50k)
perf = performance(pred,"tpr","fpr")
plot(perf)
auc = as.numeric(performance(pred, "auc")@y.values)
auc


#determining random forest
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library("randomForest")
set.seed(1)
smallforest = randomForest(over50k~.,data = trainSmall)
predictsmallforest = predict(test$over50k,newdata = test)
predictsmallforest = predict(smallforest,newdata = test)
table(test$over50k,predictsmallforest)
(9586+1093)/(9586+1093+127+1985)

vu = varUsed(smallforest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(smallforest$forest$xlevels[vusorted$ix]))
varImpPlot(smallforest)

#calculating value of cp using "train" and also determining its accuracy
library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(over50k~.,data = train,method="rpart",trControl = numFolds,tuneGrid = cartGrid)
model2 = rpart(over50k~.,data = train,method ="class",cp=0.002)
pred2 = predict(model2,newdata = test,type="class")
table(test$over50k,pred2)
9178+1838)/(9178+1838+535+1240)
prp(model2)