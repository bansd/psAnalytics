stocks = read.csv("StocksCluster.csv")
str(stocks)

table(stocks$PositiveDec)
6324/11580
cor(stocks)
#Return Nov and Oct has highest cor

summary(stocks)

library(caTools)
set.seed(144)
 
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec~.,data=stocksTrain,family=binomial)
predictlog = predict(StocksModel,type="response")

table(stocksTrain$PositiveDec,predictlog>0.5)
(990+3640)/(990+3640+2689+787)

predictlog = predict(StocksModel,type="response",newdata=stocksTest)
table(stocksTest$PositiveDec,predictlog>0.5)
(1553+417)/(1553+417+1160+344)

table(stocksTest$PositiveDec)
1897/(1577+1897)

#MAKING DEPENDENT VARIABLE NULL
limitedTrain = stocksTrain 
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#NORMALIZING DATA
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
summary(normTrain$ReturnJan)
summary(normTest$ReturnJan)
set.seed(144)
km = kmeans(normTrain,centers = 3,iter.max=1000)
km

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
summary(clusterTest)
#TO KNOW NUMBER OF DATA IN CLUSTERS
table(clusterTest)


stocksTrain1 = subset(stocksTrain, clusterTrain == 1)

stocksTrain2 = subset(stocksTrain, clusterTrain == 2)

stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)

stocksTest2 = subset(stocksTest, clusterTest == 2)

stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)

StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)

StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1,newdata=stocksTest1,type="response")
PredictTest2 = predict(StocksModel2,newdata=stocksTest2,type="response")
PredictTest3 = predict(StocksModel3,newdata=stocksTest3,type="response")

table(stocksTest1$PositiveDec,PredictTest1>0.5)
(774+30)/(774+30+23+471)

table(stocksTest2$PositiveDec,PredictTest2>0.5)
(388+757)/(757+626+309+388)

table(stocksTest3$PositiveDec,PredictTest3>0.5)
(13+49)/(49+13+13+21)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes,AllPredictions>0.5)
(1544+467)/(1544+467+1110+353)




