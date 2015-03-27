FluTrain = read.csv("FluTrain.csv")
which.max(FluTrain$ILI)
FluTrain$Week[303]
which.max(FluTrain$Queries)
FluTrain$Week[303]

hist(FluTrain$ILI)

hist(log(FluTrain$ILI))

plot(FluTrain$Queries, log(FluTrain$ILI))

FluTrend1 = lm(log(ILI)~Queries,data=FluTrain)
summary(FluTrend1)

R2 = cor(log(FluTrain$ILI),FluTrain$Queries)^2

FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

FluTest$Week

p = PredTest1[11]

f = FluTest$ILI[11]

(f-p) / f

SSE = sum((PredTest1-FluTest$ILI)^2)
SST = sum((mean(log((FluTrain$ILI)))-FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))

install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

plot(log(FluTrain$ILI),log(ILILag2))

FluTrend2=lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

nrow(FluTrain)

FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))







