Airlines = read.csv("AirlineDelay.csv")

set.seed(15071)

spl = sample(nrow(Airlines), 0.7*nrow(Airlines))

AirlinesTrain = Airlines[spl,]

AirlinesTest = Airlines[-spl,]
str(AirlinesTrain)
str(AirlinesTest)

modellm = lm(TotalDelay~.,data=AirlinesTrain)
summary(modellm)

cor(AirlinesTrain$NumPrevFlights,AirlinesTrain$PrevFlightGap)
[1] -0.6520532
> cor(AirlinesTrain$OriginWindGust,AirlinesTrain$OriginAvgWind)
[1] 0.5099535
predictlm = predict(modellm,newdata=AirlinesTest)

SSE = sum((AirlinesTest$TotalDelay-predictlm)^2)
> SSE
[1] 4744764
> SST = sum((AirlinesTest$TotalDelay-mean(AirlinesTrain$TotalDelay))^2)
> SST
[1] 5234023
> 1-SSE/SST
[1] 0.09347674


Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay", ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
> table(Airlines$DelayClass)

Airlines$TotalDelay = NULL
> set.seed(15071)
library(caTools)
> split = sample.split(Airlines$DelayClass,SplitRatio=0.7)
AirlinesTrain1 = subset(Airlines,split==TRUE)
> AirlinesTest1 = subset(Airlines,split==FALSE)

library(rpart)
> library(rpart.plot)
CARTmodel = rpart(DelayClass~., data=AirlinesTrain1,method="class")
predict = predict(CARTmodel,type="class")
> table(predict)
table(AirlinesTrain1$DelayClass,predict)
(3094+361)/(863+5704)
[1] 0.5261154
> 3094/(3094+804+1806)
[1] 0.5424264
( 3094+188)/(863+5704)

predict = predict(CARTmodel,type="class",newdata=AirlinesTest1)
> table(AirlinesTest1$DelayClass,predict)
table(predict)
1301+153)/(399+2415)

