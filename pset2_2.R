train = read.csv("pisa2009train.csv")
test = read.csv("pisa2009test.csv")
str(train)

tapply(train$readingScore,train$male,mean)

summary(train)

pisaTrain = na.omit(train)
pisaTest = na.omit(test)
str(pisaTrain)

str(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore~.,data=pisaTrain)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(PisaTrain))

RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))
SSE

ts = mean(pisaTrain$readingScore)
SST = sum((mean(N) - pisaTest$readingScore)^2)
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
ts

r2 = 1-SSE/SST
r2




