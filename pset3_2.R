parole = read.csv("parole.csv")
str(parole)
summary(parole)

#parolees in the dataset violated the terms of their parole
table(parole$violator)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)

summary(parole$state)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
nrow(train)
nrow(test)

model1 = glm(violator ~ ., data = train, family=binomial)
summary(model1)

#calculating odds and probability (y=1)
int = -4.2411574 + 0.3869904*1 + 0.8867192*1 + 50*-0.0001756 + 3*-0.1238867 + 0.0802954*12 + 1.6119919*0 + 0.6837143*1
exp(int)
1/(1+exp(-int))

#TO find max probability
predictTest = predict(model1, type="response", newdata=test)
summary(predictTest)

table(test$violator, predictTest > 0.5)
(167+12)/(167+12+11+12)
12/(11+12)
167/(167+12)

library(ROCR)
r = prediction(predictTest,test$violator)
rp = performance(r,"tpr","fpr")


auc = as.numeric(performance(r, "auc")@y.values)
auc
plot(rp,colorize=TRUE)



