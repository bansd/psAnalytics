vote = read.csv("gerber.csv")

table(vote$voting)/nrow(vote)

tapply(vote$voting, vote$civicduty, mean)
tapply(vote$voting, vote$hawthorne, mean)
tapply(vote$voting, vote$self, mean)
tapply(vote$voting, vote$neighbors, mean)

#Building logistic model
modellog = glm(voting~neighbors+self+hawthorne+civicduty,data=vote, family="binomial")
summary(modellog)

#here we have not used newdata as its same data
pred1 = predict(modellog, type="response")

#Calculating Accuracy with threshold 0.3
table(vote$voting, pred1>0.3)
(134513+51966)/(134513+100875+56730+51966)

#Calculating Accuracy with threshold 0.5
table(vote$voting, pred1>0.5)
235388/(235388+108696)

#Calculating AUC value and to determine if model is weak or strong
library(ROCR)
r = prediction(pred1,vote$voting)
auc = as.numeric(performance(r, "auc")@y.values)
auc

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote)
prp(CARTmodel)

#building model using cp = 0.0
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=vote, cp=0.0)
prp(CARTmodel3)

#building model using cp = 0.0 and variable sex
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + sex +neighbors, data=vote, cp=0.0)
prp(CARTmodel2)

table(vote$civicduty)/nrow(vote)

CARTmodel4 = rpart(voting ~ control, data=vote, cp=0.0)
CARTmodel5 = rpart(voting ~ control+sex, data=vote, cp=0.0)
prp(CARTmodel4)
prp(CARTmodel5)
prp(CARTmodel4)
prp(CARTmodel4,digits = 6)

abs(0.34-0.296638)

prp(CARTmodel5,digits = 6)
modellog2 = glm(voting~control+sex,data=vote, family="binomial")
summary(modellog2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(modellog2, newdata=Possibilities, type="response")
prp(CARTmodel5,digits=6)
abs(0.290456-0.2908065)

#Creating logistics model sex:control ratio means that is the combination
# of the "sex" and "control"
# variables - so if this new variable is 1, that means the person is a woman 
#AND in the control group


LogModel2 = glm(voting ~ sex + control + sex:control, data=vote, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")


