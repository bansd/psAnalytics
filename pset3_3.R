loans=read.csv("loans.csv")
summary(loans)

table(loans$not.fully.paid)

library("mice")
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loans)

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)

train = subset(loans,split == TRUE)
test = subset(loans,split == FALSE)
model1 = glm(not.fully.paid ~.,data = train,family = binomial)
summary(model1)

test$predict.risk = predict(model1,newdata = test,type = "response")
table(test$not.fully.paid,test$predict.risk>0.5)

library(ROCR)
r = prediction(test$predict.risk,test$not.fully.paid)
auc = as.numeric(performance(r, "auc")@y.values)
auc

model2 = glm(not.fully.paid ~int.rate,data = train,family = binomial)
summary(model2)

pr = predict(model2,newdata = test,type = "response")
table(test$not.fully.paid,pr>0.5)

summary(pr)
r1 = prediction(pr,test$not.fully.paid)
auc = as.numeric(performance(r1, "auc")@y.values)
auc

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)

test1 = subset(test,test$int.rate > 0.15)
summary(test1$profit)

table(test1$not.fully.paid)
110/(327+110)

cutoff = sort(test1$predict.risk, decreasing=FALSE)[100]
cutoff

selectedLoans = subset(test1, test1$predict.risk<=cutoff)
nrow(selectedLoans)

sum(selectedLoans$profit)
summary(selectedLoans)
table(selectedLoans$not.fully.paid)




