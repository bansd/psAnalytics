emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

emails$text[1]

max(nchar(emails$text))

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm
spdtm = removeSparseTerms(dtm, 0.95)
spdtm 

emailsSparse = as.data.frame(as.matrix(sparse))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))
emailsSparse$spam = emails$spam
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse,split==FALSE)

spamLog = glm(spam~.,data=train,family = binomial)
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~.,data=train,method="class")
library(randomForest)
set.seed(123)
spamRF = randomForest(spam~.,data=train)

predTrainLog = predict(spamLog,type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF,type="prob")[,2]

#calculating predicted probabilities
summary(predTrainLog)
summary(spamLog)
table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)


table(train$spam,predTrainLog>0.5)
(954+3052)/(3052+954+4)
prp(spamCART)
library(ROCR)
r = prediction(predTrainLog,train$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

table(train$spam,predTrainCART>0.5)
(894+2885)/(894+2885+64+167)
r = prediction(predTrainCART,train$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

table(train$spam,predTrainRF>0.5)
(914+3013)/(3013+914+39+44)
r = prediction(predTrainRF,train$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

predTestLog = predict(spamLog,type="response",newdata=test)
table(test$spam,predTestLog>0.5)
(376+1257)/(1257+51+376+34)
r = prediction(predTestLog,test$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc


predTestCART = predict(spamCART,newdata=test)[,2]
predTestRF = predict(spamRF,type="prob",newdata=test)[,2]
table(test$spam,predTestCART>0.5)
(386+1228)/(1228+80+386+24)
r = prediction(predTestCART,test$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

table(test$spam,predTestRF>0.5)
(1290+385)/(1290+385+18+25)
r = prediction(predTestRF,test$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

wordCount = rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))
emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam,xlab="spam",ylab="wordcount")


train2 = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse,split==FALSE)
spam2CART = rpart(spam~.,data=train2,method="class")
spam2RF = randomForest(spam~.,data=train2)
set.seed(123)
spam2RF = randomForest(spam~.,data=train2)
prp(spam2CART)
predTestCART = predict(spamCART,newdata=test)[,2]
predTest2CART = predict(spam2CART,newdata=test)[,2]
table(test$spam,predTest2CART>0.5)

(384+1214)/(1214+94+384+26)
r = prediction(predTest2CART,test$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc

predTest2RF = predict(spam2RF,type="prob",newdata=test)[,2]
table(test$spam,predTest2RF>0.5)
(382+1298)/(1298+10+382+28)
r = prediction(predTest2RF,test$spam)
auc = as.numeric(performance(r, "auc")@y.values)
auc
