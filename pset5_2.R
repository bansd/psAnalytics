trial = read.csv("clinical_trial.csv",stringsAsFactors=FALSE)
summary(nchar(trial$abstract))

sum(nchar(trial$abstract)==0)


which.min(nchar(trial$title))
trial$title[1258]
library(tm)
library(SnowballC)

corpusTitle = Corpus(VectorSource(trial$title))
corpusTitle = tm_map(corpusTitle,tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle,removePunctuation)
corpusTitle = tm_map(corpusTitle,removeWords,stopwords("english"))
corpusTitle = tm_map(corpusTitle,stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle,0.95)
dtmTitle

dtmTitle = as.data.frame(as.matrix(dtmTitle))
corpusAbstract = Corpus(VectorSource(trial$abstract))
corpusAbstract = tm_map(corpusAbstract,tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract,removePunctuation)
corpusAbstract = tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusAbstract = tm_map(corpusAbstract,stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract,0.95)
dtmAbstract

dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
colSums(dtmAbstract)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trial$trial
str(dtm)

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial,SplitRatio = 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
library(rpart)
library(rpart.plot)

trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)
predTrain = predict(trialCART)[,2]
summary(predTrain)
pr = predict(trialCART,newdata=test)[,2]
summary(pr)
table(train$trial,predTrain>0.5)
(441+631)/(441+631+99+131)
441/(441+131)
631/(631+99)

predTest = predict(trialCART,newdata=test)[,2]
table(test$trial,predTest>0.5)
(162+261)/(162+261+52+83)

library(ROCR)
r = prediction(predTest,test$trial)
auc = as.numeric(performance(r, "auc")@y.values)
auc




