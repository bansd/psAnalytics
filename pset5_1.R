wiki = read.csv("wiki.csv",stringsAsFactors = FALSE)
str(wiki)

wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
library(SnowballC)

#corpus for words added
corpus = Corpus(VectorSource(wiki$Added))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
length(stopwords("english"))
dtmAdded = DocumentTermMatrix(corpus)
sparseAdded = removeSparseTerms(dtmAdded,0.997)
sparseAdded
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)

#corpus for words removed
corpus = Corpus(VectorSource(wiki$Removed))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
dtmRemoved = DocumentTermMatrix(corpus)
dtmRemoved
sparseRemoved = removeSparseTerms(dtmRemoved,0.997)
sparseRemoved
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#binding above two frames and finding accuracy
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal,SplitRatio = 0.7)
train = subset(wikiWords,split==TRUE)
test = subset(wikiWords,split==FALSE)

#finding baseline accuracy
table(test$Vandal)
618/(618+545)

library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~.,data = train,method = "class")
predictCART = predict(wikiCART,newdata = test,type = "class")
table(test$Vandal,predictCART)
(12+618)/(12+618+533)
prp(wikiCART)

#Finding advertising patterns using grepl and then calculating accuracy of 
#model

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal~.,data = wikiTrain2,method = "class")
predictCART2 = predict(wikiCART2,newdata = wikiTest2,type = "class")
table(wikiTest2$Vandal,predictCART2)
(57+609)/(57+609+9+488)

#Calculating sum of rows of words added and removed

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal~.,data = wikiTrain2,method = "class")
predictCART2 = predict(wikiCART2,newdata = wikiTest2,type = "class")
table(wikiTest2$Vandal,predictCART2)
(248+514)/(514+104+297+248)

#Calculating accuracy of model with 2 independent var minor and loggedin
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
wikiCART3 = rpart(Vandal~.,data = wikiTrain3,method = "class")
predictCART3 = predict(wikiCART3,newdata = wikiTest3,type = "class")
table(wikiTest3$Vandal,predictCART3)
(595+241)/(23+595+304+241)
prp(wikiCART3)







