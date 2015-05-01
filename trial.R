NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

str(NewsTrain)

library(tm)
library(SnowballC)

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.995)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

CorpusSnippet = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusSnippet = tm_map(CorpusSnippet , tolower)
CorpusSnippet = tm_map(CorpusSnippet , PlainTextDocument)
CorpusSnippet = tm_map(CorpusSnippet , removePunctuation)
CorpusSnippet = tm_map(CorpusSnippet , removeWords, stopwords("english"))
CorpusSnippet = tm_map(CorpusSnippet , stemDocument)
dtm = DocumentTermMatrix(CorpusSnippet)
sparse = removeSparseTerms(dtm, 0.99)
SnippetWords = as.data.frame(as.matrix(sparse))
colnames(SnippetWords) = make.names(colnames(SnippetWords))
SnippetWordsTrain = head(SnippetWords, nrow(NewsTrain))
SnippetWordsTest= tail(SnippetWords, nrow(NewsTest))

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Snippet, NewsTest$Snippet)))
CorpusAbstract = tm_map(CorpusAbstract , tolower)
CorpusAbstract = tm_map(CorpusAbstract , PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract , removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract , removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract , stemDocument)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.995)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))

AbstractWordsTrain = as.data.frame(as.matrix(AbstractWordsTrain))
AbstractWordsTest = as.data.frame(as.matrix(AbstractWordsTest))
SnippetWordsTrain = as.data.frame(as.matrix(SnippetWordsTrain))
SnippetWordsTest= as.data.frame(as.matrix(SnippetWordsTest))
HeadlineWordsTrain = as.data.frame(as.matrix(HeadlineWordsTrain))
HeadlineWordsTest = as.data.frame(as.matrix(HeadlineWordsTest))

colnames(HeadlineWordsTrain) = paste0("HeadLine", colnames(HeadlineWordsTrain))
colnames(HeadlineWordsTest) = paste0("HeadLine", colnames(HeadlineWordsTest))
colnames(SnippetWordsTest) = paste0("Snippet", colnames(SnippetWordsTest))
colnames(SnippetWordsTrain) = paste0("Snippet", colnames(SnippetWordsTrain))
colnames(AbstractWordsTest) = paste0("Abstract", colnames(AbstractWordsTest))
colnames(AbstractWordsTrain) = paste0("Abstract", colnames(AbstractWordsTrain))

wordsTrain = cbind(HeadlineWordsTrain,AbstractWordsTrain)
wordsTest = cbind(HeadlineWordsTest,AbstractWordsTest)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday
NewsTrain$Hour = NewsTrain$PubDate$hour
NewsTest$Hour = NewsTest$PubDate$hour

NewsDeskFactor = as.factor(c(NewsTrain$NewsDesk, NewsTest$NewsDesk))
NewsTrain$NewsDesk = NewsDeskFactor[1:(nrow(NewsTrain))]
NewsTest$NewsDesk = NewsDeskFactor[(nrow(NewsTrain)+1):length(NewsDeskFactor)]

SectionNameFactor = as.factor(c(NewsTrain$SectionName, NewsTest$SectionName))
NewsTrain$SectionName = SectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SectionName = SectionNameFactor[(nrow(NewsTrain)+1):length(SectionNameFactor)]

SubsectionNameFactor = as.factor(c(NewsTrain$SubsectionName, NewsTest$SubsectionName))
NewsTrain$SubsectionName = SubsectionNameFactor[1:(nrow(NewsTrain))]
NewsTest$SubsectionName = SubsectionNameFactor[(nrow(NewsTrain)+1):length(SubsectionNameFactor)]

wordsTrain$Popular = as.factor(NewsTrain$Popular)
wordsTrain$WordCount = NewsTrain$WordCount
wordsTrain$Weekday = NewsTrain$Weekday
wordsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
wordsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
wordsTrain$SectionName= as.factor(NewsTrain$SectionName)
wordsTrain$Hour = NewsTrain$Hour

wordsTest$WordCount = NewsTest$WordCount
wordsTest$Weekday = NewsTest$Weekday
wordsTest$SubsectionName = as.factor(NewsTest$SubsectionName)
wordsTest$NewsDesk = as.factor(NewsTest$NewsDesk)
wordsTest$SectionName= as.factor(NewsTest$SectionName)
wordsTest$Hour = NewsTest$Hour

predictRF = predict(trainRF,type="prob",newdata=wordsTest)[,2]
mysub = data.frame(UniqueID = NewsTest$UniqueID , Probability1 = PredTest)
write.csv(mysub,"RFsubm2.csv",row.names=FALSE)

trainRF = randomForest(Popular~.,data=wordsTrain)
table(wordsTrain$Popular,predictRF>0.5)
predictRF = predict(trainRF,type="prob")[,2]
table(wordsTrain$Popular,predictRF>0.5)

NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)
NewsTrain$NewsDesk = as.factor(NewsTrain$NewsDesk)
NewsTrain$SubsectionName = as.factor(NewsTrain$SubsectionName)

library(randomForest)
library(caTools)
set.seed(123)
split = sample.split(wordsTrain$Popular, SplitRatio = 0.7)
train = subset(wordsTrain, split==TRUE)
test = subset(wordsTrain,split==FALSE)

trainRF = randomForest(Popular~.,data=train)






test = AbstractWordsTest
test$Popular = NewsTest$Popular
test$WordCount = NewsTest$WordCount
test$Weekday = NewsTest$Weekday

HeadlineWordsTrain$Popular = NewsTrain$Popular
HeadlineWordsTrain$WordCount = NewsTrain$WordCount
HeadlineWordsTest$WordCount = NewsTest$WordCount


NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday



HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)
PredTest = predict(HeadlineWordsLog, newdata=HeadlineWordsTest, type="response")
PredTest = predict(HeadlineWordsLog, type="response")
table(NewsTrain$Popular,PredTest>0.5)
(79+5326)/(113+5326+1014+79)
#[1] 0.8274648

HeadlineWordsTest$Weekday = NewsTest$Weekday
HeadlineWordsTrain$Weekday = NewsTrain$Weekday
HeadlineWordsLog = glm(Popular ~ ., data=HeadlineWordsTrain, family=binomial)
PredTest = predict(HeadlineWordsLog, type="response")
table(NewsTrain$Popular,PredTest>0.5)
(5328+78)/(5328+111+1015+78)
0.8276179

