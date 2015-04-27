library(tm)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, stopwords("english"))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))
frequencies
ncol(allTweets)

##install.packages("wordcloud")

library("wordcloud")
wordcloud(colnames(allTweets),colSums(allTweets))

wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25))
##Most frequent word was apple

##Now removing apple
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, tolower)

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25),random.order=FALSE)
colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25),colors)



