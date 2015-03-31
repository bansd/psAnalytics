songs = read.csv("songs.csv")

MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)
#or
nrow(MichaelJackson)

MichaelJackson[c(“songtitle”, “Top10”)]

#or alternatively we can use
match("Beat It",songs$songtitle)
songs$Top10[6216]

table(songs$timesignature)

#finding highest tempo
which.max(songs$tempo)
songs$songtitle[6206]

SongsTrain=subset(songs,year<=2009)
SongsTest=subset(songs,year==2010)

str(SongsTrain)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$tempo,SongsTrain$tempo_confidence)
cor(SongsTrain$loudness,SongsTrain$energy)

#Making model2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predictTest = predict(SongsLog3, type="response", newdata=SongsTest)

table(SongsTest$Top10, predictTest > 0.45)

#Accuracy
(309+19)/(309+5+40+19)

#Baseline Accuracy
(309+5)/(309+5+40+19)

#Sensivity
19/(19+40)

#specificity
309/(309+5)

#checking how many songs model predicted correctly
table(SongsTest$Top10)
















